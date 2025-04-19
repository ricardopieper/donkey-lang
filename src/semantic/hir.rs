use std::collections::BTreeSet;
use std::collections::HashMap;
use std::fmt::Display;
use std::fmt::Write;
use std::ops::Deref;
use std::ops::Index;
use std::ops::IndexMut;
use std::usize;

use crate::ast::lexer::Operator;
use crate::ast::parser::ASTIfStatement;
use crate::ast::parser::AstSpan;
use crate::ast::parser::FunctionDeclaration;
use crate::ast::parser::SpanAST;
use crate::ast::parser::Spanned;
use crate::ast::parser::SpannedOperator;
use crate::ast::parser::StringSpan;
use crate::ast::parser::TypeBoundName;
use crate::ast::parser::{ASTType, Expr, AST};
use crate::commons::float::FloatLiteral;
use crate::interner::InternedString;
use crate::types::type_constructor_db::TypeConstructor;
use crate::types::type_constructor_db::TypeConstructorDatabase;
use crate::types::type_constructor_db::TypeConstructorId;
use crate::types::type_constructor_db::TypeKind;

use super::typer::Substitution;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LiteralHIRExpr {
    Integer(i128),
    Float(FloatLiteral),
    String(InternedString),
    Char(char),
    Boolean(bool),
    None,
}

//This is for types that the user can give, like in function parameters, struct field types, or typed variable declarations.
//This type can be generic and thus not immediatelly inferrable. Thus the user given type must survive until monomorphization.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HIRUserTypeInfo {
    pub user_given_type: Option<HIRType>,
    pub resolved_type: TypeIndex,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionCall {
    pub function: HIRExpr,
    pub args: Vec<HIRExpr>,
    pub type_args: Vec<HIRUserTypeInfo>,
    pub return_type: TypeIndex,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodCall {
    pub object: Box<HIRExpr>,
    pub method_name: InternedString,
    pub args: Vec<HIRExpr>,
    pub return_type: TypeIndex,
}

// Index into a type table, tells what type an expression or variable is.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub struct TypeIndex(usize);

impl TypeIndex {
    pub fn print_name(&self, ty: &TypeTable, type_db: &TypeConstructorDatabase) -> String {
        if self.0 == usize::MAX {
            return "untyped".to_string();
        }
        ty[self].to_string(type_db)
    }

    pub fn is_untyped(&self) -> bool {
        self.0 == usize::MAX
    }

    pub fn poly<'a>(&self, ty: &'a TypeTable) -> PolyType {
        ty[*self].clone()
    }
}

// Index of the HIR node (either an expression or a statement) so that we can store
// data about it somewhere else.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct NodeIndex(usize);

impl NodeIndex {
    pub fn none() -> NodeIndex {
        NodeIndex(usize::MAX)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HIRExpr {
    Literal(LiteralHIRExpr, NodeIndex, TypeIndex),
    Variable(InternedString, NodeIndex, TypeIndex),
    TypeName {
        type_variable: TypeIndex, //the actual contents of the metadata of the TypeData instance
        type_data: TypeIndex,     //always the metadata type TypeData
        location: NodeIndex,
    },
    Cast(Box<HIRExpr>, HIRType, NodeIndex, TypeIndex),
    SelfValue(NodeIndex, TypeIndex),
    BinaryOperation(
        Box<HIRExpr>,
        SpannedOperator,
        Box<HIRExpr>,
        NodeIndex,
        TypeIndex,
    ),
    MethodCall(MethodCall, NodeIndex),
    //func_expr, args:type, return type, type_args, metadata
    FunctionCall(Box<FunctionCall>, NodeIndex),
    //struct name, type args, $(args:type,)*, return type, metadata
    //Only TExpr is actually needed in later stages, type args won't need to be inferred individually.
    StructInstantiate(InternedString, Vec<HIRUserTypeInfo>, NodeIndex, TypeIndex),
    Deref(Box<HIRExpr>, NodeIndex, TypeIndex),
    Ref(Box<HIRExpr>, NodeIndex, TypeIndex),
    UnaryExpression(SpannedOperator, Box<HIRExpr>, NodeIndex, TypeIndex),
    //obj, field, result_type, metadata
    MemberAccess(Box<HIRExpr>, InternedString, NodeIndex, TypeIndex),
    Array(Vec<HIRExpr>, NodeIndex, TypeIndex),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HIRType {
    Simple(InternedString),
    Generic(InternedString, Vec<HIRType>),
}

impl HIRType {
    pub fn bound_generics(&self, generics_in_context: &[TypeParameter]) -> Vec<InternedString> {
        match self {
            HIRType::Simple(s) => {
                if let Some(_) = generics_in_context.iter().find(|x| x.0 == *s) {
                    vec![s.clone()]
                } else {
                    vec![]
                }
            }
            HIRType::Generic(_, generics) => generics
                .iter()
                .flat_map(|x| x.bound_generics(generics_in_context))
                .collect(),
        }
    }

    pub fn make_poly(
        &self,
        type_db: &TypeConstructorDatabase,
        generics_in_context: &[TypeParameter],
        use_skolems: bool,
    ) -> PolyType {
        let bound_generics = self.bound_generics(generics_in_context);

        if bound_generics.is_empty() {
            return PolyType::mono(
                self.make_mono(type_db, generics_in_context, use_skolems)
                    .into(),
            );
        }

        match self {
            HIRType::Simple(s) => {
                let is_generic = bound_generics.iter().find(|x| *x == s).is_some();
                if is_generic {
                    PolyType {
                        quantifiers: generics_in_context.to_vec(),
                        mono: MonoType::variable(s.clone()),
                    }
                } else {
                    let ty = type_db.find_by_name(*s).expect("Could not find type");

                    PolyType {
                        quantifiers: generics_in_context.to_vec(),
                        mono: MonoType::Application(ty.id, vec![]),
                    }
                }
            }
            HIRType::Generic(type_name, type_args) => {
                let ty = type_db
                    .find_by_name(*type_name)
                    .expect("Could not find type");
                let generics: Vec<MonoType> = type_args
                    .iter()
                    .map(|x| x.make_mono(type_db, generics_in_context, use_skolems))
                    .collect();

                PolyType {
                    quantifiers: generics_in_context.to_vec(),
                    mono: MonoType::Application(ty.id, generics),
                }
            }
        }
    }

    pub fn make_mono(
        &self,
        type_db: &TypeConstructorDatabase,
        generics: &[TypeParameter],
        use_skolems: bool,
    ) -> MonoType {
        match self {
            HIRType::Simple(s) => {
                let is_generic = generics.iter().find(|x| x.0 == *s).is_some();
                if is_generic {
                    if use_skolems {
                        MonoType::Skolem(TypeVariable(s.clone()))
                    } else {
                        MonoType::variable(s.clone())
                    }
                } else {
                    match type_db.find_by_name(*s) {
                        Some(t) => MonoType::Application(t.id, vec![]),
                        None => {
                            panic!("Could not find type {s} (simple)")
                        }
                    }
                }
            }
            HIRType::Generic(type_name, type_args) => {
                let Some(ty) = type_db.find_by_name(*type_name) else {
                    panic!("Could not find type {type_name} (generic)")
                };
                let generics: Vec<MonoType> = type_args
                    .iter()
                    .map(|x| x.make_mono(type_db, generics, use_skolems))
                    .collect();
                MonoType::Application(ty.id, generics)
            }
        }
    }
}

pub struct HIRTypeDisplayer<'source> {
    hir_type: &'source HIRType,
}

impl<'source> HIRTypeDisplayer<'source> {
    pub fn new(hir_type: &'source HIRType) -> HIRTypeDisplayer<'source> {
        HIRTypeDisplayer { hir_type }
    }
}

impl<'source> Display for HIRTypeDisplayer<'source> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.hir_type {
            HIRType::Simple(s) => s.write_str(f),
            HIRType::Generic(s, generics) => {
                let comma_sep = generics
                    .iter()
                    .map(|ty| format!("{}", HIRTypeDisplayer::new(ty)))
                    .collect::<Vec<String>>()
                    .join(", ");
                s.write_str(f)?;
                f.write_char('<')?;
                f.write_str(&comma_sep)?;
                f.write_char('>')
            }
        }
    }
}

//we need to be able to represent complex stuff,
//like a function that receives a function, whose parameters are generic
//def func(another_func: Function<List<String>>)

//so we can store TypeIds, but we need it to be accompanied by more data depending on the kind of the type,
//types such as functions and generics need to be "instanced"

impl From<&ASTType> for HIRType {
    fn from(typ: &ASTType) -> Self {
        match typ {
            ASTType::Simple(name) => Self::Simple(name.0),
            ASTType::Generic(name, generics) => {
                let hir_generics = generics.iter().map(Self::from).collect::<Vec<_>>();
                HIRType::Generic(name.0, hir_generics)
            }
        }
    }
}

impl Display for HIRType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            HIRType::Simple(s) => s.write_str(f),
            HIRType::Generic(s, generics) => {
                let comma_sep = generics
                    .iter()
                    .map(|ty| format!("{}", ty))
                    .collect::<Vec<String>>()
                    .join(", ");
                s.write_str(f)?;
                f.write_char('<')?;
                f.write_str(&comma_sep)?;
                f.write_char('>')
            }
        }
    }
}

impl HIRExpr {
    pub fn get_type(&self) -> TypeIndex {
        match self {
            HIRExpr::Literal(.., t)
            | HIRExpr::Cast(.., t)
            | HIRExpr::BinaryOperation(.., t)
            | HIRExpr::StructInstantiate(.., t)
            | HIRExpr::UnaryExpression(.., t)
            | HIRExpr::Deref(.., t)
            | HIRExpr::Ref(.., t)
            | HIRExpr::MemberAccess(.., t)
            | HIRExpr::Array(.., t)
            | HIRExpr::Variable(.., t) => t.clone(),
            HIRExpr::FunctionCall(fcall, _) => fcall.return_type.clone(),
            HIRExpr::MethodCall(mcall, _) => mcall.return_type.clone(),
            HIRExpr::TypeName { type_data, .. } => type_data.clone(),
            HIRExpr::SelfValue(.., t) => t.clone(),
        }
    }

    pub fn get_node_index(&self) -> NodeIndex {
        match self {
            HIRExpr::Literal(_, node_index, _) => *node_index,
            HIRExpr::Variable(_, node_index, _) => *node_index,
            HIRExpr::TypeName { location, .. } => *location,
            HIRExpr::Cast(_, _, node_index, _) => *node_index,
            HIRExpr::SelfValue(node_index, _) => *node_index,
            HIRExpr::BinaryOperation(_, _, _, node_index, _) => *node_index,
            HIRExpr::MethodCall(_, node_index) => *node_index,
            HIRExpr::FunctionCall(_, node_index) => *node_index,
            HIRExpr::StructInstantiate(_, _, node_index, _) => *node_index,
            HIRExpr::Deref(_, node_index, _) => *node_index,
            HIRExpr::Ref(_, node_index, _) => *node_index,
            HIRExpr::UnaryExpression(_, _, node_index, _) => *node_index,
            HIRExpr::MemberAccess(_, _, node_index, _) => *node_index,
            HIRExpr::Array(_, node_index, _) => *node_index,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HIRTypedBoundName {
    pub name: InternedString,
    pub type_data: HIRTypeWithTypeVariable,
}

/*
The HIR expression is similar to the AST, but can have type information on every node.
*/
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HIRRoot {
    DeclareFunction {
        function_name: InternedString,
        type_parameters: Vec<TypeParameter>,
        parameters: Vec<HIRTypedBoundName>,
        body: Vec<HIR>,
        return_type: HIRTypeWithTypeVariable,
        method_of: Option<TypeIndex>,
        is_intrinsic: bool,
        is_external: bool,
        is_varargs: bool,
        type_table: TypeTable,
        has_been_monomorphized: bool,
    },
    StructDeclaration {
        struct_name: InternedString,
        type_parameters: Vec<TypeParameter>,
        fields: Vec<HIRTypedBoundName>,
        type_table: TypeTable,
        has_been_monomorphized: bool,
    },
    ImplDeclaration {
        struct_name: InternedString,
        type_parameters: Vec<TypeParameter>,
        methods: Vec<HIRRoot>,
        has_been_monomorphized: bool,
    },
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HIRTypeWithTypeVariable {
    pub type_variable: TypeIndex,
    pub hir_type: HIRType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HIR {
    Assign {
        path: HIRExpr,
        expression: HIRExpr,
        location: NodeIndex,
    },
    Declare {
        var: InternedString,
        typedef: HIRTypeWithTypeVariable,
        expression: HIRExpr,
        location: NodeIndex,
    },
    SyntheticDeclare {
        var: InternedString,
        typedef: TypeIndex,
        expression: HIRExpr,
        location: NodeIndex,
    },
    FunctionCall(FunctionCall, NodeIndex),
    MethodCall(MethodCall, NodeIndex),
    //condition, true branch, false branch
    //this transforms elifs into else: \n\t if ..
    If(HIRExpr, Vec<HIR>, Vec<HIR>, NodeIndex),
    //condition, body
    While(HIRExpr, Vec<HIR>, NodeIndex),
    Return(HIRExpr, NodeIndex),
    EmptyReturn(NodeIndex),
}

impl HIR {
    pub fn get_node_index(&self) -> NodeIndex {
        match self {
            HIR::Assign { location, .. }
            | HIR::Declare { location, .. }
            | HIR::SyntheticDeclare { location, .. }
            | HIR::FunctionCall(_, location)
            | HIR::MethodCall(_, location)
            | HIR::If(_, _, _, location)
            | HIR::While(_, _, location)
            | HIR::Return(_, location)
            | HIR::EmptyReturn(location) => *location,
        }
    }
}

struct IfTreeNode {
    condition: HIRExpr,
    true_body: Vec<HIR>,
}

//Refers to quantifiers in a PolyType, like in SomeType<T, U> where T and U are quantifiers.
//This is also notated like ∀T, U. SomeType<T, U>
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeParameter(pub InternedString);

//Refers to type variables, i.e. 't0, 't1, etc but they could also refer to type quantifiers
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TypeVariable(pub InternedString);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MonoType {
    Variable(TypeVariable),
    Skolem(TypeVariable),
    Application(TypeConstructorId, Vec<MonoType>),
}

impl MonoType {
    pub fn get_ctor_id(&self) -> TypeConstructorId {
        match self {
            MonoType::Application(id, _) => *id,
            _ => panic!("Expected an application type"),
        }
    }

    pub fn apply_substitution(&self, substitution: &Substitution) -> MonoType {
        match self {
            MonoType::Variable(var) => {
                if let Some(sub) = substitution.get(&var) {
                    return sub.clone();
                }

                return MonoType::Variable(var.clone());
            }
            MonoType::Skolem(var) => {
                if let Some(sub) = substitution.get(&var) {
                    return sub.clone();
                }

                return MonoType::Skolem(var.clone());
            }
            MonoType::Application(constructor, args) => {
                let new_args = args
                    .into_iter()
                    .map(|a| a.apply_substitution(substitution))
                    .collect();

                return MonoType::Application(*constructor, new_args);
            }
        }
    }

    pub fn contains_type_variable(&self, other: TypeVariable) -> bool {
        match self {
            MonoType::Variable(var) => *var == other,
            MonoType::Skolem(var) => *var == other,
            MonoType::Application(_, args) => args.iter().any(|x| x.contains_type_variable(other)),
        }
    }

    pub fn skolem<T>(name: T) -> MonoType
    where
        T: Into<InternedString>,
    {
        MonoType::Skolem(TypeVariable(name.into()))
    }

    pub fn variable<T>(name: T) -> MonoType
    where
        T: Into<InternedString>,
    {
        MonoType::Variable(TypeVariable(name.into()))
    }

    pub fn simple(ty: TypeConstructorId) -> MonoType {
        MonoType::Application(ty, vec![])
    }

    pub fn print_name(&self, type_db: &TypeConstructorDatabase) -> String {
        match self {
            MonoType::Variable(name) => format!("{}", name.0.to_string()),
            MonoType::Skolem(name) => format!("{}", name.0.to_string()),
            MonoType::Application(constructor, args) => {
                let ctor = type_db.find(*constructor);
                if ctor.kind == TypeKind::Function && args.len() > 0 {
                    let mut args = args.clone();
                    let return_type = args
                        .pop()
                        .expect("Function should always have a return type");
                    let args = args
                        .iter()
                        .map(|x| x.print_name(type_db))
                        .collect::<Vec<String>>()
                        .join(", ");
                    let return_type = return_type.print_name(type_db);
                    format!("({}) -> {}", args, return_type)
                } else {
                    let constructor_name = type_db.get_name(*constructor);
                    let args = args
                        .iter()
                        .map(|x| x.print_name(type_db))
                        .collect::<Vec<String>>()
                        .join(", ");

                    if args.len() == 0 {
                        return constructor_name.to_string();
                    } else {
                        format!("{}<{}>", constructor_name, args)
                    }
                }
            }
        }
    }

    pub fn try_get_type_argument(&self, index: usize) -> Option<&MonoType> {
        match self {
            MonoType::Application(_, args) => args.get(index),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PolyType {
    pub quantifiers: Vec<TypeParameter>,
    pub mono: MonoType,
}

impl PolyType {
    pub fn to_string(&self, type_db: &TypeConstructorDatabase) -> String {
        match self {
            PolyType { quantifiers, mono } if quantifiers.len() == 0 => mono.print_name(type_db),

            PolyType { quantifiers, mono } => {
                let vars = quantifiers
                    .iter()
                    .map(|x| x.0.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("Ɐ{}. {}", vars, mono.print_name(type_db))
            }
        }
    }

    pub fn simple(ty: TypeConstructorId) -> PolyType {
        PolyType {
            quantifiers: vec![],
            mono: MonoType::Application(ty, vec![]),
        }
    }

    pub fn mono(ty: MonoType) -> PolyType {
        PolyType {
            quantifiers: vec![],
            mono: ty,
        }
    }

    pub fn poly(args: Vec<TypeParameter>, ty: MonoType) -> PolyType {
        PolyType {
            quantifiers: args,
            mono: ty,
        }
    }

    pub fn expect_mono(&self) -> &MonoType {
        if self.quantifiers.len() == 0 {
            &self.mono
        } else {
            panic!("Expected a monotype, got a polytype")
        }
    }

    pub fn poly_from_constructor(ctor: &TypeConstructor) -> PolyType {
        let type_params = ctor.type_params.clone();
        if ctor.kind == TypeKind::Function {
            let mut args = ctor.function_params.clone();
            let return_type = ctor
                .function_return_type
                .as_ref()
                .expect("Function should always have a return type")
                .clone();
            args.push(return_type);
            return PolyType::poly(type_params, MonoType::Application(ctor.id, args));
        }

        let type_params_as_mono = type_params
            .iter()
            .map(|x| MonoType::Variable(TypeVariable(x.0)))
            .collect();
        return PolyType::poly(
            type_params,
            MonoType::Application(ctor.id, type_params_as_mono),
        );
    }

    pub fn apply_substitution(&self, substitution: &Substitution) -> PolyType {
        if !self.quantifiers.is_empty() {
            return PolyType::poly(
                self.quantifiers.clone(),
                self.mono.apply_substitution(substitution),
            );
        } else {
            return PolyType::mono(self.mono.apply_substitution(substitution));
        }
    }

    pub fn contains_type_variable(&self, other: TypeVariable) -> bool {
        //let is_in_quantifiers = self.quantifiers.iter().any(|q| q.0 == other.0);
        let is_in_quantifiers = false;
        return self.mono.contains_type_variable(other) && !is_in_quantifiers;
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeTable {
    pub table: Vec<PolyType>,
    current_index: TypeIndex,
    ty_var_current: u64,
    param_current: u64,
    frozen_indices: BTreeSet<TypeIndex>,
}

impl TypeTable {
    pub fn new() -> TypeTable {
        TypeTable {
            current_index: TypeIndex(0),
            table: vec![],
            ty_var_current: 0,
            param_current: 0,
            frozen_indices: BTreeSet::new(),
        }
    }

    pub fn next(&mut self) -> TypeIndex {
        let var_name = format!("'t{}", self.ty_var_current);
        let r = self.next_with(PolyType::mono(MonoType::Variable(TypeVariable(
            InternedString::new(&var_name),
        ))));
        self.ty_var_current += 1;
        r
    }

    pub fn next_param_or_return(&mut self) -> TypeIndex {
        let var_name = format!("'param{}", self.param_current);
        let r = self.next_with(PolyType::mono(MonoType::Variable(TypeVariable(
            InternedString::new(&var_name),
        ))));
        self.param_current += 1;
        r
    }

    pub fn next_with(&mut self, poly: PolyType) -> TypeIndex {
        self.table.push(poly);
        let ret = self.current_index;
        self.current_index = TypeIndex(self.current_index.0 + 1);
        return ret;
    }

    pub fn untyped(&self) -> TypeIndex {
        TypeIndex(usize::MAX)
    }

    pub fn apply_substitution(&mut self, substitution: &Substitution) {
        if substitution.len() == 0 {
            return;
        }

        for (_, entry) in self.table.iter_mut().enumerate() {
            match entry.mono {
                MonoType::Variable(tv) => {
                    if entry.quantifiers.len() == 0 {
                        if let Some(sub) = substitution.get(&tv) {
                            entry.mono = sub.clone();
                        }
                    }
                }
                MonoType::Application(_, ref mut args) => {
                    for arg in args {
                        *arg = arg.apply_substitution(substitution);
                    }
                }
                _ => {} //skolems are not substituted.
            }
        }
    }

    //used by monomorphization to do a function-wide substitution. Includes skolems,
    //and includes type variables that are deep inside type applications (the arguments of type applications)
    pub fn apply_function_wide_substitution(&mut self, substitution: &Substitution) {
        if substitution.len() == 0 {
            return;
        }
        for (_, entry) in self.table.iter_mut().enumerate() {
            match entry.mono {
                MonoType::Variable(tv) | MonoType::Skolem(tv) => {
                    if let Some(sub) = substitution.get(&tv) {
                        entry.mono = sub.clone();
                    }
                }
                MonoType::Application(_, ref mut args) => {
                    //perhaps this should also be done in the apply_substitution method
                    //because currently it's not possible to substitute a type variable
                    //deep inside a type application
                    for arg in args {
                        *arg = arg.apply_substitution(substitution);
                    }
                }
            }
        }
    }
}

impl Index<TypeIndex> for TypeTable {
    type Output = PolyType;

    fn index(&self, index: TypeIndex) -> &Self::Output {
        &self.table[index.0]
    }
}

impl Index<&TypeIndex> for TypeTable {
    type Output = PolyType;

    fn index(&self, index: &TypeIndex) -> &Self::Output {
        &self.table[index.0]
    }
}

impl Index<&mut TypeIndex> for TypeTable {
    type Output = PolyType;

    fn index(&self, index: &mut TypeIndex) -> &Self::Output {
        &self.table[index.0]
    }
}

impl IndexMut<TypeIndex> for TypeTable {
    fn index_mut(&mut self, index: TypeIndex) -> &mut Self::Output {
        &mut self.table[index.0]
    }
}

impl IndexMut<&TypeIndex> for TypeTable {
    fn index_mut(&mut self, index: &TypeIndex) -> &mut Self::Output {
        &mut self.table[index.0]
    }
}

impl IndexMut<&mut TypeIndex> for TypeTable {
    fn index_mut(&mut self, index: &mut TypeIndex) -> &mut Self::Output {
        &mut self.table[index.0]
    }
}

#[derive(Debug)]
pub struct MetadataItem {
    pub ast_span: AstSpan,
}

#[derive(Debug)]
pub struct MetaTable {
    pub table: Vec<MetadataItem>,
}

impl MetaTable {
    pub fn new() -> MetaTable {
        MetaTable { table: vec![] }
    }

    pub fn next(&mut self, span: &impl Spanned) -> NodeIndex {
        let ret = NodeIndex(self.table.len());
        self.table.push(MetadataItem {
            ast_span: span.get_span(),
        });
        return ret;
    }
}

impl Index<NodeIndex> for MetaTable {
    type Output = MetadataItem;

    fn index(&self, index: NodeIndex) -> &Self::Output {
        &self.table[index.0]
    }
}

impl Index<&NodeIndex> for MetaTable {
    type Output = MetadataItem;

    fn index(&self, index: &NodeIndex) -> &Self::Output {
        &self.table[index.0]
    }
}

impl IndexMut<NodeIndex> for MetaTable {
    fn index_mut(&mut self, index: NodeIndex) -> &mut Self::Output {
        &mut self.table[index.0]
    }
}

impl IndexMut<&NodeIndex> for MetaTable {
    fn index_mut(&mut self, index: &NodeIndex) -> &mut Self::Output {
        &mut self.table[index.0]
    }
}

fn expr_to_hir<'source>(
    type_db: &TypeConstructorDatabase,
    expr: &'source Expr,
    _is_in_assignment_lhs: bool,
    ty: &mut TypeTable,
    meta: &mut MetaTable,
    decls: &HashMap<InternedString, TypeIndex>,
    generics_in_context: &[TypeParameter],
) -> HIRExpr {
    match expr {
        Expr::IntegerValue(i, span) => HIRExpr::Literal(
            LiteralHIRExpr::Integer(*i),
            meta.next(span),
            ty.next_with(PolyType::simple(type_db.common_types.i32)),
        ),
        Expr::FloatValue(f, span) => HIRExpr::Literal(
            LiteralHIRExpr::Float(*f),
            meta.next(span),
            ty.next_with(PolyType::simple(type_db.common_types.f32)),
        ),
        Expr::StringValue(s) => HIRExpr::Literal(
            LiteralHIRExpr::String(s.0),
            meta.next(s),
            ty.next_with(load_stdlib_builtin(type_db, "str").expect("str type not found")),
        ),
        Expr::CharValue(c, span) => HIRExpr::Literal(
            LiteralHIRExpr::Char(*c),
            meta.next(span),
            ty.next_with(PolyType::simple(type_db.common_types.char)),
        ),
        Expr::BooleanValue(b, span) => HIRExpr::Literal(
            LiteralHIRExpr::Boolean(*b),
            meta.next(span),
            ty.next_with(PolyType::simple(type_db.common_types.bool)),
        ),

        Expr::NoneValue(span) => HIRExpr::Literal(LiteralHIRExpr::None, meta.next(span), ty.next()),
        Expr::Variable(name) => {
            if let Some(t) = decls.get(&name.0) {
                HIRExpr::Variable(name.0, meta.next(name), t.clone())
            } else {
                //Since the HIR processor processes the AST in a top-down manner
                //and decls is only scoped to the current function, we can't find
                //function names in the decls right now. Assume it exists
                //and try to resolve later.
                //The Typer will have to check if the variable actually exists.
                HIRExpr::Variable(name.0, meta.next(name), ty.next())
            }
        }
        Expr::FunctionCall(fun_expr, type_args, args, span) => match fun_expr.as_ref() {
            var @ Expr::Variable(_) => {
                println!("Type args in fcall: {:?}", type_args);
                let fcall = FunctionCall {
                    function: expr_to_hir(
                        type_db,
                        var,
                        false,
                        ty,
                        meta,
                        decls,
                        generics_in_context,
                    )
                    .into(),
                    type_args: type_args
                        .iter()
                        .map(|x| HIRUserTypeInfo {
                            user_given_type: Some(x.into()),
                            resolved_type: {
                                let hir: HIRType = x.into();
                                ty.next_with(hir.make_poly(type_db, generics_in_context, false))
                            },
                        })
                        .collect(),
                    args: args
                        .iter()
                        .map(|x| {
                            expr_to_hir(
                                type_db,
                                &x.expr,
                                false,
                                ty,
                                meta,
                                decls,
                                generics_in_context,
                            )
                        })
                        .collect(),
                    return_type: ty.next(),
                };
                HIRExpr::FunctionCall(fcall.into(), meta.next(span))
            }
            Expr::MemberAccess(obj, var_name) => HIRExpr::MethodCall(
                MethodCall {
                    object: expr_to_hir(type_db, obj, false, ty, meta, decls, generics_in_context)
                        .into(),
                    method_name: var_name.0,
                    args: args
                        .iter()
                        .map(|arg| {
                            expr_to_hir(
                                type_db,
                                &arg.expr,
                                false,
                                ty,
                                meta,
                                decls,
                                generics_in_context,
                            )
                        })
                        .collect(),
                    return_type: ty.next(),
                },
                meta.next(span),
            ),
            _ => panic!("Cannot lower function call to HIR: not variable or member access"),
        },
        Expr::IndexAccess(object, index, span) => {
            //this makes all array[index] operations take the form of:
            //*(array.__index_ptr__(index))

            let idx = InternedString::new("__index_ptr__");

            let method_call = MethodCall {
                object: Box::new(expr_to_hir(
                    type_db,
                    object,
                    false,
                    ty,
                    meta,
                    decls,
                    generics_in_context,
                )),
                method_name: idx,
                args: vec![expr_to_hir(
                    type_db,
                    index,
                    false,
                    ty,
                    meta,
                    decls,
                    generics_in_context,
                )],
                return_type: ty.next(),
            };
            let next = meta.next(span);
            HIRExpr::Deref(
                HIRExpr::MethodCall(method_call, next).into(),
                next,
                ty.next(),
            )
        }
        Expr::BinaryOperation(lhs, op, rhs) => {
            let lhs = expr_to_hir(type_db, lhs, false, ty, meta, decls, generics_in_context);
            let rhs = expr_to_hir(type_db, rhs, false, ty, meta, decls, generics_in_context);
            HIRExpr::BinaryOperation(lhs.into(), *op, rhs.into(), meta.next(op), ty.next())
        }
        Expr::Parenthesized(_) => panic!("parenthesized not expected"),
        Expr::UnaryExpression(op, rhs) => {
            let rhs = expr_to_hir(type_db, rhs, false, ty, meta, decls, generics_in_context);

            match op.0 {
                Operator::Multiply => HIRExpr::Deref(rhs.into(), meta.next(op), ty.next()),
                Operator::Ampersand => HIRExpr::Ref(rhs.into(), meta.next(op), ty.next()),
                _ => HIRExpr::UnaryExpression(*op, rhs.into(), meta.next(op), ty.next()),
            }
        }
        Expr::MemberAccess(object, member) => {
            let object = expr_to_hir(type_db, object, false, ty, meta, decls, generics_in_context);
            HIRExpr::MemberAccess(object.into(), member.0, meta.next(member), ty.next())
        }
        Expr::Array(items, span) => {
            let items = items
                .iter()
                .map(|item| expr_to_hir(type_db, item, false, ty, meta, decls, generics_in_context))
                .collect();
            HIRExpr::Array(items, meta.next(span), ty.next())
        }
        Expr::Cast(casted_expr, ast_ty, span) => {
            let typ: HIRType = ast_ty.into();
            let casted_expr = Box::new(expr_to_hir(
                type_db,
                casted_expr,
                false,
                ty,
                meta,
                decls,
                generics_in_context,
            ));
            HIRExpr::Cast(casted_expr, typ, meta.next(span), ty.next())
        }
        Expr::SelfValue(span) => HIRExpr::SelfValue(meta.next(span), ty.next()),
    }
}

fn ast_to_hir<'source>(
    ast: &'source SpanAST,
    accum: &mut Vec<HIR>,
    ty: &mut TypeTable,
    meta: &mut MetaTable,
    type_db: &TypeConstructorDatabase,
    decls: &mut HashMap<InternedString, TypeIndex>,
    generics: &[TypeParameter],
) {
    let ast = &ast.ast;
    match ast {
        AST::Declare { var, expression } => {
            //expr: we have to decompose the expression into HIR declarations
            //because the assigned/declared expression has to be sufficiently simple
            //to decrease complexity for the other compiler phases (typechecking, type inference)

            //maybe a way to do it is by calling reduce_expr_to_hir_declarations, and the function
            //itself returns a HIRExpr. It will also add to the HIR any declarations needed
            //for the decomposition.
            let result_expr =
                expr_to_hir(type_db, &expression.expr, false, ty, meta, decls, generics);

            let typedef: HIRType = (&var.name_type).into();
            let poly = typedef.make_poly(type_db, generics, false);
            let ty_var = ty.next_with(poly);
            decls.insert(var.name.0, ty_var);
            let decl_hir = HIR::Declare {
                var: var.name.0,
                typedef: HIRTypeWithTypeVariable {
                    type_variable: ty_var,
                    hir_type: typedef,
                },
                expression: result_expr,
                location: meta.next(&expression.span),
            };

            accum.push(decl_hir);
        }
        AST::Assign { path, expression } => {
            let path_expr = expr_to_hir(type_db, &path.expr, true, ty, meta, decls, generics);
            let result_expr =
                expr_to_hir(type_db, &expression.expr, false, ty, meta, decls, generics);
            if let HIRExpr::Variable(name, meta, _) = &path_expr {
                if let Some(_) = decls.get(name) {
                    let decl_hir = HIR::Assign {
                        path: path_expr.clone(),
                        expression: result_expr,
                        location: *meta,
                    };
                    accum.push(decl_hir);
                } else {
                    let ty_var = result_expr.get_type();
                    decls.insert(name.clone(), ty_var);
                    let decl_hir = HIR::SyntheticDeclare {
                        var: *name,
                        typedef: ty_var,
                        expression: result_expr,
                        location: *meta,
                    };
                    accum.push(decl_hir);
                }
            } else {
                let decl_hir = HIR::Assign {
                    path: path_expr.clone(),
                    expression: result_expr,
                    location: path_expr.get_node_index(),
                };
                accum.push(decl_hir);
            }
        }
        AST::Return(ast, expr) => match expr {
            None => {
                accum.push(HIR::EmptyReturn(meta.next(ast)));
            }
            Some(e) => {
                let result_expr = expr_to_hir(type_db, &e.expr, false, ty, meta, decls, generics);
                accum.push(HIR::Return(result_expr, meta.next(ast)));
            }
        },
        AST::StandaloneExpr(expr) => {
            let Expr::FunctionCall(..) = expr.deref() else {
                panic!("Can only lower function call standalone expr: {expr:#?}");
            };

            let result_expr: HIRExpr = expr_to_hir(type_db, expr, false, ty, meta, decls, generics);

            match result_expr {
                HIRExpr::MethodCall(mcall, meta) => {
                    let mcall = MethodCall {
                        object: mcall.object,
                        method_name: mcall.method_name,
                        args: mcall.args,
                        return_type: ty.next(),
                    };
                    accum.push(HIR::MethodCall(mcall.into(), meta));
                }
                HIRExpr::FunctionCall(mut fcall, meta) => {
                    accum.push(HIR::FunctionCall(*fcall, meta));
                }
                _ => {
                    panic!("Lowering of function call returned invalid result: {result_expr:?}");
                }
            }
        }
        AST::IfStatement {
            true_branch,
            elifs,
            final_else,
        } => {
            ast_if_to_hir(
                type_db,
                true_branch,
                accum,
                elifs,
                final_else,
                ast,
                ty,
                meta,
                decls,
                generics,
            );
        }
        AST::WhileStatement { expression, body } => {
            ast_while_to_hir(
                type_db,
                &expression.expr,
                accum,
                body,
                ast,
                ty,
                meta,
                decls,
                generics,
            );
        }
        AST::Intrinsic(_) => panic!("Cannot use intrinsic keyword"),

        ast => todo!("Not implemented HIR for {ast:?}"),
    }
}

fn ast_decl_function_to_hir<'source>(
    type_db: &TypeConstructorDatabase,
    body: &'source [SpanAST],
    function_name: InternedString,
    type_parameters: &'source [StringSpan],
    parameters: &'source [TypeBoundName],
    return_type: &Option<ASTType>,
    is_varargs: bool,
    ast: &'source AST,
    accum: &mut Vec<HIRRoot>,
    decls: &mut HashMap<InternedString, TypeIndex>,
    meta: &mut MetaTable,
) {
    let mut ty = TypeTable::new();
    let type_parameters: Vec<_> = type_parameters.iter().map(|x| TypeParameter(x.0)).collect();
    let parameters = create_parameters(parameters, type_db, &type_parameters, &mut ty, decls, true);
    let hir_return = match return_type {
        Some(x) => x.into(),
        None => HIRType::Simple(InternedString::new("Void")),
    };

    let return_ty = ty.next_param_or_return(); /*(PolyType::mono(hir_return.make_mono(
                                                   type_db,
                                                   &type_parameters,
                                                   true,
                                               )));*/

    let return_type = HIRTypeWithTypeVariable {
        hir_type: hir_return,
        type_variable: return_ty,
    };

    if body.len() == 1 {
        if let AST::Intrinsic(_) = &body[0].ast {
            accum.push(HIRRoot::DeclareFunction {
                function_name,
                type_parameters: type_parameters.iter().map(|x| TypeParameter(x.0)).collect(),
                parameters,
                body: vec![],
                is_intrinsic: true,
                is_external: false,
                is_varargs,
                return_type,
                method_of: Some(ty.next()),
                type_table: ty,
                has_been_monomorphized: false,
            });
            return;
        }

        if let AST::External(_) = &body[0].ast {
            accum.push(HIRRoot::DeclareFunction {
                function_name,
                type_parameters,
                parameters,
                body: vec![],
                is_intrinsic: false,
                is_external: true,
                is_varargs,
                return_type,
                method_of: None,
                type_table: ty,
                has_been_monomorphized: false,
            });
            return;
        }
    }

    let mut function_body = vec![];
    for node in body {
        ast_to_hir(
            node,
            &mut function_body,
            &mut ty,
            meta,
            type_db,
            decls,
            &type_parameters,
        );
    }

    let decl_hir: HIRRoot = HIRRoot::DeclareFunction {
        function_name,
        parameters,
        type_parameters,
        body: function_body,
        is_intrinsic: false,
        is_external: false,
        is_varargs,
        return_type,
        method_of: None,
        type_table: ty,
        has_been_monomorphized: false,
    };
    accum.push(decl_hir);
}

fn create_parameters(
    parameters: &[TypeBoundName],
    type_db: &TypeConstructorDatabase,
    type_params: &[TypeParameter],
    ty: &mut TypeTable,
    decls: &mut HashMap<InternedString, TypeIndex>,
    use_skolem: bool,
) -> Vec<HIRTypedBoundName> {
    parameters
        .iter()
        .map(|it| create_function_param(it, type_db, type_params, ty, decls, use_skolem))
        .collect()
}

fn create_function_param(
    param: &TypeBoundName,
    type_db: &TypeConstructorDatabase,
    type_params: &[TypeParameter], //not args, params
    ty: &mut TypeTable,
    decls: &mut HashMap<InternedString, TypeIndex>,
    use_skolem: bool,
) -> HIRTypedBoundName {
    let hir_type: HIRType = (&param.name_type).into();
    let ty_var = ty.next_param_or_return();
    decls.insert(param.name.0, ty_var);
    HIRTypedBoundName {
        name: param.name.0,
        type_data: HIRTypeWithTypeVariable {
            hir_type,
            type_variable: ty_var,
        },
    }
}

fn create_struct_field_type(
    param: &TypeBoundName,
    type_db: &TypeConstructorDatabase,
    type_params: &[TypeParameter], //not args, params
    ty: &mut TypeTable,
) -> HIRTypedBoundName {
    let hir_type: HIRType = (&param.name_type).into();
    let ty_var = hir_type.make_poly(type_db, type_params, true);
    let next = ty.next_with(ty_var);
    HIRTypedBoundName {
        name: param.name.0,
        type_data: HIRTypeWithTypeVariable {
            hir_type,
            type_variable: next,
        },
    }
}
fn ast_if_to_hir<'source>(
    type_db: &TypeConstructorDatabase,
    true_branch: &'source ASTIfStatement,
    accum: &mut Vec<HIR>,
    elifs: &'source [ASTIfStatement],
    final_else: &'source Option<Vec<SpanAST>>,
    ast: &'source AST,
    ty: &mut TypeTable,
    meta: &mut MetaTable,
    decls: &mut HashMap<InternedString, TypeIndex>,
    generics: &[TypeParameter],
) {
    let true_branch_result_expr = expr_to_hir(
        type_db,
        &true_branch.expression.expr,
        false,
        ty,
        meta,
        decls,
        generics,
    );

    let mut true_body_hir = vec![];
    let mut true_branch_scope = decls.clone();
    for node in &true_branch.statements {
        ast_to_hir(
            node,
            &mut true_body_hir,
            ty,
            meta,
            type_db,
            &mut true_branch_scope,
            generics,
        );
    }
    /*
      The HIR representation is simpler, but that means we have to do work to simplify it.
      HIR is only condition, true branch, false branch so the rest of the else ifs go inside the false branch.

      Let's just rewrite whatever the user wrote lmao

      First we have to handle some base cases, i.e. no more elifs ou elses, no more elifs but there is an else, etc.
    */
    if elifs.is_empty() && final_else.is_none() {
        //Just like function declarations, the intermediaries created here don't escape the context.
        //This is different than python, where all variables declared are scoped to the entire function;
        accum.push(HIR::If(
            true_branch_result_expr,
            true_body_hir,
            vec![],
            meta.next(&true_branch.expression.span),
        ));
    } else if elifs.is_empty() && final_else.is_some() {
        //in this case we have a final else, just generate a false branch
        let mut false_body_hir = vec![];

        match final_else {
            None => unreachable!("Shouldn't happen!"),
            Some(nodes) => {
                let mut false_branch_scope = decls.clone();

                for node in nodes.iter() {
                    ast_to_hir(
                        node,
                        &mut false_body_hir,
                        ty,
                        meta,
                        type_db,
                        &mut false_branch_scope,
                        generics,
                    );
                }

                accum.push(HIR::If(
                    true_branch_result_expr,
                    true_body_hir,
                    false_body_hir,
                    meta.next(&true_branch.expression.span),
                ));
            }
        }
    } else {
        //in this case we have elifs, so we build the "tree"
        //and we don't actually need to store the false body because we'll connect everything later.
        //it's not actually a tree... it's more like a linked list.

        //let mut current_if_tree = HIR::If(literal_true_branch_expr, true_body_hir, ());
        let mut nodes = vec![];

        let root_node = IfTreeNode {
            condition: true_branch_result_expr,
            true_body: true_body_hir,
        };

        nodes.push(root_node);

        for item in elifs {
            let elif_true_branch_result_expr = expr_to_hir(
                type_db,
                &item.expression.expr,
                false,
                ty,
                meta,
                decls,
                generics,
            );

            let mut if_node = IfTreeNode {
                condition: elif_true_branch_result_expr,
                true_body: vec![],
            };
            let mut elif_scope = decls.clone();
            for node in &item.statements {
                ast_to_hir(
                    node,
                    &mut if_node.true_body,
                    ty,
                    meta,
                    type_db,
                    &mut elif_scope,
                    generics,
                );
            }
            nodes.push(if_node);
        }
        let mut final_else_body = vec![];
        let mut final_else_scope = decls.clone();
        if let Some(statements) = final_else {
            for node in statements.iter() {
                ast_to_hir(
                    node,
                    &mut final_else_body,
                    ty,
                    meta,
                    type_db,
                    &mut final_else_scope,
                    generics,
                );
            }
        }

        let mut final_if_chain = None;

        if !final_else_body.is_empty() {
            let first_node = nodes.pop().unwrap(); //there MUST be a node here
            let node_idx = first_node.condition.get_node_index();
            final_if_chain = Some(HIR::If(
                first_node.condition,
                first_node.true_body,
                final_else_body,
                node_idx,
            ));
        }

        nodes.reverse();
        //we navigate through the nodes in reverse and build the final HIR tree
        for node in nodes {
            let node_idx = node.condition.get_node_index();
            let new_node = match final_if_chain {
                None => HIR::If(node.condition, node.true_body, vec![], node_idx),
                Some(current_chain) => HIR::If(
                    node.condition,
                    node.true_body,
                    vec![current_chain],
                    node_idx,
                ),
            };
            final_if_chain = Some(new_node);
        }
        accum.push(final_if_chain.unwrap());
    }
}

fn ast_while_to_hir<'source>(
    type_db: &TypeConstructorDatabase,
    expression: &'source Expr,
    accum: &mut Vec<HIR>,
    body: &'source [SpanAST],
    ast: &'source AST,
    ty: &mut TypeTable,
    meta: &mut MetaTable,
    decls: &mut HashMap<InternedString, TypeIndex>,
    generics: &[TypeParameter],
) {
    let expr = expr_to_hir(type_db, expression, false, ty, meta, decls, generics);

    let mut true_body_hir = vec![];
    let mut while_scope = decls.clone();
    for node in body {
        ast_to_hir(
            node,
            &mut true_body_hir,
            ty,
            meta,
            type_db,
            &mut while_scope,
            generics,
        );
    }

    accum.push(HIR::While(expr, true_body_hir, meta.next(expression)))
}

fn ast_impl_to_hir<'source>(
    type_db: &TypeConstructorDatabase,
    body: &'source [FunctionDeclaration],
    struct_name: InternedString,
    type_parameters: &'source [StringSpan],
    ast: &'source AST,
    accum: &mut Vec<HIRRoot>,
    meta: &mut MetaTable,
) {
    let mut functions = vec![];
    let mut decls = HashMap::<InternedString, TypeIndex>::new();
    for node in body {
        ast_decl_function_to_hir(
            type_db,
            &node.body,
            node.function_name.0,
            type_parameters,
            &node.parameters,
            &node.return_type,
            node.is_varargs,
            ast,
            &mut functions,
            &mut decls,
            meta,
        );
    }

    accum.push(HIRRoot::ImplDeclaration {
        struct_name,
        type_parameters: type_parameters.iter().map(|x| TypeParameter(x.0)).collect(),
        methods: functions,
        has_been_monomorphized: false,
    });
}

fn try_literal_promotion(
    type_db: &TypeConstructorDatabase,
    literal: &LiteralHIRExpr,
    type_hint: &TypeConstructorId,
    meta: &crate::ast::parser::Expr,
) -> Result<(), String> {
    let type_constructor_id = *type_hint;
    match literal {
        LiteralHIRExpr::Integer(i) => {
            //we need to check for promotions and the attempted promoted type
            //and see if it's in range, i.e. we can't promote 23752432 to an u8

            macro_rules! check_promotion {
                ($type:ty, $type_id:expr) => {
                    if type_constructor_id == $type_id {
                        if *i >= <$type>::MIN as i128 && *i <= <$type>::MAX as i128 {
                            return Ok(());
                        }
                        else {
                            return Err("Emit out of bounds error".into())
                            /*return self.ctx.errors.out_of_bounds_constructor.push_inference_error(report!(self, meta, OutOfTypeBoundsTypeConstructor {
                                expr: meta, //@TODO unecessary, at_spanned already contains the metadata
                                typ: TypeConstructParams::simple($type_id),
                            }))?;*/
                        }
                    }
                };
            }
            macro_rules! check_promotions {
                ($($type:ty: $type_id:expr),*) => {
                    $(
                        check_promotion!($type, $type_id);
                    )*
                };
            }
            check_promotions!(
                u8: type_db.common_types.u8,
                u32: type_db.common_types.u32,
                u64: type_db.common_types.u64,
                i32: type_db.common_types.i32,
                i64: type_db.common_types.i64
            );

            Err("Emit type promotion failure error".into())

            // return self
            //     .ctx
            //     .errors
            //     .type_promotion_failure
            //     .push_inference_error(report!(
            //         self,
            //         meta,
            //         TypePromotionFailure {
            //             target_type: TypeConstructParams::simple(type_constructor_id)
            //         }
            //     ))?;
        }
        _ => panic!("Cannot promote value: {:#?}", literal),
    }
}

fn load_stdlib_builtin(type_db: &TypeConstructorDatabase, name: &str) -> Option<PolyType> {
    //check if instance already exists
    let type_data_type = type_db.find_by_name(InternedString::new(name));

    match type_data_type {
        Some(d) => return Some(PolyType::simple(d.id)),
        None => {
            //TODO: Fix: navigate through all types and roots before running inference,
            //otherwise we can't use types in stdlib
            log!("Type {} not found in stdlib", name);
            None
        }
    }
}

pub fn ast_globals_to_hir<'source>(
    ast: &'source AST,
    type_db: &TypeConstructorDatabase,
    meta: &mut MetaTable,
) -> Vec<HIRRoot> {
    let mut accum = vec![];
    match ast {
        AST::DeclareFunction(FunctionDeclaration {
            function_name,
            parameters,
            body,
            type_parameters,
            return_type,
            is_varargs,
            ..
        }) => {
            let mut decls = HashMap::<InternedString, TypeIndex>::new();
            ast_decl_function_to_hir(
                type_db,
                body,
                function_name.0,
                type_parameters,
                parameters,
                return_type,
                *is_varargs,
                ast,
                &mut accum,
                &mut decls,
                meta,
            );
        }
        AST::Root(ast_nodes) => {
            for node in ast_nodes {
                accum.extend(ast_globals_to_hir(&node.ast, type_db, meta));
            }
        }
        AST::ImplDeclaration {
            struct_name,
            type_parameters,
            body,
        } => {
            ast_impl_to_hir(
                type_db,
                body,
                struct_name.0,
                type_parameters,
                ast,
                &mut accum,
                meta,
            );
        }
        AST::StructDeclaration {
            struct_name,
            type_parameters,
            body,
        } => {
            let mut ty = TypeTable::new();
            let type_parameters: Vec<_> =
                type_parameters.iter().map(|x| TypeParameter(x.0)).collect();
            let fields: Vec<_> = body
                .iter()
                .map(|field| create_struct_field_type(field, type_db, &type_parameters, &mut ty))
                .collect();

            for field in &fields {
                log!(
                    "Field: {} {}",
                    field.name,
                    &ty[field.type_data.type_variable].to_string(type_db)
                );
            }

            accum.push(HIRRoot::StructDeclaration {
                struct_name: struct_name.0,
                fields,
                type_parameters: type_parameters.iter().map(|x| TypeParameter(x.0)).collect(),
                type_table: ty,
                has_been_monomorphized: false,
            });
        }
        other => panic!("AST not supported: {other:?}"),
    }
    accum
}
