use std::collections::HashMap;


#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Int,
    Bool,
    String,
    Tuple(Box<Type>, Box<Type>),
    Function { params: Vec<Type>, ret: Box<Type> },
    Variable(String),
}

impl ToString for Type {
    fn to_string(&self) -> String {
        match self {
            Type::Int => "int".to_string(),
            Type::Bool => "bool".to_string(),
            Type::String => "str".to_string(),
            Type::Tuple(left, right) => format!("({}, {})", left.to_string(), right.to_string()),
            Type::Function { params, ret } => format!(
                "fn({}) -> {}",
                params
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                ret.to_string()
            ),
            Type::Variable(ty_var) => ty_var.to_string(),
        }
    }
}

type Substitution = HashMap<String, Type>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnificationError {
    InfiniteType,
    TypeMismatch,
    ArgCountMismatch,
}

impl Type {
    pub fn contains(&self, other: &str) -> bool {
        match self {
            Type::Variable(a) => a == other,
            Type::Int => false,
            Type::Bool => false,
            Type::String => false,
            Type::Tuple(a, b) => a.contains(other) || b.contains(other),
            Type::Function { params, ret } => {
                params.iter().any(|x| x.contains(other)) || ret.contains(other)
            }
        }
    }

    pub fn apply_substitution(&self, subs: &Substitution) -> Type {
        match self {
            Type::Int => Type::Int,
            Type::Bool => Type::Bool,
            Type::String => Type::String,
            Type::Tuple(a, b) => Type::Tuple(
                a.apply_substitution(subs).into(),
                b.apply_substitution(subs).into(),
            ),
            Type::Function { params, ret } => Type::Function {
                params: params.iter().map(|x| x.apply_substitution(subs)).collect(),
                ret: ret.apply_substitution(subs).into(),
            },
            Type::Variable(b) => {
                //try to find b in subs, or just return Type::Variable(b)
                if let Some(t) = subs.get(b) {
                    t.clone()
                } else {
                    Type::Variable(b.clone())
                }
            }
        }
    }

    //https://www.youtube.com/watch?v=JOFGT7Vq-xI
    //There's also another video from the same guy that gives the unification algorithm
    pub fn unify(&self, other: &Type) -> Result<Substitution, UnificationError> {
        match (self, other) {
            (Type::Int, Type::Int) => Ok(Substitution::new()),
            (Type::Bool, Type::Bool) => Ok(Substitution::new()),
            (Type::String, Type::String) => Ok(Substitution::new()),
            (Type::Variable(a), Type::Variable(b)) if a == b => Ok(Substitution::new()),
            (Type::Variable(v), b) if b.contains(v) => Err(UnificationError::InfiniteType),
            (Type::Variable(a), b) => {
                let mut subs = Substitution::new();
                subs.insert(a.to_string(), b.clone());
                Ok(subs)
            }
            (a, b @ Type::Variable(_)) => b.unify(a),
            (
                Type::Function { params, ret },
                Type::Function {
                    params: params2,
                    ret: ret2,
                },
            ) => {
                if params.len() != params2.len() {
                    return Err(UnificationError::ArgCountMismatch);
                }
                let mut subs = Substitution::new();
                for (a, b) in params.iter().zip(params2.iter()) {
                    let a = a.apply_substitution(&subs);
                    let b = b.apply_substitution(&subs);
                    let new_subs = a.unify(&b)?;
                    subs.extend(new_subs);
                }

                {
                    let ret = ret.apply_substitution(&subs);
                    let ret2 = ret2.apply_substitution(&subs);
                    let new_subs = ret.unify(&ret2)?;
                    subs.extend(new_subs);
                }

                Ok(subs)
            }
            (Type::Tuple(a, b), Type::Tuple(a2, b2)) => {
                let mut subs = Substitution::new();
                let (a, a2) = (a.apply_substitution(&subs), a2.apply_substitution(&subs));
                subs.extend(a.unify(&a2)?);
                let (b, b2) = (b.apply_substitution(&subs), b2.apply_substitution(&subs));
                subs.extend(b.unify(&b2)?);

                Ok(subs)
            }
            _ => Err(UnificationError::TypeMismatch),
        }
    }
}

pub struct Typer {
    let_bindings_types: HashMap<String, Type>,
}

pub struct Context {
    //The value is a shadow stack, it gets pushed and popped as the source is being processed.
    //When we find a let binding, we push to the stack. When we enter an if statement, the insides are popped when the scope ends.
    let_bindings: HashMap<String, Vec<Type>>,
}

#[cfg(test)]
pub mod test {

    #[cfg(test)]
    use pretty_assertions::assert_eq;

    use super::{Type, Typer, UnificationError};

    #[test]
    pub fn unify_primitive() -> Result<(), UnificationError> {
        let primitives = [Type::Int, Type::Bool, Type::String];
        for p in primitives {
            let t1 = p.clone();
            let t2 = p.clone();
            let result = t1.unify(&t2)?;
            assert_eq!(result.len(), 0);
        }

        Ok(())
    }

    #[test]
    pub fn same_type_variable() -> Result<(), UnificationError> {
        let t1 = Type::Variable("a".to_string());
        let t2 = Type::Variable("a".to_string());
        let result = t1.unify(&t2)?;
        assert_eq!(result.len(), 0);

        Ok(())
    }

    #[test]
    pub fn type_variable_occurs_check_triggers() {
        let t1 = Type::Variable("a".to_string());
        let t2 = Type::Function { params: vec![], ret: Type::Variable("a".to_string()).into() };
        let result = t1.unify(&t2).expect_err("Expected infinite type error");
        assert_eq!(result, UnificationError::InfiniteType);
    }

    #[test]
    pub fn type_variable_substitution() -> Result<(), UnificationError> {
        let t1 = Type::Variable("a".to_string());
        let t2 = Type::Int;
        let result = t1.unify(&t2)?;
        assert!(result["a"] == Type::Int);
        Ok(())
    }

    #[test]
    pub fn type_variable_substitution_function() -> Result<(), UnificationError> {
        let t1 = Type::Variable("a".to_string());
        let t2 = Type::Function { params: vec![Type::Int, Type::String], ret: Type::String.into() };
        let result = t1.unify(&t2)?;
        assert!(result["a"] == t2);
        Ok(())
    }

    #[test]
    pub fn type_variable_substitution_another_variable() -> Result<(), UnificationError> {
        let t1 = Type::Variable("a".to_string());
        let t2 = Type::Function { params: vec![Type::Int, Type::String], ret: Type::Variable("b".to_string()).into() };
        let result = t1.unify(&t2)?;
        assert!(result["a"] == t2);
        Ok(())
    }

    #[test]
    pub fn unification_different_types_fail() {
        let t1 = Type::Int;
        let t2 = Type::String;
        let result = t1.unify(&t2).expect_err("Expected type mismatch error");
        assert_eq!(result, UnificationError::TypeMismatch);
    }

    #[test]
    pub fn unification_argcount_fail() {
        let t1 =  Type::Function { params: vec![Type::Int], ret: Type::Variable("a".to_string()).into() };
        let t2 = Type::Function { params: vec![], ret: Type::Variable("a".to_string()).into() };
        let result = t1.unify(&t2).expect_err("Expected argument count type error");
        assert_eq!(result, UnificationError::ArgCountMismatch);
    }

    #[test]
    pub fn function_unification()  -> Result<(), UnificationError> {
        let t1 =  Type::Function { params: vec![Type::Int], ret: Type::Variable("a".to_string()).into() };
        let t2 = Type::Function { params: vec![Type::Variable("a".to_string())], ret: Type::Variable("a".to_string()).into() };
        let result = t1.unify(&t2)?;
        assert!(result["a"] == Type::Int);
        Ok(())
    }

    #[test]
    pub fn function_unification_conflicting_requirements()  {
        //should a be int or bool? neither, this should fail
        let t1 =  Type::Function { params: vec![Type::Int], ret: Type::Variable("a".to_string()).into() };
        let t2 = Type::Function { params: vec![Type::Variable("a".to_string())], ret: Type::Bool.into() };
        let result = t1.unify(&t2).expect_err("Expected type mismatch error");
        assert_eq!(result, UnificationError::TypeMismatch);
    }
}
