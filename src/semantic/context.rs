use core::panic;
use std::fs;

use super::hir::HIR;

use crate::ast::lexer::{TokenSpanIndex, TokenTable};
use crate::ast::parser::{print_errors, AstSpan, Parser};
use crate::ast::{lexer, parser};
use crate::ast::parser::AST;

#[derive(Eq, PartialEq, Hash, Debug, Copy, Clone)]
pub struct FileTableIndex(pub usize);

#[derive(Debug)]
pub struct FileTableEntry {
    pub path: String,
    pub index: FileTableIndex,
    pub ast: AST,
    pub contents: &'static str,
    pub token_table: TokenTable,
}

#[derive(Debug)]
pub struct Source {
    pub file_table: Vec<FileTableEntry>,
}

impl Source {
    pub fn new() -> Source {
        Source { file_table: vec![] }
    }
}
impl Source {
    #[allow(dead_code)]
    pub fn load_str_ref(self: &mut Source, file_name: &str, source: &str) -> bool {
        self.load(file_name.to_string(), source.to_string())
    }

    pub fn load(self: &mut Source, file_name: String, source: String) -> bool {
        let prev_count = self.file_table.len();
        let mut file_table_entry = FileTableEntry {
            path: file_name,
            index: FileTableIndex(prev_count),
            ast: AST::Break(AstSpan {
                //this gets replaced
                start: TokenSpanIndex {
                    file: FileTableIndex(0),
                    index: 0,
                },
                end: TokenSpanIndex {
                    file: FileTableIndex(0),
                    index: 0,
                },
            }),
            contents: source.leak(),
            token_table: TokenTable {
                tokens: vec![],
                spans: vec![],
            },
        };
        file_table_entry.token_table =
            lexer::tokenize(FileTableIndex(prev_count), file_table_entry.contents).unwrap();

        self.file_table.push(file_table_entry);
        let last = &self.file_table.last().unwrap();
        let mut parser = Parser::new(&last.token_table);
        let result = parser.parse_ast();

        if !parser.errors.is_empty() {
            print_errors(&parser, &self.file_table, &last.token_table);
            return false;
        }

        //println!("{}\n", print_ast(&ast, &self.interner));
        let root = parser::AST::Root(result);
        self.file_table.last_mut().unwrap().ast = root;

        return true;
    }

    pub fn load_file(&mut self, file_location: &str) -> bool {
        //println!("LOADING FILE {file_location}");
        let input = fs::read_to_string(file_location)
            .unwrap_or_else(|_| panic!("Could not read file {file_location}"));
        self.load(file_location.to_string(), input)
    }

    #[allow(dead_code)]
    pub fn load_stdlib(&mut self) -> bool {
        self.load_file("./stdlib/c_lib.dk");
        self.load_file("./stdlib/llvm_intrinsics.dk");
        self.load_file("./stdlib/asserts.dk");
        return true;
    }
}

pub struct Analyzer<'source> {
    pub hir: Vec<HIR>,

    source: &'source Source,
}

impl<'source> Analyzer<'source> {
    pub fn new(source: &'source Source) -> Analyzer<'source> {
        Analyzer {
            hir: vec![],
            source
        }
    }

    pub fn last_hir(&self, n: usize) -> &[HIR] {
        &self.hir[self.hir.len() - n..]
    }

    pub fn generate_mir_and_typecheck(&mut self, source: &'source Source) {
       

      
    }
}


#[cfg(test)]
pub mod test_utils {
    use crate::interner::InternedString;

    use super::Source;

    pub fn istr(str: &'static str) -> InternedString {
        InternedString::new(str)
    }


    pub fn parse(s: &str) -> Source {
        let mut source = Source::new();
        source.load_stdlib();
        source.load_str_ref("test", s);
        source
    }

    pub fn parse_no_stdlib(s: &str) -> Source {
        let mut source = Source::new();
        source.load_str_ref("test", s);
        source
    }

}
