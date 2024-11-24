use std::fmt::Display;

use hashbrown::HashMap;
use yuu_parse::parser::SourceCodeInfo;
use yuu_shared::ast::*;
use yuu_shared::token::{Integer, Token, TokenVariants};
use yuu_shared::{Context, Pass};

use crate::semantic_error::{GenericSemanticMsg, Note, NoteType, SemanticMsg, Severity};

#[derive(Clone, PartialEq, Eq)]
pub enum BuiltInType {
    I64,
    F32,
    F64,
    Error,
}

#[derive(Clone, PartialEq, Eq)]
pub enum TypeInfo {
    //Custom(String),
    BuiltIn(BuiltInType),
    // pub size: usize,
    // pub align: usize,
}

impl Display for TypeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeInfo::BuiltIn(built_in) => match built_in {
                BuiltInType::I64 => write!(f, "i64"),
                BuiltInType::F32 => write!(f, "f32"),
                BuiltInType::F64 => write!(f, "f64"),
                BuiltInType::Error => write!(f, "<error>"),
            },
        }
    }
}

pub struct TypeInfoMap {
    pub types: HashMap<usize, TypeInfo>,
}

pub struct TypeCheckPass<'a> {
    pub(crate) msgs: Vec<SemanticMsg>,
    pub(crate) temp_code_info: &'a SourceCodeInfo,
    pub(crate) failed: bool,
}

impl<'a> TypeCheckPass<'a> {
    pub fn new(source_code_info: &'a SourceCodeInfo) -> Self {
        Self {
            msgs: Vec::new(),
            temp_code_info: source_code_info,
            failed: false,
        }
    }

    fn infer(&mut self, root: &Node, data: &mut TypeInfoMap) -> TypeInfo {
        match root {
            Node::Expr(expr) => self.infer_expr(expr, data),
            Node::Stmt(stmt_node) => todo!(),
            Node::Type(type_node) => todo!(),
        }
    }

    fn get_type_from_lit(lit: &Token) -> TypeInfo {
        match lit.kind {
            TokenVariants::Integer(Integer::I64(_)) => TypeInfo::BuiltIn(BuiltInType::F64),
            TokenVariants::F32(_) => TypeInfo::BuiltIn(BuiltInType::F32),
            TokenVariants::F64(_) => TypeInfo::BuiltIn(BuiltInType::F64),
            _ => panic!("literal kind has no immediate type"),
        }
    }

    fn infer_expr(&mut self, expr: &ExprNode, data: &mut TypeInfoMap) -> TypeInfo {
        match expr {
            ExprNode::Literal(literal) => {
                let type_info = Self::get_type_from_lit(&literal.lit);
                data.types.insert(literal.id, type_info.clone());
                type_info
            }
            ExprNode::Binary(binary) => {
                let mut lhs = self.infer_expr(&binary.left, data);
                let rhs = self.infer_expr(&binary.right, data);

                if lhs != rhs {
                    self.failed = true;
                    let error = GenericSemanticMsg {
                        message: "Type mismatch in expression".to_string(),
                        cache: self.temp_code_info.cache.clone(),
                        span: binary.span.clone(),
                        notes: vec![
                            Note {
                                message: format!("This expression has type `{}`", lhs),
                                span: Some(binary.left.span()),
                                note_type: NoteType::Explanation,
                            },
                            Note {
                                message: format!("This expression has type `{}`", rhs),
                                span: Some(binary.right.span()),
                                note_type: NoteType::Explanation,
                            },
                            Note {
                                message: "These types must be the same".to_string(),
                                span: None,
                                note_type: NoteType::Help,
                            },
                        ],
                        severity: Severity::Error,
                    };
                    self.msgs.push(SemanticMsg::Generic(error));
                    lhs = TypeInfo::BuiltIn(BuiltInType::Error);
                }

                data.types.insert(binary.id, lhs.clone());
                lhs
            }
            ExprNode::Unary(unary_expr) => match unary_expr.op {
                UnaryOp::Negate | UnaryOp::Pos => {
                    let operand = self.infer_expr(&unary_expr.operand, data);
                    data.types.insert(unary_expr.id, operand.clone());
                    operand
                }
            },
            ExprNode::Ident(ident_expr) => {
                let type_info = TypeInfo::BuiltIn(BuiltInType::I64);
                data.types.insert(ident_expr.id, type_info.clone());
                type_info
            }
        }
    }
}

impl<'a> Pass for TypeCheckPass<'a> {
    fn run(&mut self, ast: &Node, context: &mut Context) -> bool {
        let mut data = TypeInfoMap {
            types: HashMap::new(),
        };
        self.infer(ast, &mut data);
        context.replace_pass_data(data);

        for msg in &self.msgs {
            msg.eprint();
        }

        self.failed
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ariadne::Source;
    use std::{cell::RefCell, rc::Rc};
    use yuu_parse::lexer::{ParseError, UnprocessedCodeInfo};
    use yuu_parse::parser::Parser;

    fn create_test_source(code: &str) -> Result<SourceCodeInfo, ParseError> {
        let code_info = UnprocessedCodeInfo {
            code,
            file_name: "test.yuu",
        };
        let mut parser = Parser::new(&code_info);
        let mut source_info = parser.parse()?;

        // Ensure source code is properly cached for error reporting
        let source = Source::from(code.to_string());
        let cache = Rc::new(RefCell::new((code_info.file_name.to_string(), source)));
        source_info.cache = cache;

        Ok(source_info)
    }

    #[test]
    fn test_valid_type_check() {
        let source_info = create_test_source("1 + 2 * 3").unwrap();
        let mut type_checker = TypeCheckPass::new(&source_info);
        let mut context = Context::new();
        type_checker.run(&source_info.root_node, &mut context);

        assert!(type_checker.msgs.is_empty());
    }

    #[test]
    fn test_invalid_type_check() {
        let source_info = create_test_source("1.0 + 2").unwrap();
        let mut type_checker = TypeCheckPass::new(&source_info);
        let mut context = Context::new();
        type_checker.run(&source_info.root_node, &mut context);

        assert_eq!(type_checker.msgs.len(), 1);
        for msg in &type_checker.msgs {
            msg.eprint();
        }
    }
}
