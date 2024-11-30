use std::fmt::Display;

use hashbrown::HashMap;
use yuu_parse::parser::SourceCodeInfo;
use yuu_shared::ast::*;
use yuu_shared::token::{Integer, Token, TokenKind};
use yuu_shared::{Context, Pass};

use crate::semantic_error::{GenericSemanticMsg, Note, NoteType, SemanticMsg, Severity};

#[derive(Clone, PartialEq, Eq)]
pub enum BuiltInType {
    I64,
    F32,
    F64,
    Nil,
    Error,
}

#[derive(Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub args: Vec<TypeInfo>,
    pub ret: Box<TypeInfo>,
}

#[derive(Clone, PartialEq, Eq)]
pub enum TypeInfo {
    //Custom(String),
    BuiltIn(BuiltInType),
    Function(FunctionType),
    // pub size: usize,
    // pub align: usize,
}

impl From<TypeNode> for TypeInfo {
    fn from(ty: TypeNode) -> Self {
        match ty {
            TypeNode::BuiltIn(built_in) => match built_in.kind {
                yuu_shared::ast::BuiltInTypeKind::I64 => TypeInfo::BuiltIn(BuiltInType::I64),
                yuu_shared::ast::BuiltInTypeKind::F32 => TypeInfo::BuiltIn(BuiltInType::F32),
                yuu_shared::ast::BuiltInTypeKind::F64 => TypeInfo::BuiltIn(BuiltInType::F64),
            },
            TypeNode::Ident(_ident) => todo!(),
        }
    }
}

impl TypeInfo {
    pub fn does_coerce_to_same_type(&self, other: &Self) -> bool {
        self == other
    }
}

impl Display for TypeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeInfo::BuiltIn(built_in) => match built_in {
                BuiltInType::I64 => write!(f, "i64"),
                BuiltInType::F32 => write!(f, "f32"),
                BuiltInType::F64 => write!(f, "f64"),
                BuiltInType::Error => write!(f, "<error>"),
                BuiltInType::Nil => write!(f, "nil"),
            },
        }
    }
}

pub struct Block {
    pub symbols: HashMap<String, TypeInfo>,
    pub parent: Option<usize>,
}

pub struct TypeInfoMap {
    pub types: HashMap<usize, TypeInfo>,
    pub blocks: HashMap<usize, Block>,
}

pub struct TypeCheckPass<'a> {
    pub(crate) msgs: Vec<SemanticMsg>,
    pub(crate) temp_code_info: &'a SourceCodeInfo,
    pub(crate) failed: bool,
    pub(crate) current_block: Option<usize>,
    pub(crate) current_function: Option<usize>,
}

impl<'a> TypeCheckPass<'a> {
    pub fn new(source_code_info: &'a SourceCodeInfo) -> Self {
        Self {
            msgs: Vec::new(),
            temp_code_info: source_code_info,
            failed: false,
            current_block: None,
        }
    }

    fn infer_stmt(
        &mut self,
        stmt: &StmtNode,
        data: &mut TypeInfoMap,
    ) -> (TypeInfo, Option<ReturnStmtType>) {
        match stmt {
            StmtNode::Atomic(expr) => (self.infer_expr(expr, data), None),
            StmtNode::Let(let_stmt) => {
                let type_info_inferred = self.infer_expr(&let_stmt.expr, data);
                let type_info_declared: Option<TypeInfo> =
                    let_stmt.ty.as_ref().map(|ty| ty.clone().into());
                let out_type = if let Some(t_decl) = type_info_declared {
                    if !type_info_inferred.does_coerce_to_same_type(&t_decl) {
                        self.failed = true;
                        let error = GenericSemanticMsg {
                            message: "Type mismatch in let statement".to_string(),
                            cache: self.temp_code_info.cache.clone(),
                            span: let_stmt.span.clone(),
                            notes: vec![
                                Note {
                                    message: format!(
                                        "variable `{}` is declared as `{}` here",
                                        let_stmt.pattern, t_decl
                                    ),
                                    span: let_stmt.ty.as_ref().map(|ty| ty.span().clone()),
                                    note_type: NoteType::Explanation,
                                },
                                Note {
                                    message: format!(
                                        "But its expression was inferred to have type `{}`",
                                        type_info_inferred
                                    ),
                                    span: None,
                                    note_type: NoteType::Explanation,
                                },
                            ],
                            severity: Severity::Error,
                        };
                        self.msgs.push(SemanticMsg::Generic(error));
                        return (TypeInfo::BuiltIn(BuiltInType::Error), None);
                    }
                    t_decl
                } else {
                    type_info_inferred
                };
                data.types.insert(let_stmt.id, out_type.clone());
                (out_type, None)
            }
            StmtNode::Return(return_stmt) => {
                let type_info = self.infer_expr(&return_stmt.expr, data);
                data.types.insert(return_stmt.id, type_info.clone());
                (type_info, None)
            }
        }
    }

    fn infer(&mut self, root: &Node, data: &mut TypeInfoMap) -> TypeInfo {
        match root {
            Node::Expr(expr) => self.infer_expr(expr, data),
            Node::Stmt(stmt_node) => self.infer_stmt(stmt_node, data).0,
            Node::Type(type_node) => todo!(),
            Node::Structural(structural_node) => todo!(),
            Node::Pattern(pattern_node) => todo!(),
        }
    }

    fn get_type_from_lit(lit: &Token) -> TypeInfo {
        match lit.kind {
            TokenKind::Integer(Integer::I64(_)) => TypeInfo::BuiltIn(BuiltInType::F64),
            TokenKind::F32(_) => TypeInfo::BuiltIn(BuiltInType::F32),
            TokenKind::F64(_) => TypeInfo::BuiltIn(BuiltInType::F64),
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
            ExprNode::Block(block_expr) => {
                let old_parent = self.current_block;
                let block = Block {
                    symbols: HashMap::new(),
                    parent: self.current_block,
                };

                self.current_block = Some(block_expr.id);
                data.blocks.insert(block_expr.id, block);

                for stmt in &block_expr.body {
                    let (ty, ret_stmt) = self.infer_stmt(stmt, data);
                    if let Some(ReturnStmtType::ReturnFromBlock) = ret_stmt {
                        self.current_block = old_parent;
                        return ty;
                    } else if let Some(ReturnStmtType::ReturnFromFunction) = ret_stmt {
                        todo!("return from function in block");
                        // Check if the function return type and the type of this block coerece to the same type
                        // If not -> error

                        // First check if we are in a function
                        if self.current_function.is_none() {
                            self.failed = true;
                            let error = GenericSemanticMsg {
                                message: "Return statement outside of function".to_string(),
                                cache: self.temp_code_info.cache.clone(),
                                span: stmt.span().clone(),
                                notes: vec![],
                                severity: Severity::Error,
                            };
                            self.msgs.push(SemanticMsg::Generic(error));
                        }

                        // TODO: This needs to be checked at the top level actually...
                        /*let current_function = self.current_function.unwrap();
                        let ty_func = data.types.get(&current_function);

                        if let Some(ty_func) = ty_func {
                            if !ty_func.does_coerce_to_same_type(&ty) {
                                self.failed = true;
                                let error = GenericSemanticMsg {
                                    message: "Type mismatch in function return".to_string(),
                                    cache: self.temp_code_info.cache.clone(),
                                    span: stmt.span().clone(),
                                    notes: vec![
                                        Note {
                                            message: format!("This block has type `{}`", ty),
                                            span: Some(block_expr.span.clone()),
                                            note_type: NoteType::Explanation,
                                        },
                                        Note {
                                            message: format!(
                                                "But the function return type is `{}`",
                                                current_function
                                            ),
                                            span: None,
                                            note_type: NoteType::Explanation,
                                        },
                                    ],
                                    severity: Severity::Error,
                                };
                                self.msgs.push(SemanticMsg::Generic(error));
                            } */

                        // } else {
                        //     // Function currently has no return type set -> set it
                        //     data.types.insert(current_function, ty);
                        // }
                    }
                }

                self.current_block = old_parent;

                TypeInfo::BuiltIn(BuiltInType::Nil)
            }
            ExprNode::FuncCall(func_call_expr) => {
                let lhs_ty = self.infer_expr(&func_call_expr.lhs, data);

                let arg_tys = func_call_expr
                    .args
                    .iter()
                    .map(|arg| self.infer_expr(arg, data));

                match lhs_ty {
                    TypeInfo::BuiltIn(_) => {
                        self.failed = true;
                        let error = GenericSemanticMsg {
                            message: "Function call on non-function".to_string(),
                            cache: self.temp_code_info.cache.clone(),
                            span: func_call_expr.span.clone(),
                            notes: vec![],
                            severity: Severity::Error,
                        };
                        self.msgs.push(SemanticMsg::Generic(error));
                    }

                    TypeInfo::Function(fun) => {
                        for (idx, arg_ty) in arg_tys.enumerate() {
                            let expected_ty = fun.args.get(idx);
                            if expected_ty.is_none() {
                                self.failed = true;
                                let error = GenericSemanticMsg {
                                    message: "Too many arguments in function call".to_string(),
                                    cache: self.temp_code_info.cache.clone(),
                                    span: func_call_expr.span.clone(),
                                    notes: vec![],
                                    severity: Severity::Error,
                                };
                                self.msgs.push(SemanticMsg::Generic(error));
                                break;
                            }
                            let expected_ty = expected_ty.unwrap(); 
                            if !expected_ty.does_coerce_to_same_type(&arg_ty) {
                                self.failed = true;
                                let error = GenericSemanticMsg {
                                    message: "Type mismatch in function call".to_string(),
                                    cache: self.temp_code_info.cache.clone(),
                                    span: func_call_expr.span.clone(),
                                    notes: vec![
                                        Note {
                                            message: format!("Expected type `{}`", expected_ty),
                                            span: Some(func_call_expr.args[idx].span()),
                                            note_type: NoteType::Explanation,
                                        },
                                        Note {
                                            message: format!("But found type `{}`", arg_ty),
                                            span: Some(func_call_expr.args[idx].span()),
                                            note_type: NoteType::Explanation,
                                        },
                                    ],
                                    severity: Severity::Error,
                                };
                                self.msgs.push(SemanticMsg::Generic(error));
                        }
                    } 
                }

                assert!(lhs_ty.does_coerce_to_same_type(&TypeInfo::Function(rhs_ty)));

                data.types.insert(func_call_expr.id, lhs_ty.clone());

                todo!()
            }
        }
    }
}

impl<'a> Pass for TypeCheckPass<'a> {
    fn run(&mut self, ast: &Node, context: &mut Context) -> bool {
        let mut data = TypeInfoMap {
            types: HashMap::new(),
            blocks: HashMap::new(),
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
