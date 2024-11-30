use std::{cell::RefCell, rc::Rc};

use crate::lexer::{GenericError, Lexer, Note, ParseError, UnprocessedCodeInfo};
use ariadne::Source;
use logos::Span;
use yuu_shared::{
    ast::*,
    token::{Token, TokenKind},
};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    code_info: &'a UnprocessedCodeInfo<'a>,
}

pub type SrcCache = Rc<RefCell<(String, Source)>>;

pub struct SourceCodeInfo {
    pub cache: Rc<RefCell<(String, Source)>>,
    pub root_node: Node,
}

impl<'a> Parser<'a> {
    pub fn new(code_info: &'a UnprocessedCodeInfo<'a>) -> Self {
        Self {
            lexer: Lexer::new(code_info),
            code_info,
        }
    }

    fn get_infix_precedence(op: &TokenKind) -> (i32, i32) {
        match op {
            TokenKind::Plus | TokenKind::Minus => (1, 1),
            TokenKind::Asterix | TokenKind::Slash => (2, 2),
            TokenKind::LParen => (7, 0),
            _ => (-1, -1),
        }
    }

    fn get_prefix_precedence(op: &TokenKind) -> i32 {
        match op {
            TokenKind::Plus | TokenKind::Minus => 5,
            _ => -1,
        }
    }

    fn make_unary_expr(
        &mut self,
        operand: ExprNode,
        op: Token,
        span: Span,
    ) -> Result<(Span, ExprNode), ParseError> {
        let unary_op = match op.kind {
            TokenKind::Plus => UnaryOp::Pos,
            TokenKind::Minus => UnaryOp::Negate,
            _ => {
                return Err(ParseError::GenericSyntaxError(GenericError {
                    expected: "an unary operator".to_string(),
                    found: format!("{:?}", op.kind),
                    span,
                    note: vec![],
                }));
            }
        };
        let span_copy = span.clone();
        let unary = UnaryExpr {
            operand: Box::new(operand),
            op: unary_op,
            span: span,
            id: 0,
        };
        Ok((span_copy, ExprNode::Unary(unary)))
    }

    fn make_literal_expr(&mut self, lit: Token) -> (Span, ExprNode) {
        let span = lit.span.clone();
        let lit_expr = LiteralExpr { lit, id: 0 };
        (span, ExprNode::Literal(lit_expr))
    }

    fn make_ident_expr(&mut self, ident: String, span: Span) -> (Span, ExprNode) {
        let span_clone = span.clone();
        let ident_expr = IdentExpr { ident, span, id: 0 };
        (span_clone, ExprNode::Ident(ident_expr))
    }

    fn parse_primary_expr(&mut self) -> Result<(Span, ExprNode), ParseError> {
        let t = self.lexer.next_token()?;

        match &t.kind {
            TokenKind::F32(_) | TokenKind::F64(_) | TokenKind::Integer(_) => {
                Ok(self.make_literal_expr(t))
            }

            TokenKind::Ident(ident) => Ok(self.make_ident_expr(ident.clone(), t.span)),

            TokenKind::Plus | TokenKind::Minus => {
                let prefix_precedence = Self::get_prefix_precedence(&t.kind);
                let (span, operand) = self.parse_expr_chain(prefix_precedence)?;
                let span = t.span.start..span.end;
                let unary = self.make_unary_expr(operand, t, span)?;
                Ok(unary)
            }
            TokenKind::LParen => {
                let (span, lhs) = self.parse_expr_chain(0)?;

                let token_lparen_span = t.span.clone();

                let must_be_rparen = self.lexer.next_token()?;

                let span = span.start..must_be_rparen.span.end;

                match must_be_rparen.kind {
                    TokenKind::RParen => Ok((span, lhs)),
                    _ => Err(ParseError::GenericSyntaxError(GenericError {
                        expected: "a closing parenthesis".to_string(),
                        found: format!("{:?}", must_be_rparen.kind),
                        span,
                        note: vec![
                            Note {
                                message: "the opening parenthesis is here".to_string(),
                                span: token_lparen_span,
                            },
                            Note {
                                message: "the closing parenthesis should be here".to_string(),
                                span: must_be_rparen.span.clone(),
                            },
                        ],
                    })),
                }
            }
            _ => Err(ParseError::GenericSyntaxError({
                let expected = "a primary expression".to_string();
                let found = format!("{:?}", t.kind);
                let span = t.span;
                GenericError {
                    expected,
                    found,
                    span,
                    note: vec![],
                }
            })),
        }
    }

    fn make_bin_expr(
        &mut self,
        lhs: ExprNode,
        rhs: ExprNode,
        token: Token,
        span: Span,
    ) -> Result<(Span, ExprNode), ParseError> {
        let bin_op = match token.kind {
            TokenKind::Plus => BinOp::Add,
            TokenKind::Minus => BinOp::Subtract,
            TokenKind::Asterix => BinOp::Multiply,
            TokenKind::Slash => BinOp::Divide,
            _ => {
                return Err(ParseError::GenericSyntaxError(GenericError {
                    expected: "a binary operator".to_string(),
                    found: format!("{:?}", token.kind),
                    span: token.span,
                    note: vec![],
                }));
            }
        };
        let span_copy = span.clone();
        let binary = BinaryExpr {
            left: Box::new(lhs),
            right: Box::new(rhs),
            op: bin_op,
            span,
            id: 0,
        };
        Ok((span_copy, ExprNode::Binary(binary)))
    }

    fn parse_bin_expr(
        &mut self,
        lhs: ExprNode,
        op_token: Token,
        min_precedence: i32,
    ) -> Result<(Span, ExprNode), ParseError> {
        let (span, rhs) = self.parse_expr_chain(min_precedence)?;
        let span = lhs.span().start..span.end;
        let lhs = self.make_bin_expr(lhs, rhs, op_token, span)?;
        Ok(lhs)
    }

    fn parse_expr_chain(&mut self, min_precedence: i32) -> Result<(Span, ExprNode), ParseError> {
        let mut lhs = self.parse_primary_expr()?;
        loop {
            let op = self.lexer.peek()?;

            let (left_precedence, right_precedence) = Self::get_infix_precedence(&op.kind);

            if left_precedence < min_precedence {
                return Ok(lhs);
            }

            let op = self.lexer.next_token()?;

            match &op.kind {
                TokenKind::Plus | TokenKind::Minus | TokenKind::Asterix | TokenKind::Slash => {
                    lhs = self.parse_bin_expr(lhs.1, op, right_precedence)?;
                }
                TokenKind::LParen => {
                    lhs = self.parse_func_call_expr(lhs.1, op, right_precedence)?;
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn parse_expr(&mut self) -> Result<(Span, ExprNode), ParseError> {
        self.parse_expr_chain(0)
    }

    pub fn parse_type(&mut self) -> Result<TypeNode, ParseError> {
        let t = self.lexer.next_token()?;
        let out = match t.kind {
            TokenKind::I64Kw | TokenKind::F32Kw | TokenKind::F64Kw => (
                t.span.clone(),
                TypeNode::BuiltIn(BuiltInType {
                    span: t.span.clone(),
                    kind: match t.kind {
                        TokenKind::I64Kw => BuiltInTypeKind::I64,
                        TokenKind::F32Kw => BuiltInTypeKind::F32,
                        TokenKind::F64Kw => BuiltInTypeKind::F64,
                        _ => unreachable!(),
                    },
                    id: 0,
                }),
            ),
            TokenKind::Ident(name) => {
                let span = t.span.clone();
                let ty = TypeNode::Ident(IdentType {
                    span: span.clone(),
                    name,
                    id: 0,
                });
                (span, ty)
            }
            _ => {
                return Err(ParseError::GenericSyntaxError(GenericError {
                    expected: "a type".to_string(),
                    found: format!("{:?}", t.kind),
                    span: t.span.clone(),
                    note: vec![],
                }))
            }
        };
        Ok(out.1)
    }

    pub fn parse_atomic_stmt(&mut self) -> Result<(Span, StmtNode), ParseError> {
        let (span, expr) = self.parse_expr()?;
        let stmt = StmtNode::Atomic(expr);
        Ok((span, stmt))
    }

    pub fn parse_pattern(&mut self) -> Result<PatternNode, ParseError> {
        let t = self.lexer.next_token()?;
        match t.kind {
            TokenKind::Ident(ident) => Ok(PatternNode::Ident(IdentPattern {
                span: t.span.clone(),
                name: ident,
                id: 0,
            })),
            _ => Err(ParseError::GenericSyntaxError(GenericError {
                expected: "an pattern naming the binding".to_string(),
                found: format!("{:?}", t.kind),
                span: t.span.clone(),
                note: vec![],
            })),
        }
    }

    pub fn parse_let_stmt(&mut self) -> Result<(Span, StmtNode), ParseError> {
        let let_tkn = self.lexer.expect(&[TokenKind::Let], Vec::new())?;
        let pattern = self.parse_pattern()?;
        let la = self.lexer.peek()?;
        let ty = if la.kind == TokenKind::Colon {
            let out = self.parse_type()?;
            Some(out)
        } else {
            None
        };
        let _ = self.lexer.expect(&[TokenKind::Equal], Vec::new())?;
        let (span, expr) = self.parse_expr()?;
        let span = let_tkn.span.start..span.end;
        let stmt = StmtNode::Let(LetStmt {
            span: span.clone(),
            pattern: Box::new(pattern),
            expr: Box::new(expr),
            ty,
            id: 0,
        });
        Ok((span, stmt))
    }

    pub fn parse_ret_stmt(&mut self) -> Result<(Span, StmtNode), ParseError> {
        let ret_tkn = self
            .lexer
            .expect(&[TokenKind::Return, TokenKind::Result], Vec::new())?;

        let (span, expr) = self.parse_expr()?;
        let span = ret_tkn.span.start..span.end;
        let stmt = StmtNode::Return(ReturnStmt {
            span: span.clone(),
            expr: Box::new(expr),
            ty: match &ret_tkn.kind {
                TokenKind::Return => ReturnStmtType::ReturnFromFunction,
                TokenKind::Result => ReturnStmtType::ReturnFromBlock,
                _ => panic!("Expected return or result token"),
            },
            id: 0,
        });
        Ok((span, stmt))
    }

    pub fn parse_func_arg(&mut self) -> Result<FuncArg, ParseError> {
        let pattern = self.parse_pattern()?;
        let _ = self.lexer.expect(&[TokenKind::Colon], Vec::new())?;
        let ty = self.parse_type()?;
        let span = pattern.span().start..ty.span().end;
        Ok(FuncArg {
            span: span.clone(),
            pattern,
            ty,
            id: 0,
        })
    }

    pub fn parse_func_args(&mut self) -> Result<Vec<FuncArg>, ParseError> {
        let mut args = Vec::new();
        loop {
            let la = self.lexer.peek()?;
            if la.kind == TokenKind::RParen {
                return Ok(args);
            }
            let arg = self.parse_func_arg()?;
            args.push(arg);
            let la = self.lexer.peek()?;
            if la.kind == TokenKind::Comma {
                let _ = self.lexer.next_token()?;
            } else if la.kind != TokenKind::RParen {
                return Err(ParseError::GenericSyntaxError(GenericError {
                    expected: "a comma or a closing parenthesis".to_string(),
                    found: format!("{:?}", la.kind),
                    span: la.span.clone(),
                    note: vec![],
                }));
            }
        }
    }

    pub fn make_block_expr(&mut self, stmts: Vec<StmtNode>, span: Span) -> (Span, BlockExpr) {
        (
            span.clone(),
            BlockExpr {
                id: 0,
                span,
                body: stmts,
            },
        )
    }

    pub fn parse_func_call_expr(
        &mut self,
        lhs: ExprNode,
        _op_token: Token,
        _min_precedence: i32,
    ) -> Result<(Span, ExprNode), ParseError> {
        let mut args = Vec::new();
        loop {
            let la = self.lexer.peek()?;
            if la.kind == TokenKind::RParen {
                let rparen = self.lexer.next_token()?;
                let span = lhs.span().start..rparen.span.end;
                let func_call = FuncCallExpr {
                    lhs: Box::new(lhs),
                    args,
                    span: span.clone(),
                    id: 0,
                };
                return Ok((span, ExprNode::FuncCall(func_call)));
            } else if la.kind == TokenKind::Comma {
                let _ = self.lexer.next_token()?;
            }
            let (_, arg) = self.parse_expr_chain(0)?;
            args.push(arg);
        }
    }

    pub fn parse_block_expr(&mut self) -> Result<(Span, BlockExpr), ParseError> {
        let lbrace = self.lexer.expect(&[TokenKind::LBrace], Vec::new())?;
        let mut stmts = Vec::new();
        loop {
            let tkn = self.lexer.peek()?;
            if tkn.kind == TokenKind::RBrace {
                let rbrace = self.lexer.next_token()?;
                let span = lbrace.span.start..rbrace.span.end;
                let block_expr = self.make_block_expr(stmts, span);
                return Ok(block_expr);
            }
            // TODO: Add specific error message that we expected a stmt here if we find something else...
            let (_, stmt) = self.parse_stmt()?;
            stmts.push(stmt);
        }
    }

    pub fn make_func_decl(
        &mut self,
        name: String,
        span: Span,
        args: Vec<FuncArg>,
        ret_ty: Option<Box<TypeNode>>,
    ) -> (Span, FuncDeclStructural) {
        (
            span.clone(),
            FuncDeclStructural {
                id: 0,
                span,
                name,
                args,
                ret_ty,
            },
        )
    }

    pub fn parse_func_decl(&mut self) -> Result<(Span, FuncDeclStructural), ParseError> {
        let fn_tkn = self.lexer.expect(&[TokenKind::Fn], Vec::new())?;
        let ident = self.lexer.next_token()?;
        match ident.kind {
            TokenKind::Ident(ident) => {
                let _ = self.lexer.expect(&[TokenKind::LParen], Vec::new())?;
                let args = self.parse_func_args()?;
                let rparen = self.lexer.expect(&[TokenKind::RParen], Vec::new())?;
                let ret_ty = if self.lexer.peek()?.kind == TokenKind::Arrow {
                    let _ = self.lexer.next_token()?;
                    Some(Box::new(self.parse_type()?))
                } else {
                    None
                };
                let span = fn_tkn.span.start..if let Some(ty) = &ret_ty {
                    ty.span().end
                } else {
                    rparen.span.end
                };
                Ok(self.make_func_decl(ident, span, args, ret_ty))
            }
            _ => Err(ParseError::GenericSyntaxError(GenericError {
                expected: "an identifier naming the function".to_string(),
                found: format!("{:?}", ident.kind),
                span: ident.span.clone(),
                note: vec![Note {
                    message: "The fn keyword is here, indicating a function declaration"
                        .to_string(),
                    span: fn_tkn.span.clone(),
                }],
            })),
        }
    }

    pub fn make_func_def(
        &mut self,
        span: Span,
        decl: FuncDeclStructural,
        body: BlockExpr,
    ) -> (Span, FuncDefStructural) {
        (
            span.clone(),
            FuncDefStructural {
                id: 0,
                decl,
                body,
                span,
            },
        )
    }

    pub fn parse_func_def(&mut self) -> Result<(Span, FuncDefStructural), ParseError> {
        let (span, decl) = self.parse_func_decl()?;
        let (block_span, block) = self.parse_block_expr()?;
        let span = span.start..block_span.end;
        Ok(self.make_func_def(span, decl, block))
    }

    fn parse_semicolon(&mut self, x: Span) -> Result<Span, ParseError> {
        let smcln = self.lexer.expect(&[TokenKind::Semicolon], Vec::new())?;
        Ok(x.start..smcln.span.end)
    }

    pub fn parse_stmt(&mut self) -> Result<(Span, StmtNode), ParseError> {
        let t = self.lexer.peek()?;

        let x = match t.kind {
            TokenKind::Let => {
                let (span, stmt) = self.parse_let_stmt()?;
                (self.parse_semicolon(span)?, stmt)
            }
            TokenKind::Return | TokenKind::Result => {
                let (span, stmt) = self.parse_ret_stmt()?;
                (self.parse_semicolon(span)?, stmt)
            }
            _ => {
                let (span, stmt) = self.parse_atomic_stmt()?;
                (self.parse_semicolon(span)?, stmt)
            }
        };

        Ok(x)
    }

    pub fn parse_structural(&mut self) -> Result<(Span, StructuralNode), ParseError> {
        let t = self.lexer.peek()?;
        let x = match t.kind {
            TokenKind::Fn => {
                let (span, func_def) = self.parse_func_def()?;
                (span, StructuralNode::FuncDef(func_def))
            }
            _ => unreachable!("only fn is supported as structural node"),
        };
        Ok(x)
    }

    pub fn parse(&mut self) -> Result<SourceCodeInfo, ParseError> {
        let (_, structural) = self.parse_structural()?;
        let root_node = Node::Structural(structural);
        let source_code = ariadne::Source::from(self.code_info.code.to_string());
        let source_code_info = SourceCodeInfo {
            cache: Rc::new(RefCell::new((
                self.code_info.file_name.to_string(),
                source_code,
            ))),
            root_node,
        };
        Ok(source_code_info)
    }
}

#[cfg(test)]
mod tests {

    use crate::add_ids::{self};

    use super::*;

    #[test]
    fn test_literal_expression() {
        let code_info = UnprocessedCodeInfo {
            code: "42",
            file_name: "test.yuu",
        };
        let mut parser = Parser::new(&code_info);
        let result = parser.parse();
        assert!(result.is_ok());
        if let Ok(Node::Expr(expr)) = result.as_ref().map(|x| &x.root_node) {
            match expr {
                ExprNode::Literal(lit) => assert_eq!(
                    lit.lit.kind,
                    TokenKind::Integer(yuu_shared::token::Integer::I64(42))
                ),
                _ => panic!("Expected a literal expression"),
            }
        }
    }

    #[test]
    fn test_unary_expression() {
        let code_info = UnprocessedCodeInfo {
            code: "-42",
            file_name: "test.yuu",
        };
        let mut parser = Parser::new(&code_info);
        let result = parser.parse();
        assert!(result.is_ok());
    }

    #[test]
    fn test_binary_expression() {
        let code_info = UnprocessedCodeInfo {
            code: "1 + 2",
            file_name: "test.yuu",
        };
        let mut parser = Parser::new(&code_info);
        let result = parser.parse();
        assert!(result.is_ok());
    }

    #[test]
    fn test_complex_expression() {
        let code_info = UnprocessedCodeInfo {
            code: "--1 + 2 + 3 * --4 * +-5 + 6",
            file_name: "test.yuu",
        };
        let mut parser = Parser::new(&code_info);
        let result = parser.parse();
        assert!(result.is_ok()); // Fixed assertion
        let mut result = result.unwrap();
        add_ids::add_ids(&mut result.root_node);
        let ron = result.root_node.to_graphviz_string();
        println!("{}", ron);
    }

    // Syntax error tests
    #[test]
    fn test_syntax_error() {
        let code_info = UnprocessedCodeInfo {
            code: "1 + 2 - * 4",
            file_name: "test.yuu",
        };
        let mut parser = Parser::new(&code_info);
        let result = parser.parse();

        assert!(result.is_err()); // Changed to assert error

        if let Err(err) = result {
            err.print(&code_info);
        }
    }

    #[test]
    fn test_syntax_error_parentesis() {
        let code_info = UnprocessedCodeInfo {
            code: "1 +2 + 3+ 4+ (1 + (2 - 4 + 5)",
            file_name: "test.yuu",
        };
        let mut parser = Parser::new(&code_info);
        let result = parser.parse();
        assert!(result.is_err());

        if let Err(err) = result {
            err.print(&code_info);
        }
    }

    #[test]
    fn test_function_definition() {
        let code_info = UnprocessedCodeInfo {
            code: "fn fac(n: i64) -> i64 {
            let n_out = n * fac(n - 1);
            return n_out;
        }",
            file_name: "test.yuu",
        };

        let mut parser = Parser::new(&code_info);
        let result = parser.parse();

        if let Err(err) = result {
            err.print(&code_info);
        } else {
            let mut result = result.unwrap();
            add_ids::add_ids(&mut result.root_node);
            println!("{}", result.root_node.to_graphviz_string());
        }
    }
}
