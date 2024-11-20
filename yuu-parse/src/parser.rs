use std::{cell::RefCell, rc::Rc};

use crate::lexer::{GenericError, Lexer, Note, ParseError, UnprocessedCodeInfo};
use ariadne::Source;
use logos::Span;
use yuu_shared::{
    ast::*,
    token::{Token, TokenVariants},
};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    code_info: &'a UnprocessedCodeInfo<'a>,
    current_node_id: usize,
}

pub type SrcCache = Rc<RefCell<(String, Source)>>;

#[derive(Debug)]
pub struct SourceCodeInfo {
    pub cache: Rc<RefCell<(String, Source)>>,
    pub root_node: Node,
}

impl<'a> Parser<'a> {
    pub fn new(code_info: &'a UnprocessedCodeInfo<'a>) -> Self {
        Self {
            lexer: Lexer::new(code_info),
            code_info,
            current_node_id: 0,
        }
    }

    fn get_infix_precedence(op: &TokenVariants) -> (i32, i32) {
        match op {
            TokenVariants::Plus | TokenVariants::Minus => (1, 1),
            TokenVariants::Asterix | TokenVariants::Slash => (2, 2),
            _ => (-1, -1),
        }
    }

    fn get_prefix_precedence(op: &TokenVariants) -> i32 {
        match op {
            TokenVariants::Plus | TokenVariants::Minus => 5,
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
            TokenVariants::Plus => UnaryOp::Pos,
            TokenVariants::Minus => UnaryOp::Negate,
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
        self.current_node_id += 1;
        let unary = UnaryExpr {
            operand: Box::new(operand),
            op: unary_op,
            span: span,
            id: self.current_node_id,
        };
        Ok((span_copy, ExprNode::Unary(unary)))
    }

    fn make_literal_expr(&mut self, lit: Token) -> (Span, ExprNode) {
        self.current_node_id += 1;
        let span = lit.span.clone();
        let lit_expr = LiteralExpr {
            lit,
            id: self.current_node_id,
        };
        (span, ExprNode::Literal(lit_expr))
    }

    fn make_ident_expr(&mut self, ident: String, span: Span) -> (Span, ExprNode) {
        self.current_node_id += 1;
        let span_clone = span.clone();
        let ident_expr = IdentExpr {
            ident,
            span,
            id: self.current_node_id,
        };
        (span_clone, ExprNode::Ident(ident_expr))
    }

    fn parse_primary_expr(&mut self) -> Result<(Span, ExprNode), ParseError> {
        let t = self.lexer.next_token()?;

        self.current_node_id += 1;
        match &t.kind {
            TokenVariants::F32(_) | TokenVariants::F64(_) | TokenVariants::Integer(_) => {
                Ok(self.make_literal_expr(t))
            }

            TokenVariants::Ident(ident) => Ok(self.make_ident_expr(ident.clone(), t.span)),

            TokenVariants::Plus | TokenVariants::Minus => {
                let prefix_precedence = Self::get_prefix_precedence(&t.kind);
                let (span, operand) = self.parse_expr_chain(prefix_precedence)?;
                let span = t.span.start..span.end;
                let unary = self.make_unary_expr(operand, t, span)?;
                Ok(unary)
            }
            TokenVariants::LParen => {
                let (span, lhs) = self.parse_expr_chain(0)?;

                let token_lparen_span = t.span.clone();

                let must_be_rparen = self.lexer.next_token()?;

                let span = span.start..must_be_rparen.span.end;

                match must_be_rparen.kind {
                    TokenVariants::RParen => Ok((span, lhs)),
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
            TokenVariants::Plus => BinOp::Add,
            TokenVariants::Minus => BinOp::Subtract,
            TokenVariants::Asterix => BinOp::Multiply,
            TokenVariants::Slash => BinOp::Divide,
            _ => {
                return Err(ParseError::GenericSyntaxError(GenericError {
                    expected: "a binary operator".to_string(),
                    found: format!("{:?}", token.kind),
                    span: token.span,
                    note: vec![],
                }));
            }
        };
        self.current_node_id += 1;
        let span_copy = span.clone();
        let binary = BinaryExpr {
            left: Box::new(lhs),
            right: Box::new(rhs),
            op: bin_op,
            span,
            id: self.current_node_id,
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
                TokenVariants::Plus
                | TokenVariants::Minus
                | TokenVariants::Asterix
                | TokenVariants::Slash => {
                    lhs = self.parse_bin_expr(lhs.1, op, right_precedence)?;
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn parse_expr(&mut self) -> Result<(Span, ExprNode), ParseError> {
        self.parse_expr_chain(0)
    }

    pub fn parse(&mut self) -> Result<SourceCodeInfo, ParseError> {
        let (_, expr) = self.parse_expr()?;
        let root_node = Node::Expr(expr);
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
                    TokenVariants::Integer(yuu_shared::token::Integer::I64(42))
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
        if let Ok(Node::Expr(expr)) = result.as_ref().map(|x| &x.root_node) {
            match expr {
                ExprNode::Unary(unary) => {
                    assert_eq!(unary.op, UnaryOp::Negate);
                    if let ExprNode::Literal(lit) = unary.operand.as_ref() {
                        assert_eq!(
                            lit.lit.kind,
                            TokenVariants::Integer(yuu_shared::token::Integer::I64(42))
                        );
                    } else {
                        panic!("Expected a literal operand");
                    }
                }
                _ => panic!("Expected a unary expression"),
            }
        }
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
        if let Ok(Node::Expr(expr)) = result.map(|x| x.root_node) {
            match expr {
                ExprNode::Binary(binary) => {
                    assert_eq!(binary.op, BinOp::Add);
                    if let ExprNode::Literal(lit) = *binary.left {
                        assert_eq!(
                            lit.lit.kind,
                            TokenVariants::Integer(yuu_shared::token::Integer::I64(1))
                        );
                    } else {
                        panic!("Expected a literal left operand");
                    }
                    if let ExprNode::Literal(lit) = *binary.right {
                        assert_eq!(
                            lit.lit.kind,
                            TokenVariants::Integer(yuu_shared::token::Integer::I64(2))
                        );
                    } else {
                        panic!("Expected a literal right operand");
                    }
                }
                _ => panic!("Expected a binary expression"),
            }
        }
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
        let result = result.unwrap();

        use std::fmt::Write;
        let mut output = String::new();
        write!(&mut output, "{}", result.root_node).unwrap();
        println!("{}", output);
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
        result.unwrap_err().print(&code_info);
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
        result.unwrap_err().print(&code_info);
    }
}
