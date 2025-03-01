use crate::{
    add_ids::add_ids,
    lexer::{CatchIn, Lexer, ParseError, ParseResult, UnprocessedCodeInfo},
};
use logos::Span;
use yuu_shared::{
    ast::*,
    block::FUNC_BLOCK_NAME,
    error::{ErrorKind, YuuError},
    token::{Token, TokenKind},
};

pub struct Parser {
    lexer: Lexer,
    errors: Vec<ParseError>,
}

impl Parser {
    pub fn new(code_info: &UnprocessedCodeInfo) -> Self {
        Self {
            lexer: Lexer::new(code_info),
            errors: Vec::new(),
        }
    }

    fn get_infix_precedence(op: &TokenKind) -> (i32, i32) {
        match op {
            TokenKind::LParen => (8, 0), // Function calls highest
            TokenKind::Asterix | TokenKind::Slash => (6, 6), // Multiplication/division
            TokenKind::Plus | TokenKind::Minus => (5, 5), // Addition/subtraction
            TokenKind::EqEq => (4, 4),   // Equality
            _ => (-1, -1),
        }
    }

    fn get_prefix_precedence(op: &TokenKind) -> i32 {
        match op {
            TokenKind::Plus | TokenKind::Minus => 7, // Unary operators higher than binary
            _ => -1,
        }
    }

    fn make_unary_expr(
        &mut self,
        operand: ExprNode,
        op: Token,
        span: Span,
    ) -> ParseResult<(Span, ExprNode)> {
        let unary_op = match op.kind {
            TokenKind::Plus => UnaryOp::Pos,
            TokenKind::Minus => UnaryOp::Negate,
            _ => {
                self.errors.push(
                    YuuError::builder()
                        .kind(ErrorKind::InvalidSyntax)
                        .message(format!("Expected an unary operator, found {:?}", op.kind))
                        .source(
                            self.lexer.code_info.code.clone(),
                            self.lexer.code_info.file_name.clone(),
                        )
                        .span(span.clone(), "invalid unary operator")
                        .build(),
                );
                return Err(self.lexer.synchronize());
            }
        };
        let span_copy = span.clone();
        let unary = UnaryExpr {
            operand: Box::new(operand),
            op: unary_op,
            span,
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

    fn parse_primary_expr(&mut self) -> ParseResult<(Span, ExprNode)> {
        let t = self.lexer.next_token();
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

                let must_be_rparen = self.lexer.next_token();

                let span = span.start..must_be_rparen.span.end;

                match must_be_rparen.kind {
                    TokenKind::RParen => Ok((span, lhs)),
                    _ => {
                        let mut error = YuuError::unexpected_token(
                            must_be_rparen.span.clone(),
                            "a closing parenthesis ')'".to_string(),
                            format!("{:?}", must_be_rparen.kind),
                            self.lexer.code_info.code.clone(),
                            self.lexer.code_info.file_name.clone(),
                        );

                        error = error.with_related_info(
                            "The opening parenthesis is here".to_string(),
                            Some((token_lparen_span, "opening parenthesis")),
                        );

                        error = error.with_related_info(
                            "The closing parenthesis should be here".to_string(),
                            Some((must_be_rparen.span.clone(), "missing closing parenthesis")),
                        );

                        self.errors.push(error);
                        return Err(self.lexer.synchronize());
                    }
                }
            }
            TokenKind::IfKw => self.parse_if_expr(),
            _ => {
                let x = YuuError::unexpected_token(
                    t.span.clone(),
                    "a primary expression".to_string(),
                    format!("{:?}", t.kind),
                    self.lexer.code_info.code.clone(),
                    self.lexer.code_info.file_name.clone(),
                )
                .with_help("Expected a primary expression (number, identifier, etc.)".to_string());
                self.errors.push(x);
                return Err(self.lexer.synchronize());
            }
        }
    }

    fn make_bin_expr(
        &mut self,
        lhs: ExprNode,
        rhs: ExprNode,
        token: Token,
        span: Span,
    ) -> ParseResult<(Span, ExprNode)> {
        let bin_op = match token.kind {
            TokenKind::Plus => BinOp::Add,
            TokenKind::Minus => BinOp::Subtract,
            TokenKind::Asterix => BinOp::Multiply,
            TokenKind::Slash => BinOp::Divide,
            TokenKind::EqEq => BinOp::Eq,
            _ => {
                self.errors.push(
                    YuuError::builder()
                        .kind(ErrorKind::InvalidSyntax)
                        .message(format!(
                            "Expected a binary operator, found {:?}",
                            token.kind
                        ))
                        .source(
                            self.lexer.code_info.code.clone(),
                            self.lexer.code_info.file_name.clone(),
                        )
                        .span(token.span.clone(), "invalid binary operator")
                        .build(),
                );
                return Err(self.lexer.synchronize());
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

    pub fn parse_bin_expr(
        &mut self,
        lhs: ExprNode,
        op_token: Token,
        min_precedence: i32,
    ) -> ParseResult<(Span, ExprNode)> {
        let (span, rhs) = self.parse_expr_chain(min_precedence)?;
        let span = lhs.span().start..span.end;
        let lhs = self.make_bin_expr(lhs, rhs, op_token, span)?;
        Ok(lhs)
    }

    pub fn parse_condition_with_body(&mut self) -> ParseResult<(Span, ConditionWithBody)> {
        let (_, condition) = self.parse_expr_chain(0)?;
        let (block_span, block) = self.parse_block_expr()?;

        Ok((
            block_span,
            ConditionWithBody {
                condition: Box::new(condition),
                body: block,
            },
        ))
    }

    pub fn parse_assignment_expr(&mut self) -> ParseResult<(Span, ExprNode)> {
        let binding = self.parse_binding()?;
        let _ = self.lexer.expect(&[TokenKind::Equal], &mut self.errors)?;
        let (_, rhs) = self.parse_expr_chain(0)?;
        let span = binding.span().start..rhs.span().end;
        Ok((
            span.clone(),
            ExprNode::Assignment(AssignmentExpr {
                binding: Box::new(binding),
                rhs: Box::new(rhs),
                span,
                id: 0,
            }),
        ))
    }

    pub fn parse_if_expr(&mut self) -> ParseResult<(Span, ExprNode)> {
        let (body_span, cond_with_body) = self.parse_condition_with_body()?;
        let start_span = body_span;

        // Parse possible else if block -> peek for else if token

        let mut elifs = Vec::new();
        let mut last_body_span = start_span.clone();
        loop {
            let peeked_tokens = self.lexer.peek_n::<2>();
            if peeked_tokens[0].kind == TokenKind::ElseKw
                && peeked_tokens[1].kind == TokenKind::IfKw
            {
                let _ = self.lexer.next_token();
                let _ = self.lexer.next_token();
                let (body_span, elif_cond_body) = self.parse_condition_with_body()?;
                elifs.push(elif_cond_body);
                last_body_span = body_span;
            } else {
                break;
            }
        }

        let maybe_else = self.lexer.peek();

        let else_ = if maybe_else.kind == TokenKind::ElseKw {
            let _ = self.lexer.next_token();
            let (body_span, block) = self.parse_block_expr()?;
            last_body_span = body_span;
            Some(block)
        } else {
            None
        };

        let overall_span = start_span.start..last_body_span.end; // Use stored start span

        let if_expr = IfExpr {
            id: 0,
            span: overall_span.clone(),
            if_block: cond_with_body,
            else_if_blocks: elifs,
            else_block: else_,
        };

        Ok((overall_span, ExprNode::If(if_expr)))
    }

    pub fn parse_expr_chain(&mut self, min_precedence: i32) -> ParseResult<(Span, ExprNode)> {
        let mut lhs = self.parse_primary_expr()?;
        loop {
            let op = self.lexer.peek();

            let (left_precedence, right_precedence) = Self::get_infix_precedence(&op.kind);

            if left_precedence < min_precedence {
                return Ok(lhs);
            }
            let op = op.clone();
            self.lexer.eat();

            match op.kind {
                TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Asterix
                | TokenKind::Slash
                | TokenKind::EqEq => {
                    lhs = self.parse_bin_expr(lhs.1, op.clone(), right_precedence)?;
                }
                TokenKind::LParen => {
                    lhs = self.parse_func_call_expr(lhs.1, op.clone(), right_precedence)?;
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    pub fn parse_expr(&mut self) -> ParseResult<(Span, ExprNode)> {
        self.parse_expr_chain(0)
    }

    pub fn parse_type(&mut self) -> ParseResult<TypeNode> {
        let t = self.lexer.next_token();
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
                self.errors.push(
                    YuuError::unexpected_token(
                        t.span.clone(),
                        "a type".to_string(),
                        format!("{:?}", t.kind),
                        self.lexer.code_info.code.clone(),
                        self.lexer.code_info.file_name.clone(),
                    )
                    .with_help("Expected a type identifier".to_string()),
                );
                return Err(self.lexer.synchronize());
            }
        };
        Ok(out.1)
    }

    pub fn parse_atomic_stmt(&mut self) -> ParseResult<(Span, StmtNode)> {
        let (span, expr) = self.parse_expr()?;
        Ok((span, StmtNode::Atomic(expr)))
    }

    pub fn parse_binding(&mut self) -> ParseResult<BindingNode> {
        // Check if we have a mut keyword
        let mut_tkn = self.lexer.peek();
        let is_mut = if mut_tkn.kind == TokenKind::MutKw {
            let _ = self.lexer.next_token();
            true
        } else {
            false
        };

        let t = self.lexer.next_token();
        match t.kind {
            TokenKind::Ident(ident) => Ok(BindingNode::Ident(IdentBinding {
                span: t.span.clone(),
                name: ident,
                id: 0,
                is_mut,
            })),
            _ => {
                self.errors.push(
                    YuuError::unexpected_token(
                        t.span.clone(),
                        "an identifier".to_string(),
                        format!("{:?}", t.kind),
                        self.lexer.code_info.code.clone(),
                        self.lexer.code_info.file_name.clone(),
                    )
                    .with_help("Expected an identifier for the binding pattern".to_string()),
                );
                Err(self.lexer.synchronize())
            }
        }
    }

    pub fn parse_let_stmt(&mut self) -> ParseResult<(Span, StmtNode)> {
        let let_tkn = self.lexer.expect(&[TokenKind::LetKw], &mut self.errors)?;
        let binding = self.parse_binding()?;
        let la = self.lexer.peek();
        let ty = if la.kind == TokenKind::Colon {
            let _ = self.lexer.next_token();
            let out = self.parse_type()?;
            Some(out)
        } else {
            None
        };
        let _ = self.lexer.expect(&[TokenKind::Equal], &mut self.errors)?;
        let (expr_span, expr) = self.parse_expr()?;
        let span = let_tkn.span.start..expr_span.end;

        Ok((
            span.clone(),
            StmtNode::Let(LetStmt {
                span,
                binding: Box::new(binding),
                expr: Box::new(expr),
                ty,
                id: 0,
            }),
        ))
    }

    pub fn parse_func_arg(&mut self) -> ParseResult<FuncArg> {
        let pattern = self.parse_binding()?;
        let _ = self.lexer.expect(&[TokenKind::Colon], &mut self.errors)?;
        let ty = self.parse_type()?;
        let span = pattern.span().start..ty.span().end;
        Ok(FuncArg {
            span: span.clone(),
            binding: pattern,
            ty,
            id: 0,
        })
    }

    pub fn parse_func_args(&mut self) -> ParseResult<Vec<FuncArg>> {
        let mut args = Vec::new();
        loop {
            let la = self.lexer.peek();
            if la.kind == TokenKind::RParen {
                return Ok(args);
            }
            let arg = self.parse_func_arg()?;
            args.push(arg);
            let la = self.lexer.peek();
            if la.kind == TokenKind::Comma {
                let _ = self.lexer.next_token();
            } else if la.kind != TokenKind::RParen {
                self.errors.push(YuuError::unexpected_token(
                    la.span.clone(),
                    "a comma ',' or a closing parenthesis ')'".to_string(),
                    format!("{:?}", la.kind),
                    self.lexer.code_info.code.clone(),
                    self.lexer.code_info.file_name.clone(),
                ).with_help("Function arguments should be separated by commas, with the list ending with a closing parenthesis".to_string()));
                return Err(self.lexer.synchronize());
            }
        }
    }

    pub fn make_block_expr(
        &mut self,
        stmts: Vec<StmtNode>,
        span: Span,
        label: Option<String>,
        last_expr: Option<Box<ExprNode>>,
    ) -> (Span, BlockExpr) {
        (
            span.clone(),
            BlockExpr {
                id: 0,
                span,
                body: stmts,
                label,
                last_expr,
            },
        )
    }

    pub fn parse_func_call_expr(
        &mut self,
        lhs: ExprNode,
        op_token: Token,
        _min_precedence: i32,
    ) -> ParseResult<(Span, ExprNode)> {
        let mut args = Vec::new();
        // Track function name for better error messages
        let opening_paren_span = op_token.span.clone();
        let func_name = match &lhs {
            ExprNode::Ident(ident) => ident.ident.clone(),
            _ => "function".to_string(),
        };

        loop {
            let la = self.lexer.peek();
            if la.kind == TokenKind::RParen {
                let rparen = self.lexer.next_token();
                let span = lhs.span().start..rparen.span.end;
                let func_call = FuncCallExpr {
                    lhs: Box::new(lhs),
                    args,
                    span: span.clone(),
                    id: 0,
                };
                return Ok((span, ExprNode::FuncCall(func_call)));
            } else if la.kind == TokenKind::Comma {
                let _ = self.lexer.next_token();
            } else if la.kind == TokenKind::Semicolon || la.kind == TokenKind::EOF {
                // Special case: Likely missing closing parenthesis
                let mut error = YuuError::builder()
                    .kind(ErrorKind::MissingToken)
                    .message(format!(
                        "Expected a closing parenthesis for function call to '{}'",
                        func_name
                    ))
                    .source(
                        self.lexer.code_info.code.clone(),
                        self.lexer.code_info.file_name.clone(),
                    )
                    .span(la.span.clone(), format!("unexpected {:?}", la.kind))
                    // Add label for the opening parenthesis
                    .label(opening_paren_span, "opening parenthesis here");

                // Add label for the function name if it's an identifier
                if let ExprNode::Ident(ident) = &lhs {
                    error = error.label(ident.span.clone(), "function being called");
                }

                error = error.help(
                    format!("Function calls must be closed with a matching ')'. Add a closing parenthesis after the arguments to function '{}'", func_name)
                );

                self.errors.push(error.build());
                return Err(self.lexer.synchronize());
            }
            let (_, arg) = self.parse_expr_chain(0)?;
            args.push(arg);
        }
    }

    pub fn parse_block_expr(&mut self) -> ParseResult<(Span, BlockExpr)> {
        // Handle block label
        let label = if self.lexer.peek().kind == TokenKind::Colon {
            self.lexer.next_token(); // consume colon
            let label_token = self.lexer.next_token();
            match label_token.kind {
                TokenKind::Ident(label) => Some(label),
                _ => {
                    self.errors.push(YuuError::unexpected_token(
                        label_token.span.clone(),
                        "a label identifier".to_string(),
                        format!("{:?}", label_token.kind),
                        self.lexer.code_info.code.clone(),
                        self.lexer.code_info.file_name.clone(),
                    ).with_help("The preceding colon indicates a block label, so an identifier is expected".to_string()));
                    return Err(self.lexer.synchronize());
                }
            }
        } else {
            None
        };

        let lbrace = self.lexer.expect(&[TokenKind::LBrace], &mut self.errors)?;
        let mut stmts = Vec::new();
        let mut out_expr = None::<Box<ExprNode>>;

        loop {
            // Check for empty block or end of block
            let next = self.lexer.peek();
            if next.kind == TokenKind::RBrace {
                let rbrace = self.lexer.next_token();
                let span = lbrace.span.start..rbrace.span.end;
                return Ok(self.make_block_expr(stmts, span, label, out_expr));
            }

            // Parse statement or expression
            let stmt_res = self.parse_stmt();

            let (span, node) = match stmt_res {
                Ok((a, b)) => (a, b),
                Err(sw) if matches!(sw, CatchIn::Statement) => (0..0, StmtNode::Error(0)),
                Err(x) => return Err(x),
            };

            // Look for terminator
            let terminator = self.lexer.next_token();
            match terminator.kind {
                TokenKind::Semicolon => {
                    stmts.push(node);
                }
                TokenKind::Bang => {
                    // Must be followed by closing brace
                    let next = self.lexer.peek();
                    if next.kind != TokenKind::RBrace {
                        self.errors.push(
                            YuuError::unexpected_token(
                                next.span.clone(),
                                "a closing brace".to_string(),
                                format!("{:?}", next.kind),
                                self.lexer.code_info.code.clone(),
                                self.lexer.code_info.file_name.clone(),
                            )
                            .with_help("Expected a closing brace after '!'".to_string()),
                        );
                        return Err(self.lexer.synchronize());
                    }

                    if let StmtNode::Atomic(expr) = node {
                        out_expr = Some(Box::new(expr));
                        break;
                    } else {
                        self.errors.push(
                            YuuError::builder()
                                .kind(ErrorKind::InvalidSyntax)
                                .message("Expected an expression before '!'".to_string())
                                .source(
                                    self.lexer.code_info.code.clone(),
                                    self.lexer.code_info.file_name.clone(),
                                )
                                .span(span, "invalid expression")
                                .help("Only expressions can be used with the '!' terminator")
                                .build(),
                        );
                        return Err(self.lexer.synchronize());
                    }
                }
                _ => {
                    self.errors.push(
                        YuuError::unexpected_token(
                            terminator.span.clone(),
                            "';' or '!'".to_string(),
                            format!("{:?}", terminator.kind),
                            self.lexer.code_info.code.clone(),
                            self.lexer.code_info.file_name.clone(),
                        )
                        .with_help("Expected ';' or '!' after statement/expression".to_string()),
                    );
                    return Err(self.lexer.synchronize());
                }
            }
        }

        // Consume the closing brace after a '!' terminator
        let rbrace = self.lexer.next_token();
        let span = lbrace.span.start..rbrace.span.end;
        Ok(self.make_block_expr(stmts, span, label, out_expr))
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

    pub fn parse_func_decl(&mut self) -> ParseResult<(Span, FuncDeclStructural)> {
        let fn_tkn = self.lexer.expect(&[TokenKind::Fn], &mut self.errors)?;
        let ident = self.lexer.next_token();
        match ident.kind {
            TokenKind::Ident(ident) => {
                let _ = self.lexer.expect(&[TokenKind::LParen], &mut self.errors)?;
                let args = self.parse_func_args()?;
                let _rparen = self.lexer.expect(&[TokenKind::RParen], &mut self.errors)?;
                let _arrow = self.lexer.expect(&[TokenKind::Arrow], &mut self.errors)?;
                let ret_ty = Box::new(self.parse_type()?);
                let span = fn_tkn.span.start..ret_ty.span().end;
                Ok(self.make_func_decl(ident, span, args, Some(ret_ty)))
            }
            _ => {
                let mut error = YuuError::unexpected_token(
                    ident.span.clone(),
                    "an identifier".to_string(),
                    format!("{:?}", ident.kind),
                    self.lexer.code_info.code.clone(),
                    self.lexer.code_info.file_name.clone(),
                )
                .with_help("Expected an identifier naming the function".to_string());

                error = error.with_related_info(
                    "The fn keyword is here, indicating a function declaration".to_string(),
                    Some((fn_tkn.span.clone(), "function keyword")),
                );

                self.errors.push(error);
                Err(self.lexer.synchronize())
            }
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

    pub fn parse_func_def(&mut self) -> ParseResult<(Span, FuncDefStructural)> {
        let (span, decl) = self.parse_func_decl()?;
        let (block_span, mut block) = self.parse_block_expr()?; // TODO: write a parse_root_func_block - we don't want the root function blocks to have labels
        block.label = Some(FUNC_BLOCK_NAME.to_string()); // 🤡
        let span = span.start..block_span.end;
        Ok(self.make_func_def(span, decl, block))
    }

    pub fn parse_stmt(&mut self) -> ParseResult<(Span, StmtNode)> {
        let peek = self.lexer.peek();
        match peek.kind {
            TokenKind::LetKw => self.parse_let_stmt(),
            TokenKind::OutKw => self.parse_out_stmt(),
            TokenKind::Return => self.parse_return_stmt(),
            _ => self.parse_atomic_stmt(),
        }
    }

    fn parse_out_stmt(&mut self) -> ParseResult<(Span, StmtNode)> {
        let out_token = self.lexer.expect(&[TokenKind::OutKw], &mut self.errors)?;

        // Check for :
        let colon = self.lexer.expect(&[TokenKind::Colon], &mut self.errors)?;
        // Must be followed by identifier (i.e. :label)
        let label_token = self.lexer.next_token();
        let target = match label_token.kind {
            TokenKind::Ident(label) => label,
            _ => {
                let mut error = YuuError::unexpected_token(
                    label_token.span.clone(),
                    "a label identifier".to_string(),
                    format!("{:?}", label_token.kind),
                    self.lexer.code_info.code.clone(),
                    self.lexer.code_info.file_name.clone(),
                )
                .with_help("Expected a label identifier after colon".to_string());

                error = error.with_related_info(
                    "The preceding colon is here - this marks the start of a block label, so a block identifier is expected".to_string(),
                    Some((colon.span.clone(), "colon")),
                );

                self.errors.push(error);
                return Err(self.lexer.synchronize());
            }
        };

        // Parse the value expression
        let (expr_span, expr) = self.parse_expr()?;
        let span = out_token.span.start..expr_span.end;

        Ok((
            span.clone(),
            StmtNode::Break(BreakStmt {
                span,
                expr: Box::new(expr),
                target,
                id: 0,
            }),
        ))
    }

    fn parse_return_stmt(&mut self) -> ParseResult<(Span, StmtNode)> {
        let ret_token = self.lexer.expect(&[TokenKind::Return], &mut self.errors)?;

        let (expr_span, expr) = self.parse_expr()?;
        let span = ret_token.span.start..expr_span.end;

        Ok((
            span.clone(),
            StmtNode::Break(BreakStmt {
                span,
                expr: Box::new(expr),
                target: FUNC_BLOCK_NAME.to_string(),
                id: 0,
            }),
        ))
    }

    pub fn parse_structural(&mut self) -> ParseResult<(Span, StructuralNode)> {
        let t = self.lexer.peek();
        let x = match t.kind {
            TokenKind::Fn => {
                let res = self.parse_func_def(); // TODO: Here check CatchIn

                match res {
                    Ok((r, fds)) => Ok((r, StructuralNode::FuncDef(fds))),
                    Err(e) if matches!(e, CatchIn::FunctionDecl) => {
                        Ok((0..0, StructuralNode::Error(0)))
                    }
                    Err(x) => return Err(x),
                }
            }
            _ => {
                self.errors.push(
                    YuuError::unexpected_token(
                        t.span.clone(),
                        "fn".to_string(),
                        format!("{:?}", t.kind),
                        self.lexer.code_info.code.clone(),
                        self.lexer.code_info.file_name.clone(),
                    )
                    .with_help(
                        "Only function definitions are currently supported as structural nodes"
                            .to_string(),
                    ),
                );
                return Err(self.lexer.synchronize());
            }
        };
        x
    }

    pub fn parse(&mut self) -> AST {
        // Parse structural nodes as often as possible, until EOF
        let mut structural_nodes = Vec::new();
        loop {
            let t = self.lexer.peek();
            if t.kind == TokenKind::EOF {
                break;
            }
            structural_nodes.push(Box::new(
                self.parse_structural()
                    .expect("Critical Error: Couldn't recover from syntax error - goodbye world")
                    .1,
            ));
        }

        let ast = AST {
            structurals: structural_nodes,
        };

        ast
    }

    pub fn parse_and_add_ids(&mut self) -> AST {
        let mut ast = self.parse();
        add_ids(&mut ast);
        ast
    }
}

#[cfg(test)]
mod tests {

    use yuu_shared::graphviz_output;

    use crate::add_ids::{self};

    use super::*;

    #[test]
    fn test_parse_fac() {
        let code_info = UnprocessedCodeInfo {
            code: "fn fac(n: i64) -> i64 {
            if n == 0 {
                1!
            }
            else {
                let n_out = n * fac(n - 1);
                return n_out;
            }!
        }"
            .into(),
            file_name: "test.yuu".into(),
        };

        let mut parser = Parser::new(&code_info);
        let result = parser.parse();
        if !parser.errors.is_empty() {
            for error in parser.errors {
                println!("{}", error);
            }
        } else {
            let mut ast = result;
            add_ids(&mut ast);
        }
    }

    #[test]
    fn test_parse_failure_1() {
        yuu_shared::error::setup_error_formatter(Some("base16-ocean.dark"), true).unwrap();
        let x = UnprocessedCodeInfo {
            code: "fn fac(n: i64) -> i64 {
            if n == 0 {
                1!
            }
            else {
                let n_out = n * fac(n - 1;
                return n_out;
            }!
        }"
            .into(),
            file_name: "test.yuu".into(),
        };

        let mut parser = Parser::new(&x);
        let _ = parser.parse();
        for error in parser.errors {
            let error: miette::Report = error.into();
            println!("{:?}", error);
        }
    }
}
