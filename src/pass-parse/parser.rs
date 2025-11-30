use crate::{
    pass_diagnostics::{ErrorKind, YuuError},
    pass_parse::{
        add_ids::add_ids,
        lexer::{CatchIn, Lexer, ParseError, ParseResult},
    },
};
use crate::{
    pass_parse::ast::*,
    pass_parse::token::{Token, TokenKind},
    pass_yir_lowering::block::FUNC_BLOCK_NAME,
};
use logos::Span;
use ustr::{Ustr, ustr};

pub struct Parser {
    lexer: Lexer,
    errors: Vec<ParseError>,
}

impl Parser {
    pub fn new(code_info: &SourceInfo) -> Self {
        Self {
            lexer: Lexer::new(code_info),
            errors: Vec::new(),
        }
    }

    pub fn dismantle(self) -> (Vec<ParseError>, Lexer) {
        (self.errors, self.lexer)
    }
    fn get_infix_binding_power(op: &TokenKind) -> (i32, i32) {
        match op {
            TokenKind::LParen => (15, 0), // Function calls highest precedence, but no right-hand side
            TokenKind::Dot => (13, 14),   // Member access (left-associative: a.b.c = (a.b).c)
            TokenKind::MultiDeref(_) | TokenKind::DotAmpersand => (13, 0), // Postfix operators: no right-hand side
            TokenKind::At => (12, 11), // Pointer instantiation type@expr (right-associative for chaining: i64@ptr@addr)
            TokenKind::Asterix | TokenKind::Slash | TokenKind::Percent => (11, 12), // Multiplication/division/modulo (left-associative)
            TokenKind::Plus | TokenKind::Minus => (9, 10), // Addition/subtraction (left-associative)
            TokenKind::Lt | TokenKind::Gt | TokenKind::LtEq | TokenKind::GtEq => (7, 8), // Comparisons (left-associative)
            TokenKind::EqEq | TokenKind::NotEq => (5, 6), // Equality/Inequality (left-associative)
            TokenKind::Equal => (2, 1), // Assignment (right-associative: a = b = c means a = (b = c))
            _ => (-1, -1),
        }
    }

    fn get_prefix_precedence(op: &TokenKind) -> i32 {
        match op {
            TokenKind::Plus | TokenKind::Minus | TokenKind::Tilde => 13, // Unary operators bind tighter than * and +
            TokenKind::At => 13, // Heap allocation operator @expr
            _ => -1,
        }
    }

    fn make_unary_expr(
        &mut self,
        operand: ExprNode,
        op: Token,
        span: Span,
    ) -> ParseResult<(Span, ExprNode)> {
        match op.kind {
            TokenKind::At => {
                // Handle heap allocation: @expr
                let span_copy = span.clone();
                let heap_alloc = HeapAllocExpr {
                    value: Box::new(operand),
                    span,
                    id: 0,
                };
                Ok((span_copy, ExprNode::HeapAlloc(heap_alloc)))
            }
            TokenKind::Plus | TokenKind::Minus | TokenKind::Tilde => {
                let unary_op = match op.kind {
                    TokenKind::Plus => UnaryOp::Pos,
                    TokenKind::Minus => UnaryOp::Negate,
                    TokenKind::Tilde => UnaryOp::Free,
                    _ => unreachable!(),
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
            _ => {
                self.errors.push(
                    YuuError::builder()
                        .kind(ErrorKind::InvalidSyntax)
                        .message(format!("Expected a prefix operator, found {}", op.kind))
                        .source(
                            self.lexer.code_info.source.clone(),
                            self.lexer.code_info.file_name.clone(),
                        )
                        .span(span.clone(), "invalid prefix operator")
                        .build(),
                );
                Err(self.lexer.synchronize())
            }
        }
    }

    fn make_literal_expr(&mut self, lit: Token) -> (Span, ExprNode) {
        let span = lit.span.clone();
        let lit_expr = LiteralExpr { lit, id: 0 };
        (span, ExprNode::Literal(lit_expr))
    }

    fn make_ident_expr(&mut self, ident: Ustr, span: Span) -> (Span, ExprNode) {
        let span_clone = span.clone();
        let ident_expr = IdentExpr { ident, span, id: 0 };
        (span_clone, ExprNode::Ident(ident_expr))
    }

    // Inspired by: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    fn parse_primary_expr(&mut self) -> ParseResult<(Span, ExprNode)> {
        let peek = self.lexer.peek();

        match &peek.kind {
            TokenKind::F32(_) | TokenKind::F64(_) | TokenKind::Integer(_) | TokenKind::NilKw => {
                let t = self.lexer.next_token(); // Consume the token
                Ok(self.make_literal_expr(t))
            }


            TokenKind::Ident(_) => {
                // Check if this identifier is followed by a double colon (enum instantiation)
                let peek_next = self.lexer.peek_at(1);
                if peek_next.kind == TokenKind::DoubleColon {
                    // This is an enum instantiation expression
                    return self.parse_enum_instantiation_expr();
                }

                // Check if this identifier is followed by a left brace (struct instantiation)
                if peek_next.kind == TokenKind::LBrace {
                    // This is a struct instantiation expression
                    return self.parse_struct_instantiation_expr();
                }

                // Regular identifier
                let t = self.lexer.next_token();
                match t.kind {
                    TokenKind::Ident(ident) => Ok(self.make_ident_expr(ident, t.span)),
                    _ => unreachable!("Token kind should match the peeked token"),
                }
            }

            TokenKind::Plus | TokenKind::Minus | TokenKind::At => {
                let t = self.lexer.next_token();
                let prefix_precedence = Self::get_prefix_precedence(&t.kind);
                let (span, operand) = self.parse_expr_chain(prefix_precedence)?;
                let span = t.span.start..span.end;
                let unary = self.make_unary_expr(operand, t, span)?;
                Ok(unary)
            }
            TokenKind::LBracket => {
                // Array expression: [init_value:type; count]
                self.parse_array_expr()
            }
            TokenKind::LParen => {
                let t = self.lexer.next_token();
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
                            must_be_rparen.kind,
                            self.lexer.code_info.source.clone(),
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
                        Err(self.lexer.synchronize())
                    }
                }
            }
            _ => {
                let t = self.lexer.next_token();
                let x = YuuError::unexpected_token(
                    t.span.clone(),
                    "a primary expression".to_string(),
                    t.kind,
                    self.lexer.code_info.source.clone(),
                    self.lexer.code_info.file_name.clone(),
                )
                .with_help("Expected a primary expression (number, identifier, etc.)".to_string());
                self.errors.push(x);
                Err(self.lexer.synchronize())
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
            TokenKind::Percent => BinOp::Modulo,
            TokenKind::EqEq => BinOp::Eq,
            TokenKind::NotEq => BinOp::NotEq,
            TokenKind::Lt => BinOp::Lt,
            TokenKind::Gt => BinOp::Gt,
            TokenKind::LtEq => BinOp::LtEq,
            TokenKind::GtEq => BinOp::GtEq,
            _ => {
                self.errors.push(
                    YuuError::builder()
                        .kind(ErrorKind::InvalidSyntax)
                        .message(format!("Expected a binary operator, found {}", token.kind))
                        .source(
                            self.lexer.code_info.source.clone(),
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
        min_binding_power: i32,
    ) -> ParseResult<(Span, ExprNode)> {
        let (span, rhs) = self.parse_expr_chain(min_binding_power)?;
        let span = lhs.span().start..span.end;
        let lhs = self.make_bin_expr(lhs, rhs, op_token, span)?;
        Ok(lhs)
    }

    pub fn parse_condition_with_body(&mut self) -> ParseResult<(Span, ConditionWithBody)> {
        let (_, condition) = self.parse_expr_chain(0)?;
        let (block_span, block) = self.parse_block_stmt_inner()?;

        Ok((
            condition.span().start..block_span.end,
            ConditionWithBody {
                condition: Box::new(condition),
                body: block,
            },
        ))
    }
    pub fn parse_assignment_expr(
        &mut self,
        lhs: ExprNode,
        _op_token: Token,
        min_binding_power: i32,
    ) -> ParseResult<(Span, ExprNode)> {
        // Determine the proper LValueKind based on the LHS expression structure
        let lvalue_kind = match &lhs {
            ExprNode::Ident(_) => LValueKind::Variable,
            ExprNode::MemberAccess(_) => LValueKind::FieldAccess,
            ExprNode::Deref(_) => LValueKind::Dereference,
            _ => {
                // Invalid assignment target - will be caught during type inference
                // For now, default to Variable to avoid Unknown
                LValueKind::Variable
            }
        };

        let (rhs_span, rhs) = self.parse_expr_chain(min_binding_power)?;
        let span = lhs.span().start..rhs_span.end;
        Ok((
            span.clone(),
            ExprNode::Assignment(AssignmentExpr {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                span,
                id: 0,
                lvalue_kind,
            }),
        ))
    }

    pub fn parse_while_stmt(&mut self) -> ParseResult<(Span, StmtNode)> {
        let _ = self.lexer.expect(&[TokenKind::WhileKw], &mut self.errors)?;
        let (overall_span, cond_with_body) = self.parse_condition_with_body()?;
        Ok((
            overall_span.clone(),
            StmtNode::While(WhileStmt {
                id: 0,
                span: overall_span,
                condition_block: cond_with_body,
            }),
        ))
    }

    pub fn parse_block_stmt(&mut self) -> ParseResult<(Span, StmtNode)> {
        let (span, block_stmt) = self.parse_block_stmt_inner()?;
        Ok((
            span.clone(),
            StmtNode::Block(BlockStmt {
                id: 0,
                span: span.clone(),
                body: block_stmt.body,
                label: block_stmt.label,
            }),
        ))
    }

    pub fn parse_if_stmt(&mut self) -> ParseResult<(Span, StmtNode)> {
        let _ = self.lexer.expect(&[TokenKind::IfKw], &mut self.errors)?;
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
            let (body_span, block) = self.parse_block_stmt_inner()?;
            last_body_span = body_span;
            Some(block)
        } else {
            None
        };

        let overall_span = start_span.start..last_body_span.end; // Use stored start span

        let if_stmt = IfStmt {
            id: 0,
            span: overall_span.clone(),
            if_block: cond_with_body,
            else_if_blocks: elifs,
            else_block: else_,
        };

        Ok((overall_span, StmtNode::If(if_stmt)))
    }

    // What if member is a function pointer? Then transforming as we do it here, won't work. The function pointer call has to be resolved explicitly through "a.*b()" syntax...
    pub fn parse_member_access_expr(
        &mut self,
        lhs: ExprNode,
        _op_token: Token,
        min_binding_power: i32,
    ) -> ParseResult<(Span, ExprNode)> {
        // We parse the rhs, transforming func calls like a.b.c() into c(a.b) (unified call syntax)
        // If not a function call, we expect a identifier expression (otherwise func call expression!).

        // Parse the right-hand side using parse_expr_chain (similar to parse_bin_expr)
        let (rhs_span, rhs) = self.parse_expr_chain(min_binding_power)?;

        // Match on the result - only Ident and FuncCall are allowed
        match rhs {
            ExprNode::Ident(ident_expr) => {
                // Regular member access: a.b
                let span = lhs.span().start..rhs_span.end;
                Ok((
                    span.clone(),
                    ExprNode::MemberAccess(MemberAccessExpr {
                        lhs: Box::new(lhs),
                        field: Field {
                            name: ident_expr.ident,
                            span: ident_expr.span,
                        },
                        span,
                        id: 0,
                    }),
                ))
            }
            ExprNode::FuncCall(mut func_call) => {
                // Method call using unified call syntax: a.b.c() -> c(a.b, ...)
                // Insert the original lhs as the first argument
                let span = lhs.span().start..rhs_span.end;
                func_call.args.insert(0, lhs);
                Ok((span, ExprNode::FuncCall(func_call)))
            }
            _ => {
                // Invalid right-hand side for member access
                self.errors.push(
                    YuuError::builder()
                        .kind(ErrorKind::InvalidSyntax)
                        .message(
                            "Invalid member access: expected identifier or function call"
                                .to_string(),
                        )
                        .source(
                            self.lexer.code_info.source.clone(),
                            self.lexer.code_info.file_name.clone(),
                        )
                        .span(rhs_span, "expected identifier or function call here")
                        .help(
                            "Member access must be either 'a.field' or 'a.method(args)'"
                                .to_string(),
                        )
                        .build(),
                );
                Err(self.lexer.synchronize())
            }
        }
    }
    pub fn parse_expr_chain(&mut self, min_binding_power: i32) -> ParseResult<(Span, ExprNode)> {
        let mut lhs = self.parse_primary_expr()?;
        loop {
            let op = self.lexer.peek();

            let (left_binding_power, right_binding_power) = Self::get_infix_binding_power(&op.kind);

            // It kinda looks like this now:
            /*
            <lhs> lbp_<op>_rbp <rhs> lbp2_<op>_rbp2 <rhs2> ...; now the question is: does "lbp" bind the lhs tighter than "min_binding_power" - basically the binding power that the above chain had?
            If it does, we continue parsing our chain, otherwise we return the lhs as we should not bind it to our current chain... the above chain binds it more tightly.
            Of course "<rhs>" and <rhs2>, ... are expressions themselves, so we have to recursively parse them as well. The binding power that they have to check is then obviously the "right_binding_power" of the operator that we are currently parsing.
            ==> The returned value here marks our new "lhs" for the next iteration of the loop.
             */
            if left_binding_power < min_binding_power {
                return Ok(lhs);
            }
            let op = op.clone();
            self.lexer.eat(); // Consume the operator token

            match op.kind {
                TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Asterix
                | TokenKind::Slash
                | TokenKind::Percent
                | TokenKind::Lt
                | TokenKind::Gt
                | TokenKind::NotEq
                | TokenKind::LtEq
                | TokenKind::GtEq
                | TokenKind::EqEq => {
                    lhs = self.parse_bin_expr(lhs.1, op.clone(), right_binding_power)?;
                }
                TokenKind::At => {
                    lhs = self.parse_pointer_instantiation_expr_infix(lhs.1, op.clone(), right_binding_power)?;
                }
                TokenKind::Equal => {
                    lhs = self.parse_assignment_expr(lhs.1, op.clone(), right_binding_power)?;
                }
                TokenKind::LParen => {
                    lhs = self.parse_func_call_expr(lhs.1, op.clone(), right_binding_power)?;
                }
                TokenKind::Dot => {
                    lhs = self.parse_member_access_expr(lhs.1, op.clone(), right_binding_power)?;
                }
                TokenKind::MultiDeref(count) => {
                    lhs = self.parse_multi_deref_expr(lhs.1, op.clone(), count)?;
                }
                TokenKind::DotAmpersand => {
                    lhs = self.parse_address_of_expr(lhs.1, op.clone())?;
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    pub fn parse_deref_expr(
        &mut self,
        operand: ExprNode,
        op_token: Token,
    ) -> ParseResult<(Span, ExprNode)> {
        let span = operand.span().start..op_token.span.end;
        let deref_expr = DerefExpr {
            operand: Box::new(operand),
            span: span.clone(),
            id: 0,
        };
        Ok((span, ExprNode::Deref(deref_expr)))
    }

    pub fn parse_multi_deref_expr(
        &mut self,
        operand: ExprNode,
        op_token: Token,
        count: usize,
    ) -> ParseResult<(Span, ExprNode)> {
        let span = operand.span().start..op_token.span.end;

        // Create nested DerefExpr nodes
        let mut current_expr = operand;
        for _ in 0..count {
            current_expr = ExprNode::Deref(DerefExpr {
                operand: Box::new(current_expr),
                span: span.clone(),
                id: 0,
            });
        }

        Ok((span, current_expr))
    }

    pub fn parse_address_of_expr(
        &mut self,
        operand: ExprNode,
        op_token: Token,
    ) -> ParseResult<(Span, ExprNode)> {
        let span = operand.span().start..op_token.span.end;
        let address_of_expr = AddressOfExpr {
            operand: Box::new(operand),
            span: span.clone(),
            id: 0,
        };
        Ok((span, ExprNode::AddressOf(address_of_expr)))
    }

    pub fn parse_expr(&mut self) -> ParseResult<(Span, ExprNode)> {
        self.parse_expr_chain(0)
    }

    pub fn parse_type(&mut self) -> ParseResult<TypeNode> {
        let t = self.lexer.next_token();
        let out = match t.kind {
            TokenKind::Asterix => {
                // Parse pointer type: *T
                let pointee = Box::new(self.parse_type()?);
                let span = t.span.start..pointee.span().end;
                (
                    span.clone(),
                    TypeNode::Pointer(PointerType {
                        id: 0,
                        span,
                        pointee,
                    }),
                )
            }
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
                        t.kind,
                        self.lexer.code_info.source.clone(),
                        self.lexer.code_info.file_name.clone(),
                    )
                    .with_help("Expected a type identifier".to_string()),
                );
                return Err(self.lexer.synchronize());
            }
        };
        Ok(out.1)
    }

    fn expect_semicolon_or_block_terminator(
        &mut self,
        start_span: Span,
        end_span: Span,
    ) -> ParseResult<Span> {
        let dot_maybe = self.lexer.peek();
        if dot_maybe.kind == TokenKind::BlockTerminator {
            Ok(start_span.start..end_span.end)
        } else {
            let semicolon = self.lexer.expect_semicolon(&mut self.errors)?;
            Ok(start_span.start..semicolon.span.end)
        }
    }

    pub fn parse_atomic_stmt(&mut self) -> ParseResult<(Span, StmtNode)> {
        let (span, expr) = self.parse_expr()?;
        let final_span = self.expect_semicolon_or_block_terminator(span.clone(), span.clone())?;
        Ok((final_span, StmtNode::Atomic(expr)))
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
                        t.kind,
                        self.lexer.code_info.source.clone(),
                        self.lexer.code_info.file_name.clone(),
                    )
                    .with_help("Expected an identifier denoting a binding".to_string()),
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

        let span = self.expect_semicolon_or_block_terminator(let_tkn.span.clone(), expr_span)?;

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

    pub fn parse_func_arg(&mut self) -> ParseResult<Arg> {
        let tkn = self.lexer.next_token();
        let (name, is_mut, start_span) = match tkn.kind {
            TokenKind::MutKw => {
                let ident = self.lexer.next_token();
                match ident.kind {
                    TokenKind::Ident(name) => (name, true, tkn.span),
                    _ => {
                        let mut error = YuuError::unexpected_token(
                            ident.span.clone(),
                            "an identifier".to_string(),
                            ident.kind,
                            self.lexer.code_info.source.clone(),
                            self.lexer.code_info.file_name.clone(),
                        );
                        error = error
                            .with_help("Expected an identifier after 'mut' keyword".to_string());
                        self.errors.push(error);
                        return Err(self.lexer.synchronize());
                    }
                }
            }
            TokenKind::Ident(name) => (name, false, tkn.span),
            _ => {
                let mut error = YuuError::unexpected_token(
                    tkn.span.clone(),
                    "an identifier or 'mut'".to_string(),
                    tkn.kind,
                    self.lexer.code_info.source.clone(),
                    self.lexer.code_info.file_name.clone(),
                );
                error = error
                    .with_help("Expected 'mut' or an identifier naming the argument".to_string());
                self.errors.push(error);
                return Err(self.lexer.synchronize());
            }
        };

        let _ = self.lexer.expect_colon(&mut self.errors)?;
        let ty = self.parse_type()?;
        let span = start_span.start..ty.span().end;
        Ok(Arg {
            span: span.clone(),
            name,
            ty,
            id: 0,
            is_mut,
        })
    }

    pub fn parse_struct_fields(&mut self) -> ParseResult<Vec<Arg>> {
        let mut fields = Vec::new();
        loop {
            let la = self.lexer.peek();
            if la.kind == TokenKind::BlockTerminator {
                return Ok(fields);
            }
            let arg = self.parse_func_arg()?;
            fields.push(arg);
            let la = self.lexer.peek();
            if la.kind == TokenKind::Comma {
                let _ = self.lexer.next_token();
            } else if la.kind != TokenKind::BlockTerminator {
                self.errors.push(YuuError::unexpected_token(
                    la.span.clone(),
                    "a comma ',' or a block terminator '.'".to_string(),
                    la.kind,
                    self.lexer.code_info.source.clone(),
                    self.lexer.code_info.file_name.clone(),
                ).with_help("Struct fields should be separated by commas, with finalized by a block terminator '.'".to_string()));
                return Err(self.lexer.synchronize());
            }
        }
    }

    pub fn parse_func_args(&mut self) -> ParseResult<Vec<Arg>> {
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
                    la.kind,
                    self.lexer.code_info.source.clone(),
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
        label: Option<Ustr>,
    ) -> (Span, BlockStmt) {
        (
            span.clone(),
            BlockStmt {
                id: 0,
                span,
                body: stmts,
                label,
            },
        )
    }
    pub fn parse_func_call_expr(
        &mut self,
        lhs: ExprNode,
        op_token: Token,
        _min_binding_power: i32,
    ) -> ParseResult<(Span, ExprNode)> {
        let mut args = Vec::new();
        // Track function name for better error messages
        let opening_paren_span = op_token.span.clone();
        let func_name = match &lhs {
            ExprNode::Ident(ident) => ident.ident,
            _ => ustr("function"),
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
                        self.lexer.code_info.source.clone(),
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

    pub fn parse_struct_instantiation_expr(&mut self) -> ParseResult<(Span, ExprNode)> {
        // Parse the struct name identifier
        let ident = self.lexer.next_token();
        match ident.kind {
            TokenKind::Ident(name) => {
                // Expect the opening brace '{'
                let _l_brace = self.lexer.expect(&[TokenKind::LBrace], &mut self.errors)?;
                let mut fields = Vec::new();

                // Parse fields until we hit a closing brace
                loop {
                    // Check if we're at the closing brace
                    let next = self.lexer.peek();
                    if next.kind == TokenKind::RBrace {
                        // Consume the closing brace and return struct instantiation
                        let r_brace = self.lexer.next_token();
                        let span = ident.span.start..r_brace.span.end;
                        return Ok((
                            span.clone(),
                            ExprNode::StructInstantiation(StructInstantiationExpr {
                                struct_name: name,
                                fields,
                                span,
                                id: 0,
                            }),
                        ));
                    }

                    // Parse field name
                    let field_ident = self.lexer.next_token();
                    match field_ident.kind {
                        TokenKind::Ident(field_name) => {
                            // Expect colon between field name and value
                            let _ = self.lexer.expect_colon(&mut self.errors)?;

                            // Parse the field value expression
                            let (expr_span, expr) = self.parse_expr()?;
                            let field_span = field_ident.span.start..expr_span.end;
                            let field = Field {
                                name: field_name,
                                span: field_span.clone(),
                            };

                            // Add the field to our collection
                            fields.push((field, expr));

                            // Check what follows: comma or closing brace
                            let la = self.lexer.peek();
                            match la.kind {
                                TokenKind::Comma => {
                                    // Consume the comma and continue parsing fields
                                    let _ = self.lexer.next_token();

                                    // Allow trailing comma - peek ahead to see if next token is RBrace
                                    let peek_after_comma = self.lexer.peek();
                                    if peek_after_comma.kind == TokenKind::RBrace {
                                        // We have a trailing comma followed by a closing brace, so break out
                                        continue;
                                    }
                                }
                                TokenKind::RBrace => {
                                    // We'll handle this in the next loop iteration
                                }
                                _ => {
                                    self.errors.push(
                                        YuuError::unexpected_token(
                                            la.span.clone(),
                                            "a comma ',' or a closing brace '}'".to_string(),
                                            la.kind,
                                            self.lexer.code_info.source.clone(),
                                            self.lexer.code_info.file_name.clone(),
                                        )
                                        .with_help("Struct fields should be separated by commas and the list must end with a closing brace '}'".to_string()),
                                    );
                                    return Err(self.lexer.synchronize());
                                }
                            }
                        }
                        _ => {
                            self.errors.push(
                                YuuError::unexpected_token(
                                    field_ident.span.clone(),
                                    "a field identifier".to_string(),
                                    field_ident.kind,
                                    self.lexer.code_info.source.clone(),
                                    self.lexer.code_info.file_name.clone(),
                                )
                                .with_help("Expected a field name identifier".to_string()),
                            );
                            return Err(self.lexer.synchronize());
                        }
                    }
                }
            }
            _ => {
                self.errors.push(
                    YuuError::unexpected_token(
                        ident.span.clone(),
                        "a struct name identifier".to_string(),
                        ident.kind,
                        self.lexer.code_info.source.clone(),
                        self.lexer.code_info.file_name.clone(),
                    )
                    .with_help("Expected a struct name identifier".to_string()),
                );
                Err(self.lexer.synchronize())
            }
        }
    }

    pub fn parse_pointer_instantiation_expr(&mut self) -> ParseResult<(Span, ExprNode)> {
        // Parse the type name (can be identifier or keyword)
        let type_token = self.lexer.next_token();
        let type_name = match type_token.kind {
            TokenKind::Ident(name) => name,
            TokenKind::NilKw => ustr::ustr("nil"),
            _ => {
                self.errors.push(
                    YuuError::unexpected_token(
                        type_token.span.clone(),
                        "a type name".to_string(),
                        type_token.kind,
                        self.lexer.code_info.source.clone(),
                        self.lexer.code_info.file_name.clone(),
                    )
                    .with_help("Expected a type name identifier for pointer instantiation".to_string()),
                );
                return Err(self.lexer.synchronize());
            }
        };

        // Expect the @ token
        let _at_token = self.lexer.expect(&[TokenKind::At], &mut self.errors)?;

        // Parse the address expression
        let (_, address_expr) = self.parse_expr()?;

        let span = type_token.span.start..address_expr.span().end;
        Ok((
            span.clone(),
            ExprNode::PointerInstantiation(PointerInstantiationExpr {
                id: 0,
                span,
                type_name,
                address: Box::new(address_expr),
            }),
        ))
    }

    pub fn parse_array_expr(&mut self) -> ParseResult<(Span, ExprNode)> {
        use crate::pass_parse::ast::{ArrayExpr, ExprNode};

        let lbracket_token = self.lexer.expect(&[TokenKind::LBracket], &mut self.errors)?;

        let (init_value, element_type) = if self.lexer.peek().kind == TokenKind::Colon {
            // Pattern: [:type; count] - uninitialized
            self.lexer.next_token(); // consume ':'
            let type_node = self.parse_type()?;
            (None, Some(Box::new(type_node)))
        } else {
            // Parse expression first
            let (_, first_expr) = self.parse_expr()?;
            if self.lexer.peek().kind == TokenKind::Colon {
                // Pattern: [value:type; count] - initialized with explicit type
                self.lexer.next_token(); // consume ':'
                let type_node = self.parse_type()?;
                (Some(Box::new(first_expr)), Some(Box::new(type_node)))
            } else {
                // Pattern: [value; count] - initialized with inferred type
                (Some(Box::new(first_expr)), None)
            }
        };

        // Expect semicolon
        self.lexer.expect(&[TokenKind::Semicolon], &mut self.errors)?;

        // Parse size expression
        let (_, size_expr) = self.parse_expr()?;

        let rbracket_token = self.lexer.expect(&[TokenKind::RBracket], &mut self.errors)?;

        let span = lbracket_token.span.start..rbracket_token.span.end;
        let array_expr = ArrayExpr {
            init_value,
            element_type,
            size: Box::new(size_expr),
            span: span.clone(),
            id: 0,
        };

        Ok((span, ExprNode::Array(array_expr)))
    }

    pub fn parse_pointer_instantiation_expr_infix(
        &mut self,
        lhs: ExprNode,
        _op: Token,
        right_binding_power: i32,
    ) -> ParseResult<(Span, ExprNode)> {
        // lhs is the type expression (like an identifier for a type name)
        let type_name = match &lhs {
            ExprNode::Ident(ident_expr) => ident_expr.ident,
            _ => {
                self.errors.push(
                    YuuError::builder()
                        .kind(ErrorKind::InvalidSyntax)
                        .message("Left side of @ must be a type name".to_string())
                        .source(
                            self.lexer.code_info.source.clone(),
                            self.lexer.code_info.file_name.clone(),
                        )
                        .span(lhs.span().clone(), "not a valid type name")
                        .build(),
                );
                return Err(self.lexer.synchronize());
            }
        };

        // Parse the address expression with the right binding power
        let (_, address_expr) = self.parse_expr_chain(right_binding_power)?;

        let span = lhs.span().start..address_expr.span().end;
        Ok((
            span.clone(),
            ExprNode::PointerInstantiation(PointerInstantiationExpr {
                id: 0,
                span,
                type_name,
                address: Box::new(address_expr),
            }),
        ))
    }

    pub fn parse_enum_instantiation_expr(&mut self) -> ParseResult<(Span, ExprNode)> {
        // First token should be the enum name (identifier)
        let enum_token = self.lexer.next_token();
        let enum_name = match enum_token.kind {
            TokenKind::Ident(name) => name,
            _ => unreachable!("Should only be called when we know we have an identifier"),
        };

        // Expect double colon
        let _ = self
            .lexer
            .expect(&[TokenKind::DoubleColon], &mut self.errors)?;

        // Get variant name
        let variant_token = self.lexer.next_token();
        let variant_name = match variant_token.kind {
            TokenKind::Ident(name) => name,
            _ => {
                self.errors.push(
                    YuuError::unexpected_token(
                        variant_token.span.clone(),
                        "an identifier".to_string(),
                        variant_token.kind,
                        self.lexer.code_info.source.clone(),
                        self.lexer.code_info.file_name.clone(),
                    )
                    .with_help("Expected a variant name identifier after '::'".to_string()),
                );
                return Err(self.lexer.synchronize());
            }
        };

        // Check for optional data expression in parentheses
        let (data, end_span) = if self.lexer.peek().kind == TokenKind::LParen {
            let _ = self.lexer.next_token(); // consume '('
            let (_, expr) = self.parse_expr()?;
            let rparen = self.lexer.expect(&[TokenKind::RParen], &mut self.errors)?;
            (Some(Box::new(expr)), rparen.span.clone())
        } else {
            (None, variant_token.span.clone())
        };

        let span = enum_token.span.start..end_span.end;
        let enum_instantiation = EnumInstantiationExpr {
            id: 0,
            span: span.clone(),
            enum_name,
            variant_name,
            data,
        };

        Ok((span, ExprNode::EnumInstantiation(enum_instantiation)))
    }

    pub fn parse_block_stmt_inner(&mut self) -> ParseResult<(Span, BlockStmt)> {
        let colon = self.lexer.expect(&[TokenKind::Colon], &mut self.errors)?;
        let mut stmts = Vec::new();

        loop {
            // Check for empty block or end of block
            let next = self.lexer.peek();
            if next.kind == TokenKind::BlockTerminator {
                let bt = self.lexer.next_token();

                let span = colon.span.start..bt.span.end;

                return Ok(self.make_block_expr(stmts, span, None));
            }

            let stmt_res = self.parse_stmt();

            let node = match stmt_res {
                Ok((_, b)) => b,
                Err(CatchIn::Statement) => {
                    // Eat the ";"
                    self.lexer.eat();
                    StmtNode::Error(0)
                }
                Err(CatchIn::BlockTerminator) => {
                    // Hit a block terminator while parsing statements
                    // This means we've recovered to a block boundary, continue to the next iteration
                    // which will detect the BlockTerminator and handle it properly
                    continue;
                }
                Err(x) => return Err(x),
            };

            stmts.push(node);
        }
    }

    pub fn parse_refutable_pattern(&mut self) -> ParseResult<(Span, RefutablePatternNode)> {
        let peek = self.lexer.peek();

        match peek.kind {
            TokenKind::Ident(name_discriminator) => {
                let token = self.lexer.next_token(); // consume identifier
                let span = token.span.clone();
                let _ = self
                    .lexer
                    .expect(&[TokenKind::DoubleColon], &mut self.errors)?;
                let discriminee = self.lexer.next_token();
                let name_discriminee = match discriminee.kind {
                    TokenKind::Ident(name_discriminee) => name_discriminee,
                    _ => {
                        self.errors.push(
                            YuuError::unexpected_token(
                                discriminee.span.clone(),
                                "an identifier".to_string(),
                                discriminee.kind,
                                self.lexer.code_info.source.clone(),
                                self.lexer.code_info.file_name.clone(),
                            )
                            .with_help("Expected a variant name identifier after '::'".to_string()),
                        );
                        return Err(self.lexer.synchronize());
                    }
                };

                let peeked = self.lexer.peek();
                match peeked.kind {
                    TokenKind::LParen => {
                        // We have data associated with the binding
                        let _ = self.lexer.next_token();
                        let binding = self.parse_binding()?;
                        let last_tkn = self.lexer.expect(&[TokenKind::RParen], &mut self.errors)?;
                        let full_span = span.start..last_tkn.span.end;
                        let enum_pattern = EnumPattern {
                            enum_name: name_discriminator,
                            variant_name: name_discriminee,
                            binding: Some(Box::new(binding)),
                            span: full_span.clone(),
                            id: 0,
                        };
                        let node = RefutablePatternNode::Enum(enum_pattern);
                        Ok((full_span, node))
                    }
                    _ => {
                        // We only have a unit variant
                        let final_span = span.start..discriminee.span.end;
                        let enum_pattern = EnumPattern {
                            enum_name: name_discriminator,
                            variant_name: name_discriminee,
                            binding: None,
                            span: final_span.clone(),
                            id: 0,
                        };
                        let node = RefutablePatternNode::Enum(enum_pattern);
                        Ok((final_span, node))
                    }
                }
            }
            _ => {
                self.errors.push(
                    YuuError::unexpected_token(
                        peek.span.clone(),
                        "a pattern".to_string(),
                        peek.kind,
                        self.lexer.code_info.source.clone(),
                        self.lexer.code_info.file_name.clone(),
                    )
                    .with_help(
                        "Expected a refutable pattern (currently only enum patterns are supported)"
                            .to_string(),
                    ),
                );
                Err(self.lexer.synchronize())
            }
        }
    }

    pub fn parse_match_stmt(&mut self) -> ParseResult<(Span, StmtNode)> {
        let match_start = self.lexer.expect(&[TokenKind::MatchKw], &mut self.errors)?;
        let scrutinee = self.parse_expr()?;
        let _ = self.lexer.expect_colon(&mut self.errors)?;

        // Here, we loop over the body, picking up case for case.
        let mut arms = Vec::new();
        loop {
            // Check if we should stop parsing cases
            let peek = self.lexer.peek();
            if peek.kind == TokenKind::DefaultKw || peek.kind == TokenKind::BlockTerminator {
                break;
            }

            // Pattern parsing
            let pattern = self.parse_refutable_pattern()?;

            let block = self.parse_block_stmt_inner()?;
            let match_arm = MatchArm {
                body: block.1,
                id: 0,
                pattern: Box::new(pattern.1),
                span: pattern.0.start..block.0.end,
            };

            arms.push(match_arm);
        }

        // Check for optional default case
        let default_case = if self.lexer.peek().kind == TokenKind::DefaultKw {
            let _ = self.lexer.next_token(); // consume "default"
            let block = self.parse_block_stmt_inner()?;
            Some(block.1)
        } else {
            None
        };

        // Now we expect a block terminator:
        let bt = self
            .lexer
            .expect(&[TokenKind::BlockTerminator], &mut self.errors)?;

        let match_stmt = MatchStmt {
            id: 0,
            span: (match_start.span.start..bt.span.end),
            scrutinee: Box::new(scrutinee.1),
            arms,
            default_case,
        };

        Ok((
            (match_start.span.start..bt.span.end),
            StmtNode::Match(match_stmt),
        ))
    }

    pub fn parse_break_stmt(&mut self) -> ParseResult<(Span, StmtNode)> {
        let break_token = self.lexer.expect(&[TokenKind::BreakKw], &mut self.errors)?;

        let span = self.expect_semicolon_or_block_terminator(
            break_token.span.clone(),
            break_token.span.clone(),
        )?;

        Ok((span.clone(), StmtNode::Break(BreakStmt { span, id: 0 })))
    }

    pub fn parse_func_decl(&mut self) -> ParseResult<(Span, FuncDeclStructural)> {
        let fn_tkn = self.lexer.expect(&[TokenKind::FnKw], &mut self.errors)?;
        let ident = self.lexer.next_token();
        match ident.kind {
            TokenKind::Ident(ident) => {
                let _ = self.lexer.expect(&[TokenKind::LParen], &mut self.errors)?;
                let args = self.parse_func_args()?;
                let _rparen = self.lexer.expect(&[TokenKind::RParen], &mut self.errors)?;

                let arrow_maybe = self.lexer.peek();

                let (span, ret_ty) = if let TokenKind::Arrow = arrow_maybe.kind {
                    let _ = self.lexer.next_token(); // Consume arrow
                    let ret_ty = Box::new(self.parse_type()?);
                    let span = fn_tkn.span.start..ret_ty.span().end;
                    (span, Some(ret_ty))
                } else {
                    let span = fn_tkn.span.start.._rparen.span.end;
                    (span, None)
                };

                Ok(self.make_func_decl(ident, span, args, ret_ty))
            }
            _ => {
                let mut error = YuuError::unexpected_token(
                    ident.span.clone(),
                    "an identifier".to_string(),
                    ident.kind,
                    self.lexer.code_info.source.clone(),
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

    pub fn make_func_decl(
        &mut self,
        name: Ustr,
        span: Span,
        args: Vec<Arg>,
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

    pub fn parse_struct_def(&mut self) -> ParseResult<(Span, StructDefStructural)> {
        let (span, decl) = self.parse_struct_decl()?;

        let _ = self.lexer.expect_colon(&mut self.errors)?;

        let fields = self.parse_struct_fields()?;

        let terminator = self
            .lexer
            .expect(&[TokenKind::BlockTerminator], &mut self.errors)?;

        let span = span.start..terminator.span.end;
        let out = StructDefStructural {
            id: 0,
            span: span.clone(),
            decl,
            fields,
        };

        Ok((span, out))
    }

    pub fn parse_enum_decl(&mut self) -> ParseResult<(Span, EnumDeclStructural)> {
        let enum_tkn = self.lexer.expect(&[TokenKind::EnumKw], &mut self.errors)?;
        let ident = self.lexer.peek();
        let out = match ident.kind {
            TokenKind::Ident(name) => {
                let ident = self.lexer.next_token(); // Consume ident
                let span = enum_tkn.span.start..ident.span.end;
                (
                    span.clone(),
                    EnumDeclStructural {
                        id: 0,
                        name,
                        span: enum_tkn.span.start..ident.span.end,
                    },
                )
            }
            _ => {
                self.errors.push(
                    YuuError::builder()
                        .kind(ErrorKind::MissingToken)
                        .message("Expected identifier after 'enum' keyword".to_string())
                        .source(
                            self.lexer.code_info.source.clone(),
                            self.lexer.code_info.file_name.clone(),
                        )
                        .span(ident.span.clone(), "expected enum name here")
                        .help("Enum declarations must be followed by a valid identifier")
                        .build(),
                );
                return Err(self.lexer.synchronize());
            }
        };
        Ok(out)
    }

    pub fn parse_enum_variant(&mut self) -> ParseResult<EnumVariant> {
        let ident_token = self.lexer.next_token();
        match ident_token.kind {
            TokenKind::Ident(name) => {
                let start_span = ident_token.span.clone();

                // Check if there's a colon for a data variant
                let peek = self.lexer.peek();

                // Check common error: "("
                if peek.kind == TokenKind::LParen {
                    self.errors.push(
                        YuuError::builder()
                            .kind(ErrorKind::UnexpectedToken)
                            .message("Unexpected '(' after variant name".to_string())
                            .source(
                                self.lexer.code_info.source.clone(),
                                self.lexer.code_info.file_name.clone(),
                            )
                            .span(
                                peek.span.clone(),
                                "Did you mean to use ':' for a data variant?",
                            )
                            .help("Enum variant data types should be specified with a colon ':', not parentheses '()'".to_string())
                            .build(),
                    );
                    return Err(self.lexer.synchronize());
                }

                if peek.kind == TokenKind::Colon {
                    // Data variant: consume colon and parse type
                    let _ = self.lexer.next_token(); // consume colon
                    let data_type = self.parse_type()?;
                    let end_span = data_type.span().end;

                    Ok(EnumVariant {
                        id: 0,
                        span: start_span.start..end_span,
                        name,
                        data_type: Some(data_type),
                    })
                } else {
                    // Unit variant: no type
                    Ok(EnumVariant {
                        id: 0,
                        span: start_span.clone(),
                        name,
                        data_type: None,
                    })
                }
            }
            _ => {
                self.errors.push(
                    YuuError::unexpected_token(
                        ident_token.span.clone(),
                        "an identifier".to_string(),
                        ident_token.kind,
                        self.lexer.code_info.source.clone(),
                        self.lexer.code_info.file_name.clone(),
                    )
                    .with_help("Expected a variant name identifier".to_string()),
                );
                Err(self.lexer.synchronize())
            }
        }
    }

    pub fn parse_enum_variants(&mut self) -> ParseResult<Vec<EnumVariant>> {
        let mut variants = Vec::new();
        loop {
            let la = self.lexer.peek();
            if la.kind == TokenKind::BlockTerminator {
                return Ok(variants);
            }
            let variant = self.parse_enum_variant()?;
            variants.push(variant);
            let la = self.lexer.peek();
            if la.kind == TokenKind::Comma {
                let _ = self.lexer.next_token();
            } else if la.kind != TokenKind::BlockTerminator {
                self.errors.push(YuuError::unexpected_token(
                    la.span.clone(),
                    "a comma ',' or a block terminator '.'".to_string(),
                    la.kind,
                    self.lexer.code_info.source.clone(),
                    self.lexer.code_info.file_name.clone(),
                ).with_help("Enum variants should be separated by commas, with the list ending with a block terminator ' .'".to_string()));
                return Err(self.lexer.synchronize());
            }
        }
    }

    pub fn parse_enum_def(&mut self) -> ParseResult<(Span, EnumDefStructural)> {
        let (span, decl) = self.parse_enum_decl()?;
        let _ = self.lexer.expect_colon(&mut self.errors)?;
        let variants = self.parse_enum_variants()?;
        let terminator = self
            .lexer
            .expect(&[TokenKind::BlockTerminator], &mut self.errors)?;
        let span = span.start..terminator.span.end;
        let out = EnumDefStructural {
            id: 0,
            span: span.clone(),
            decl,
            variants,
        };
        Ok((span, out))
    }

    pub fn parse_struct_decl(&mut self) -> ParseResult<(Span, StructDeclStructural)> {
        let struct_tkn = self
            .lexer
            .expect(&[TokenKind::StructKw], &mut self.errors)?;
        let ident = self.lexer.expect_tag(
            TokenKind::Ident("an identifier naming the struct".intern()),
            &mut self.errors,
        )?; // TODO: Really ugly - we are using the identifier token as error msg.
        let name = match ident.kind {
            TokenKind::Ident(ident) => ident,
            _ => unreachable!("We should have an identifier here"),
        };

        let span = struct_tkn.span.start..ident.span.end;

        Ok((span.clone(), StructDeclStructural { id: 0, span, name }))
    }

    pub fn make_func_def(
        &mut self,
        span: Span,
        decl: FuncDeclStructural,
        body: BlockStmt,
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
        let (block_span, mut block) = self.parse_block_stmt_inner()?; // TODO: write a parse_root_func_block - we don't want the root function blocks to have labels
        block.label = Some(ustr(FUNC_BLOCK_NAME)); // 🤡
        let span = span.start..block_span.end;
        Ok(self.make_func_def(span, decl, block))
    }

    pub fn parse_stmt(&mut self) -> ParseResult<(Span, StmtNode)> {
        let peek = self.lexer.peek();
        match peek.kind {
            TokenKind::LetKw => self.parse_let_stmt(),
            //TokenKind::OutKw => self.parse_out_stmt(),
            TokenKind::BreakKw => self.parse_break_stmt(),
            TokenKind::Return => self.parse_return_stmt(),
            TokenKind::DeferKw => self.parse_defer_stmt(),
            TokenKind::IfKw => self.parse_if_stmt(),
            TokenKind::WhileKw => self.parse_while_stmt(),
            TokenKind::MatchKw => self.parse_match_stmt(),
            TokenKind::Colon => self.parse_block_stmt(),
            _ => self.parse_atomic_stmt(),
        }
    }

    fn parse_return_stmt(&mut self) -> ParseResult<(Span, StmtNode)> {
        let ret_token = self.lexer.expect(&[TokenKind::Return], &mut self.errors)?;

        // Check if there's an expression or if we hit a semicolon/block terminator
        let peek_kind = self.lexer.peek().kind;
        let (span, expr) =
            if peek_kind == TokenKind::Semicolon || peek_kind == TokenKind::BlockTerminator {
                // No expression - return nil
                let span = ret_token.span.clone();
                (span, None)
            } else {
                // Parse the expression
                let (expr_span, expr) = self.parse_expr()?;
                let span =
                    self.expect_semicolon_or_block_terminator(ret_token.span.clone(), expr_span)?;
                (span, Some(Box::new(expr)))
            };

        Ok((
            span.clone(),
            StmtNode::Return(ReturnStmt { span, expr, id: 0 }),
        ))
    }

    fn parse_defer_stmt(&mut self) -> ParseResult<(Span, StmtNode)> {
        let defer_token = self.lexer.expect(&[TokenKind::DeferKw], &mut self.errors)?;

        // Parse the expression - defer always requires an expression
        let (expr_span, expr) = self.parse_expr()?;
        let span = self.expect_semicolon_or_block_terminator(defer_token.span.clone(), expr_span)?;

        Ok((
            span.clone(),
            StmtNode::Defer(DeferStmt {
                span,
                expr: Box::new(expr),
                id: 0
            }),
        ))
    }

    pub fn parse_structural(&mut self) -> ParseResult<(Span, StructuralNode)> {
        let t = self.lexer.peek();
        let x = match t.kind {
            TokenKind::FnKw => {
                let (range, res) = self.parse_func_def()?;
                (range, StructuralNode::FuncDef(res))
            }
            TokenKind::StructKw => {
                let (range, struct_) = self.parse_struct_def()?;
                (range, StructuralNode::StructDef(struct_))
            }
            TokenKind::EnumKw => {
                let (range, enum_) = self.parse_enum_def()?;
                (range, StructuralNode::EnumDef(enum_))
            }
            _ => {
                self.errors.push(
                    YuuError::unexpected_token(
                        t.span.clone(),
                        "fn".to_string(),
                        t.kind,
                        self.lexer.code_info.source.clone(),
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
        Ok(x)
    }

    pub fn parse(&mut self) -> AST {
        // Parse structural nodes as often as possible, until EOF
        let mut structural_nodes = Vec::new();
        loop {
            let t = self.lexer.peek();
            if t.kind == TokenKind::EOF {
                break;
            }

            let parsed_structural = self.parse_structural();
            match parsed_structural {
                Ok((_, structural)) => structural_nodes.push(Box::new(structural)),
                Err(CatchIn::FunctionDecl) => {
                    // Recover here
                    structural_nodes.push(Box::new(StructuralNode::Error(0)));
                    continue;
                }
                Err(CatchIn::BlockTerminator) => {
                    // Recover from malformed structural element by syncing to block end
                    // The synchronizer positioned us AT the BlockTerminator but didn't consume it
                    // We need to consume it to avoid infinite loop, then continue parsing
                    self.lexer.eat(); // Consume the BlockTerminator
                    structural_nodes.push(Box::new(StructuralNode::Error(0)));
                    continue;
                }
                Err(_) => {
                    // Unknown error during structural parsing - emergency recovery - just skip until we find a token that has a unique semantic definition
                    structural_nodes.push(Box::new(StructuralNode::Error(0)));
                    self.lexer.unstuck_parser();
                    continue;
                }
            }
        }

        AST {
            structurals: structural_nodes,
        }
    }
    pub fn parse_and_add_ids(&mut self) -> AST {
        let mut ast = self.parse();
        add_ids(&mut ast);
        ast
    }
}
