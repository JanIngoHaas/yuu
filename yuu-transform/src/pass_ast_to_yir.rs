use yuu_shared::{
    ast::{BinOp, ExprNode, Node, NodeId, StmtNode, UnaryOp},
    type_info::{TypeInfo, TypeInfoTable},
    yir::{InstPair, InstTriple, Inst, InstKind, Operand, Register},
};
use std::{borrow::Borrow, rc::Rc};

pub struct TransientData {
    next_var: i64,
    next_label: i64,
    next_slot: i64,
    instructions: Vec<Inst>,
}

pub fn lower_type(ty: &Rc<TypeInfo>) -> Rc<yuu_shared::yir::YirTypeInfo> {
    match ty.borrow() {
        TypeInfo::BuiltIn(prim) => Rc::new(yuu_shared::yir::YirTypeInfo::YuuPrimitiveType(*prim)),
        _ => todo!("Non-primitive types not supported yet"),
    }
}

impl TransientData {
    pub fn new() -> Self {
        Self {
            next_var: 0,
            next_label: 0,
            next_slot: -1,
            instructions: Vec::new(),
        }
    }

    fn fresh_slot(&mut self, ty: &Rc<TypeInfo>) -> Register {
        let id = self.next_slot;
        self.next_slot -= 1;
        Register {
            name: None,
            id,
            ty: lower_type(ty),
        }
    }

    fn fresh_var(&mut self, ty: &Rc<TypeInfo>) -> Register {
        let id = self.next_var;
        self.next_var += 1;
        Register {
            name: None,
            id,
            ty: lower_type(ty),
        }
    }

    fn fresh_label(&mut self) -> i64 {
        let id = self.next_label;
        self.next_label += 1;
        id
    }

    fn add_inst(&mut self, kind: InstKind) {
        self.instructions.push(Inst { kind, id: self.instructions.len() as i64 });
    }
}

pub struct AstToYirPass<'a> {
    type_table: &'a TypeInfoTable,
}

impl<'a> AstToYirPass<'a> {
    pub fn new(type_table: &'a TypeInfoTable) -> Self {
        Self { type_table }
    }

    fn get_type(&self, node_id: NodeId) -> &Rc<TypeInfo> {
        self.type_table.types.get(&node_id).unwrap()
    }

    pub fn lower_ast(&self, node: &Node) -> (Vec<Inst>, Option<Operand>) {
        let mut data = TransientData::new();
        let result = self.lower_ast_with_data(node, &mut data);
        (data.instructions, result)
    }

    fn lower_ast_with_data(&self, node: &Node, data: &mut TransientData) -> Option<Operand> {
        match node {
            Node::Expr(expr) => self.lower_expr(expr, data),
            Node::Stmt(stmt) => self.lower_stmt(stmt, data),
            _ => panic!("Unexpected node type at top level"),
        }
    }

    fn lower_expr(&self, expr: &ExprNode, data: &mut TransientData) -> Option<Operand> {
        match expr {
            ExprNode::Literal(lit) => {
                match &lit.lit {
                    yuu_shared::token::Token { kind: yuu_shared::token::TokenKind::Integer(yuu_shared::token::Integer::I64(n)), .. } => {
                        Some(Operand::ImmediateI64Op(*n))
                    }
                    yuu_shared::token::Token { kind: yuu_shared::token::TokenKind::F32(f), .. } => {
                        Some(Operand::ImmediateF32Op(*f))
                    }
                    yuu_shared::token::Token { kind: yuu_shared::token::TokenKind::F64(f), .. } => {
                        Some(Operand::ImmediateF64Op(*f))
                    }
                    _ => panic!("Unsupported literal type"),
                }
            }

            // We actually need a pass that lowers operator overloads to function calls
            // So, we can be sure that binary expressions are always simple arithmetic operations
            ExprNode::Binary(bin_expr) => {
                let lhs = self.lower_expr(&bin_expr.left, data).unwrap();
                let rhs = self.lower_expr(&bin_expr.right, data).unwrap();
                let result_type = self.get_type(bin_expr.id);
                assert!(result_type.is_primitive()); // For now, we only support primitive types‚
                let result = data.fresh_var(result_type);

                let inst = match bin_expr.op {
                    BinOp::Add => InstKind::Add(InstTriple { register: result.clone(), op1: lhs, op2: rhs }),
                    BinOp::Subtract => InstKind::Sub(InstTriple { register: result.clone(), op1: lhs, op2: rhs }),
                    BinOp::Multiply => InstKind::Mul(InstTriple { register: result.clone(), op1: lhs, op2: rhs }),
                    BinOp::Divide => InstKind::Div(InstTriple { register: result.clone(), op1: lhs, op2: rhs }),
                    _ => todo!("Other binary operators not implemented yet"),
                };

                data.add_inst(inst);
                Some(Operand::RegisterOp(result))
            }
            ExprNode::Unary(un_expr) => {
                let operand = self.lower_expr(&un_expr.operand, data).unwrap();
                match un_expr.op {
                    UnaryOp::Negate => {
                        let result_type = self.get_type(un_expr.id);
                        let result = data.fresh_var(result_type);
                        data.add_inst(InstKind::Neg(InstPair {
                            mem: result.clone(),
                            op: operand,
                        }));
                        return Some(Operand::RegisterOp(result))
                    }
                    UnaryOp::Pos => {
                        return Some(operand); 
                    }
                }

            }
            ExprNode::If(if_expr) => {

                let cond = self.lower_expr(&if_expr.if_block.condition, data).unwrap();
                let then_label = data.fresh_label();
                let else_label = data.fresh_label();
                let merge_label = data.fresh_label();
                let merge_slot = data.fresh_slot(self.get_type(if_expr.id));

                data.add_inst(InstKind::Slot(merge_slot, Default::default()));
                data.add_inst(InstKind::Branch(cond, then_label, else_label));
                data.add_inst(InstKind::Label(then_label, None));
                let then_result = self.lower_expr(&if_expr.if_block.body, data);
                // Write result to merge slot



                // TODO: Implement if expression lowering
                // 1. Lower condition
                // 2. Create labels for then/else blocks
                // 3. Generate branch instruction
                // 4. Lower then block
                // 5. Generate jump to merge point
                // 6. Lower else block
                // 7. Generate merge point
                todo!("If expression lowering not implemented yet")
            }
            // Add other expression types as needed
            _ => todo!("Expression type not implemented yet"),
        }
    }

    fn lower_stmt(&self, stmt: &StmtNode, data: &mut TransientData) -> Option<Operand> {
        match stmt {
            StmtNode::Let(let_stmt) => {
                let value = self.lower_expr(&let_stmt.expr, data).unwrap();
                let var_type = self.get_type(let_stmt.id);
                let var = Register {
                    name: Some(let_stmt.binding.to_string()),
                    id: data.next_var,
                    ty: var_type,
                };
                data.next_var += 1;

                data.add_inst(Inst::Declare(var.clone()));
                data.add_inst(Inst::Assign(var.clone(), value));
                Some(Operand::RegisterOp(var))
            }
            StmtNode::Atomic(expr) => self.lower_expr(expr, data),
            StmtNode::Return(ret_stmt) => {
                // TODO: Implement return statement
                // 1. Lower expression
                // 2. Assign to return variable
                // 3. Jump to function epilogue
                todo!("Return statement not implemented yet")
            }
        }
    }
}
