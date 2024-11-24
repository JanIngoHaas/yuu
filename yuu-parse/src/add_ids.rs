use yuu_shared::ast::*;

pub struct IdGenerator {
    next_id: usize,
}

impl IdGenerator {
    pub fn new() -> Self {
        Self { next_id: 0 }
    }

    fn next(&mut self) -> usize {
        let id = self.next_id;
        self.next_id += 1;
        id
    }
}

pub trait AddId {
    fn add_id(&mut self, gen: &mut IdGenerator);
}

impl AddId for Node {
    fn add_id(&mut self, gen: &mut IdGenerator) {
        match self {
            Node::Expr(expr) => expr.add_id(gen),
            Node::Stmt(stmt) => stmt.add_id(gen),
            Node::Type(ty) => ty.add_id(gen),
            Node::Structural(s) => s.add_id(gen),
        }
    }
}

impl AddId for ExprNode {
    fn add_id(&mut self, gen: &mut IdGenerator) {
        match self {
            ExprNode::Literal(lit) => lit.id = gen.next(),
            ExprNode::Binary(bin) => {
                bin.id = gen.next();
                bin.left.add_id(gen);
                bin.right.add_id(gen);
            }
            ExprNode::Unary(un) => {
                un.id = gen.next();
                un.operand.add_id(gen);
            }
            ExprNode::Ident(id) => id.id = gen.next(),
            ExprNode::Block(block) => {
                block.id = gen.next();
                for stmt in &mut block.body {
                    stmt.add_id(gen);
                }
            }
        }
    }
}

impl AddId for StmtNode {
    fn add_id(&mut self, gen: &mut IdGenerator) {
        match self {
            StmtNode::Let(let_stmt) => {
                let_stmt.expr.add_id(gen);
                if let Some(ty) = &mut let_stmt.ty {
                    ty.add_id(gen);
                }
            }
            StmtNode::Atomic(expr) => expr.add_id(gen),
            StmtNode::Return(ret) => ret.expr.add_id(gen),
        }
    }
}

impl AddId for TypeNode {
    fn add_id(&mut self, gen: &mut IdGenerator) {
        match self {
            TypeNode::Ident(i) => {
                i.id = gen.next();
            }
            TypeNode::BuiltIn(built_in_type) => {
                built_in_type.id = gen.next();
            }
        }
    }
}

impl AddId for StructuralNode {
    fn add_id(&mut self, gen: &mut IdGenerator) {
        match self {
            StructuralNode::FuncDecl(fd) => {
                fd.id = gen.next();
                for arg in &mut fd.args {
                    arg.add_id(gen);
                }
                if let Some(ty) = &mut fd.ret_ty {
                    ty.add_id(gen);
                }
            }
            StructuralNode::FuncDef(def) => {
                def.id = gen.next();
                for stmt in &mut def.body.body {
                    stmt.add_id(gen);
                }
            }
        }
    }
}

pub fn add_ids(node: &mut Node) {
    let mut gen = IdGenerator::new();
    node.add_id(&mut gen);
}
