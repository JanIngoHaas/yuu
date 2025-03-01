use yuu_shared::ast::*;

pub struct IdGenerator {
    next_id: NodeId,
}

impl IdGenerator {
    pub fn new() -> Self {
        Self { next_id: 0 }
    }

    fn next(&mut self) -> NodeId {
        if self.next_id == 8 {
            println!("Yo");
        }
        let id = self.next_id;
        self.next_id += 1;
        id
    }
}

impl Default for IdGenerator {
    fn default() -> Self {
        Self::new()
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
            Node::Binding(pn) => pn.add_id(gen),
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
                block.add_id(gen);
            }
            ExprNode::FuncCall(func_call_expr) => {
                func_call_expr.lhs.add_id(gen);
                for arg in &mut func_call_expr.args {
                    arg.add_id(gen);
                }
                func_call_expr.id = gen.next();
            }
            ExprNode::If(if_expr) => {
                if_expr.id = gen.next();
                if_expr.if_block.condition.add_id(gen);
                if_expr.if_block.body.add_id(gen);
                // Add IDs to else-if blocks
                for else_if in &mut if_expr.else_if_blocks {
                    else_if.condition.add_id(gen);
                    else_if.body.add_id(gen);
                }
                if let Some(else_expr) = &mut if_expr.else_block {
                    else_expr.add_id(gen);
                }
            }
            ExprNode::Assignment(assignment_expr) => {
                assignment_expr.id = gen.next();
                assignment_expr.binding.add_id(gen);
                assignment_expr.rhs.add_id(gen);
            }
        }
    }
}

impl AddId for StmtNode {
    fn add_id(&mut self, gen: &mut IdGenerator) {
        match self {
            StmtNode::Let(let_stmt) => {
                let_stmt.id = gen.next();
                let_stmt.binding.add_id(gen);
                let_stmt.expr.add_id(gen);
                if let Some(ty) = &mut let_stmt.ty {
                    ty.add_id(gen);
                }
            }
            StmtNode::Atomic(expr) => expr.add_id(gen),
            StmtNode::Break(exit_stmt) => {
                exit_stmt.id = gen.next();
                exit_stmt.expr.add_id(gen);
            }
            StmtNode::Error(e) => *e = gen.next(),
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

impl AddId for BindingNode {
    fn add_id(&mut self, gen: &mut IdGenerator) {
        match self {
            BindingNode::Ident(i) => {
                i.id = gen.next();
            }
        }
    }
}

impl AddId for FuncArg {
    fn add_id(&mut self, gen: &mut IdGenerator) {
        self.binding.add_id(gen);
        self.ty.add_id(gen);
        self.id = gen.next();
    }
}

impl AddId for FuncDeclStructural {
    fn add_id(&mut self, gen: &mut IdGenerator) {
        self.id = gen.next();
        for arg in &mut self.args {
            arg.add_id(gen);
        }
        if let Some(ty) = &mut self.ret_ty {
            ty.add_id(gen);
        }
    }
}

impl AddId for StructuralNode {
    fn add_id(&mut self, gen: &mut IdGenerator) {
        match self {
            StructuralNode::FuncDecl(fd) => {
                fd.add_id(gen);
            }
            StructuralNode::FuncDef(def) => {
                def.decl.add_id(gen);
                def.id = gen.next();
                def.body.add_id(gen);
            }
            StructuralNode::Error(x) => *x = gen.next(),
        }
    }
}

impl AddId for BlockExpr {
    fn add_id(&mut self, gen: &mut IdGenerator) {
        self.id = gen.next();
        for stmt in &mut self.body {
            stmt.add_id(gen);
        }
        if let Some(last_expr) = &mut self.last_expr {
            last_expr.add_id(gen);
        }
    }
}

pub fn add_ids(root: &mut AST) {
    // Use a single IdGenerator instance for the entire AST
    let mut generator = IdGenerator::new();
    for node in root.structurals.iter_mut() {
        node.add_id(&mut generator);
    }
}

pub trait GetId {
    fn node_id(&self) -> NodeId;
}

impl GetId for Node {
    fn node_id(&self) -> NodeId {
        match self {
            Node::Expr(expr) => expr.node_id(),
            Node::Stmt(stmt) => stmt.node_id(),
            Node::Type(ty) => ty.node_id(),
            Node::Structural(s) => s.node_id(),
            Node::Binding(pn) => pn.node_id(),
        }
    }
}

impl GetId for ExprNode {
    fn node_id(&self) -> NodeId {
        match self {
            ExprNode::Literal(lit) => lit.id,
            ExprNode::Binary(bin) => bin.id,
            ExprNode::Unary(un) => un.id,
            ExprNode::Ident(id) => id.id,
            ExprNode::Block(block) => block.id,
            ExprNode::FuncCall(func_call_expr) => func_call_expr.id,
            ExprNode::If(if_expr) => if_expr.id,
            ExprNode::Assignment(assignment_expr) => assignment_expr.id,
        }
    }
}

impl GetId for StmtNode {
    fn node_id(&self) -> NodeId {
        match self {
            StmtNode::Let(let_stmt) => let_stmt.id,
            StmtNode::Atomic(expr) => expr.node_id(),
            StmtNode::Break(exit_stmt) => exit_stmt.id,
            StmtNode::Error(x) => *x,
        }
    }
}

impl GetId for TypeNode {
    fn node_id(&self) -> NodeId {
        match self {
            TypeNode::Ident(i) => i.id,
            TypeNode::BuiltIn(built_in_type) => built_in_type.id,
        }
    }
}

impl GetId for BindingNode {
    fn node_id(&self) -> NodeId {
        match self {
            BindingNode::Ident(i) => i.id,
        }
    }
}

impl GetId for StructuralNode {
    fn node_id(&self) -> NodeId {
        match self {
            StructuralNode::FuncDecl(fd) => fd.id,
            StructuralNode::FuncDef(def) => def.id,
            StructuralNode::Error(x) => *x,
        }
    }
}
