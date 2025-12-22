use crate::pass_parse::ast::*;

pub struct IdGenerator {
    next_expr_id: NodeId,
    next_non_expr_id: NodeId,
}

impl IdGenerator {
    pub fn new() -> Self {
        Self {
            next_expr_id: 0,
            next_non_expr_id: usize::MAX,
        }
    }

    fn next_expr(&mut self) -> NodeId {
        let id = self.next_expr_id;
        self.next_expr_id += 1;
        id
    }

    pub fn next_non_expr(&mut self) -> NodeId {
        let id = self.next_non_expr_id;
        self.next_non_expr_id -= 1;
        id
    }

    pub fn max_expr_id(&self) -> NodeId {
        if self.next_expr_id == 0 { 0 } else { self.next_expr_id - 1 }
    }

    pub fn expr_count(&self) -> usize {
        self.next_expr_id as usize
    }
}

impl Default for IdGenerator {
    fn default() -> Self {
        Self::new()
    }
}

pub trait AddId {
    fn add_id(&mut self, generator: &mut IdGenerator);
}

impl AddId for Node {
    fn add_id(&mut self, generator: &mut IdGenerator) {
        match self {
            Node::Expr(expr) => expr.add_id(generator),
            Node::Stmt(stmt) => stmt.add_id(generator),
            Node::Type(ty) => ty.add_id(generator),
            Node::Structural(s) => s.add_id(generator),
            Node::Binding(pn) => pn.add_id(generator),
        }
    }
}

impl AddId for ExprNode {
    fn add_id(&mut self, generator: &mut IdGenerator) {
        match self {
            ExprNode::Literal(lit) => lit.id = generator.next_expr(),
            ExprNode::Binary(bin) => {
                bin.id = generator.next_expr();
                bin.left.add_id(generator);
                bin.right.add_id(generator);
            }
            ExprNode::Unary(un) => {
                un.id = generator.next_expr();
                un.expr.add_id(generator);
            }
            ExprNode::Ident(id) => id.id = generator.next_expr(),
            ExprNode::FuncCall(func_call_expr) => {
                func_call_expr.lhs.add_id(generator);
                for arg in &mut func_call_expr.args {
                    arg.add_id(generator);
                }
                func_call_expr.id = generator.next_expr();
            }
            ExprNode::Assignment(assignment_expr) => {
                assignment_expr.id = generator.next_expr();
                assignment_expr.lhs.add_id(generator);
                assignment_expr.rhs.add_id(generator);
            }
            ExprNode::StructInstantiation(struct_instantiation_expr) => {
                struct_instantiation_expr.id = generator.next_expr();
                for (_name, field) in &mut struct_instantiation_expr.fields {
                    field.add_id(generator);
                }
            }
            ExprNode::MemberAccess(member_access_expr) => {
                member_access_expr.id = generator.next_expr();
                member_access_expr.lhs.add_id(generator);
                // Field doesn't need an ID as it's just a name + span
            }
            ExprNode::EnumInstantiation(enum_instantiation_expr) => {
                enum_instantiation_expr.id = generator.next_expr();
                if let Some(data) = &mut enum_instantiation_expr.data {
                    data.add_id(generator);
                }
            }
            ExprNode::Deref(deref_expr) => {
                deref_expr.id = generator.next_expr();
                deref_expr.expr.add_id(generator);
            }
            ExprNode::AddressOf(address_of_expr) => {
                address_of_expr.id = generator.next_expr();
                address_of_expr.expr.add_id(generator);
            }

            ExprNode::HeapAlloc(heap_alloc_expr) => {
                heap_alloc_expr.id = generator.next_expr();
                heap_alloc_expr.expr.add_id(generator);
            }
            ExprNode::Array(array_expr) => {
                array_expr.id = generator.next_expr();
                if let Some(init_value) = &mut array_expr.init_expr {
                    init_value.add_id(generator);
                }
                if let Some(element_type) = &mut array_expr.element_type {
                    element_type.add_id(generator);
                }
                array_expr.size.add_id(generator);
            }
            ExprNode::ArrayLiteral(array_literal_expr) => {
                array_literal_expr.id = generator.next_expr();
                for element in &mut array_literal_expr.elements {
                    element.add_id(generator);
                }
                if let Some(element_type) = &mut array_literal_expr.element_type {
                    element_type.add_id(generator);
                }
            }
            ExprNode::PointerOp(pointer_op_expr) => {
                pointer_op_expr.id = generator.next_expr();
                pointer_op_expr.left.add_id(generator);
                pointer_op_expr.right.add_id(generator);
            }
            ExprNode::Cast(cast_expr) => {
                cast_expr.id = generator.next_expr();
                cast_expr.expr.add_id(generator);
                cast_expr.target_type.add_id(generator);
            }
            ExprNode::LuaMeta(lua_meta) => {
                lua_meta.id = generator.next_expr();
            }
        }
    }
}

impl AddId for StmtNode {
    fn add_id(&mut self, generator: &mut IdGenerator) {
        match self {
            StmtNode::Let(let_stmt) => {
                let_stmt.id = generator.next_non_expr();
                let_stmt.binding.add_id(generator);
                let_stmt.expr.add_id(generator);
            }
            StmtNode::Atomic(expr) => expr.add_id(generator),
            StmtNode::Break(exit_stmt) => {
                exit_stmt.id = generator.next_non_expr();
            }
            StmtNode::Return(return_stmt) => {
                return_stmt.id = generator.next_non_expr();
                if let Some(expr) = return_stmt.expr.as_deref_mut() {
                    expr.add_id(generator)
                }
            }
            StmtNode::Defer(defer_stmt) => {
                defer_stmt.id = generator.next_non_expr();
                defer_stmt.expr.add_id(generator);
            }
            StmtNode::If(if_stmt) => {
                if_stmt.id = generator.next_non_expr();
                if_stmt.if_block.condition.add_id(generator);
                if_stmt.if_block.body.add_id(generator);
                // Add IDs to else-if blocks
                for else_if in &mut if_stmt.else_if_blocks {
                    else_if.condition.add_id(generator);
                    else_if.body.add_id(generator);
                }
                if let Some(else_expr) = &mut if_stmt.else_block {
                    else_expr.add_id(generator);
                }
            }
            StmtNode::While(while_stmt) => {
                while_stmt.id = generator.next_non_expr();
                while_stmt.condition_block.body.add_id(generator);
                while_stmt.condition_block.condition.add_id(generator);
            }
            StmtNode::Block(block_stmt) => {
                block_stmt.id = generator.next_non_expr();
                for stmt in &mut block_stmt.body {
                    stmt.add_id(generator);
                }
            }
            StmtNode::Match(match_stmt) => {
                match_stmt.id = generator.next_non_expr();
                match_stmt.scrutinee.add_id(generator);
                for arm in &mut match_stmt.arms {
                    arm.id = generator.next_non_expr();
                    match &mut *arm.pattern {
                        RefutablePatternNode::Enum(enum_pattern) => {
                            enum_pattern.id = generator.next_non_expr();
                            if let Some(binding) = &mut enum_pattern.binding {
                                binding.add_id(generator);
                            }
                        }
                    }
                    arm.body.add_id(generator);
                }
                if let Some(default_case) = &mut match_stmt.default_case {
                    default_case.add_id(generator);
                }
            }
            StmtNode::Decl(decl_stmt) => {
                decl_stmt.id = generator.next_non_expr();
                decl_stmt.ident.add_id(generator);
            }
            StmtNode::Def(def_stmt) => {
                def_stmt.id = generator.next_non_expr();
                def_stmt.ident.add_id(generator);
                def_stmt.expr.add_id(generator);
            }
            StmtNode::Error(e) => *e = generator.next_non_expr(),
        }
    }
}

impl AddId for TypeNode {
    fn add_id(&mut self, generator: &mut IdGenerator) {
        match self {
            TypeNode::Ident(i) => {
                i.id = generator.next_non_expr();
            }
            TypeNode::BuiltIn(built_in_type) => {
                built_in_type.id = generator.next_non_expr();
            }
            TypeNode::Pointer(pointer_type) => {
                pointer_type.id = generator.next_non_expr();
                pointer_type.pointee.add_id(generator);
            }
            TypeNode::Array(array_type) => {
                array_type.id = generator.next_non_expr();
                array_type.element_type.add_id(generator);
                array_type.size.add_id(generator);
            }
        }
    }
}

impl AddId for BindingNode {
    fn add_id(&mut self, generator: &mut IdGenerator) {
        match self {
            BindingNode::Ident(i) => {
                i.id = generator.next_expr(); // Variable bindings need expression IDs for type lookup
            }
        }
    }
}

impl AddId for IdentBinding {
    fn add_id(&mut self, generator: &mut IdGenerator) {
        self.id = generator.next_expr(); // IdentBinding needs expression ID for type lookup
    }
}

impl AddId for RefutablePatternNode {
    fn add_id(&mut self, generator: &mut IdGenerator) {
        match self {
            RefutablePatternNode::Enum(enum_pattern) => {
                enum_pattern.id = generator.next_non_expr();
                if let Some(binding) = &mut enum_pattern.binding {
                    binding.add_id(generator);
                }
            }
        }
    }
}

impl AddId for Arg {
    fn add_id(&mut self, generator: &mut IdGenerator) {
        self.ty.add_id(generator);
        self.id = generator.next_expr(); // Function parameters need expression IDs for type lookup
    }
}

impl AddId for FuncDeclStructural {
    fn add_id(&mut self, generator: &mut IdGenerator) {
        self.id = generator.next_non_expr();
        for arg in &mut self.args {
            arg.add_id(generator);
        }
        if let Some(ty) = &mut self.ret_ty {
            ty.add_id(generator);
        }
    }
}

impl AddId for StructDeclStructural {
    fn add_id(&mut self, generator: &mut IdGenerator) {
        self.id = generator.next_non_expr();
    }
}

impl AddId for EnumDeclStructural {
    fn add_id(&mut self, generator: &mut IdGenerator) {
        self.id = generator.next_non_expr();
    }
}

impl AddId for StructuralNode {
    fn add_id(&mut self, generator: &mut IdGenerator) {
        match self {
            StructuralNode::FuncDecl(fd) => {
                fd.add_id(generator);
            }
            StructuralNode::FuncDef(def) => {
                def.decl.add_id(generator);
                def.id = generator.next_expr(); // Function definitions need expression IDs for type table
                def.body.add_id(generator);
            }
            StructuralNode::Error(x) => *x = generator.next_non_expr(),
            StructuralNode::StructDecl(struct_decl_structural) => {
                struct_decl_structural.id = generator.next_non_expr();
            }
            StructuralNode::StructDef(struct_def_structural) => {
                struct_def_structural.decl.add_id(generator);
                struct_def_structural.id = generator.next_non_expr();
                for field in &mut struct_def_structural.fields {
                    field.add_id(generator);
                }
            }
            StructuralNode::EnumDef(enum_def_structural) => {
                enum_def_structural.decl.add_id(generator);
                enum_def_structural.id = generator.next_non_expr();
                for variant in &mut enum_def_structural.variants {
                    variant.id = generator.next_non_expr();
                }
            }
        }
    }
}

impl AddId for BlockStmt {
    fn add_id(&mut self, generator: &mut IdGenerator) {
        self.id = generator.next_non_expr();
        for stmt in &mut self.body {
            stmt.add_id(generator);
        }
    }
}

pub fn add_ids(root: &mut AST) -> (usize, IdGenerator) {
    // Use a single IdGenerator instance for the entire AST
    let mut generator = IdGenerator::new();
    for node in root.structurals.iter_mut() {
        node.add_id(&mut generator);
    }
    let expr_count = generator.expr_count();
    (expr_count, generator)
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
            ExprNode::FuncCall(func_call_expr) => func_call_expr.id,
            ExprNode::Assignment(assignment_expr) => assignment_expr.id,
            ExprNode::StructInstantiation(struct_instantiation_expr) => {
                struct_instantiation_expr.id
            }
            ExprNode::MemberAccess(member_access_expr) => member_access_expr.id,
            ExprNode::EnumInstantiation(enum_instantiation_expr) => enum_instantiation_expr.id,
            ExprNode::Deref(deref_expr) => deref_expr.id,
            ExprNode::AddressOf(address_of_expr) => address_of_expr.id,

            ExprNode::HeapAlloc(heap_alloc_expr) => heap_alloc_expr.id,
            ExprNode::Array(array_expr) => array_expr.id,
            ExprNode::ArrayLiteral(array_literal_expr) => array_literal_expr.id,
            ExprNode::PointerOp(pointer_op_expr) => pointer_op_expr.id,
            ExprNode::Cast(cast_expr) => cast_expr.id,
            ExprNode::LuaMeta(lua_meta) => lua_meta.id,
        }
    }
}

impl GetId for StmtNode {
    fn node_id(&self) -> NodeId {
        match self {
            StmtNode::Let(let_stmt) => let_stmt.id,
            StmtNode::Atomic(expr) => expr.node_id(),
            StmtNode::Break(exit_stmt) => exit_stmt.id,
            StmtNode::Return(return_stmt) => return_stmt.id,
            StmtNode::Defer(defer_stmt) => defer_stmt.id,
            StmtNode::If(if_stmt) => if_stmt.id,
            StmtNode::While(while_stmt) => while_stmt.id,
            StmtNode::Block(block_stmt) => block_stmt.id,
            StmtNode::Match(match_stmt) => match_stmt.id,
            StmtNode::Decl(decl_stmt) => decl_stmt.id,
            StmtNode::Def(def_stmt) => def_stmt.id,
            StmtNode::Error(x) => *x,
        }
    }
}

impl GetId for TypeNode {
    fn node_id(&self) -> NodeId {
        match self {
            TypeNode::Ident(i) => i.id,
            TypeNode::BuiltIn(built_in_type) => built_in_type.id,
            TypeNode::Pointer(pointer_type) => pointer_type.id,
            TypeNode::Array(array_type) => array_type.id,
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
            StructuralNode::StructDecl(struct_decl_structural) => struct_decl_structural.id,
            StructuralNode::StructDef(struct_def_structural) => struct_def_structural.id,
            StructuralNode::EnumDef(enum_def_structural) => enum_def_structural.id,
        }
    }
}
