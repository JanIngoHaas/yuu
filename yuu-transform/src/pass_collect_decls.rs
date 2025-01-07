use yuu_shared::{
    ast::{FuncDeclStructural, StructuralNode, AST},
    block::{Block, RootBlock},
    context::Context,
    scheduler::Pass,
    semantic_error::SemanticError,
    type_info::TypeInfoTable,
};

pub struct PassCollectDecls {}

impl Default for PassCollectDecls {
    fn default() -> Self {
        Self::new()
    }
}

impl PassCollectDecls {
    pub fn new() -> Self {
        Self {}
    }

    fn collect_func_decl(
        decl: &FuncDeclStructural,
        block: &mut Block,
    ) -> Result<(), SemanticError> {
        block.declare_function(decl.name.clone(), decl.id, decl.span.clone())
    }

    fn collect_structural(
        structural: &StructuralNode,
        block: &mut Block,
    ) -> Result<(), SemanticError> {
        match structural {
            StructuralNode::FuncDecl(decl) => Self::collect_func_decl(decl, block),
            StructuralNode::FuncDef(def) => Self::collect_func_decl(&def.decl, block),
        }
    }

    fn collect_decls(ast: &AST, block: &mut Block) -> Result<(), SemanticError> {
        for node in &ast.structurals {
            Self::collect_structural(node, block)?;
        }
        Ok(())
    }
}

impl Pass for PassCollectDecls {
    fn run(&self, context: &mut Context) -> anyhow::Result<()> {
        let ast = context.require_pass_data::<AST>(self);

        let mut type_info_table = TypeInfoTable::new();
        let mut root_block = RootBlock::new(&mut type_info_table);
        let ast = ast.lock().unwrap();
        let ast = &*ast;

        Self::collect_decls(ast, root_block.root_mut())?;
        context.add_pass_data(root_block);
        context.add_pass_data(type_info_table);

        Ok(())
    }

    fn install(self, schedule: &mut yuu_shared::scheduler::Schedule) {
        schedule.requires_resource_read::<AST>(&self);
        schedule.produces_resource::<Box<RootBlock>>(&self);
        schedule.produces_resource::<TypeInfoTable>(&self);
        schedule.add_pass(self);
    }

    fn get_name(&self) -> &'static str {
        "CollectDecls"
    }
}
