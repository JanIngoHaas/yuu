use std::{cell::RefCell, rc::Rc};

use yuu_parse::parser::SourceCodeInfo;
use yuu_shared::{
    ast::{FuncDeclStructural, Node, StructuralNode, AST},
    block::Block,
    semantic_error::SemanticError,
    type_info::TypeInfoTable,
    Pass,
};

pub struct PassCollectDecls {}

impl PassCollectDecls {
    pub fn new() -> Self {
        Self {}
    }

    fn collect_func_decl(
        decl: &FuncDeclStructural,
        block: &Rc<RefCell<Block>>,
    ) -> Result<(), SemanticError> {
        block
            .as_ref()
            .borrow_mut()
            .declare_function(decl.name.clone(), decl.id, decl.span.clone())
    }

    fn collect_structural(
        structural: &StructuralNode,
        block: &Rc<RefCell<Block>>,
    ) -> Result<(), SemanticError> {
        match structural {
            StructuralNode::FuncDecl(decl) => Self::collect_func_decl(decl, block),
            StructuralNode::FuncDef(def) => Self::collect_func_decl(&def.decl, block),
        }
    }

    fn collect_decls(ast: &AST, block: &Rc<RefCell<Block>>) -> Result<(), SemanticError> {
        for node in &ast.structurals {
            Self::collect_structural(node, block)?;
        }
        Ok(())
    }
}

impl Pass for PassCollectDecls {
    fn run(&mut self, context: &mut yuu_shared::Context) -> bool {
        let src_info = context.require_pass_data::<SourceCodeInfo>("CollectDecls");

        let mut type_info_table = TypeInfoTable::new();
        let root_block = Block::root(&mut type_info_table);

        if let Err(err) = Self::collect_decls(&src_info.borrow().root_node, &root_block) {
            // TODO: Handle error properly
            return false;
        }

        context.add_pass_data(root_block);
        context.add_pass_data(type_info_table);

        true
    }

    fn install(self, pipeline: &mut yuu_shared::Pipeline) {
        pipeline.add_pass(self);
    }
}
