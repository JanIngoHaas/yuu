use std::collections::{HashMap, HashSet};

use logos::Span;
use lsp_server::{Connection, ErrorCode, Message, Notification, Request, Response};
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionOptions, CompletionParams, CompletionResponse,
    Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, GotoDefinitionParams,
    GotoDefinitionResponse, Hover, HoverContents, HoverParams, HoverProviderCapability, InlayHint,
    InlayHintKind, InlayHintLabel, InlayHintParams, Location, MarkupContent, MarkupKind,
    NumberOrString, OneOf, Position, PublishDiagnosticsParams, Range, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
    TextDocumentSyncSaveOptions,
};
use miette::{IntoDiagnostic, Result};

use crate::{
    pass_diagnostics::YuuError,
    pass_parse::{
        AST, Arg, BindingNode, BlockStmt, ExprNode, FuncDeclStructural, IdentBinding, NodeId,
        StmtNode, StructuralMetadata, StructuralNode, TypeNode,
    },
    utils::pipeline::Pipeline,
};

const KEYWORD_COMPLETIONS: &[&str] = &[
    "end", "fn", "struct", "enum", "union", "let", "dec", "def", "if", "else", "while", "match",
    "case", "default", "return", "break", "defer", "mut", "extern", "new", "as", "true", "false",
    "nil",
];

#[derive(Debug, Clone)]
struct DocumentState {
    version: i32,
    text: String,
    analysis: DocumentAnalysis,
}

#[derive(Debug, Default)]
struct YuuLanguageServer {
    documents: HashMap<String, DocumentState>,
    shutdown_requested: bool,
}

impl YuuLanguageServer {
    fn handle_notification(
        &mut self,
        connection: &Connection,
        notification: Notification,
    ) -> Result<bool> {
        match notification.method.as_str() {
            "textDocument/didOpen" => {
                let params: DidOpenTextDocumentParams =
                    serde_json::from_value(notification.params).into_diagnostic()?;
                let uri = params.text_document.uri.to_string();
                let text = params.text_document.text;
                let analysis = analyze_document(&uri, &text, DiagnosticsMode::Off);
                self.documents.insert(
                    uri.clone(),
                    DocumentState {
                        version: params.text_document.version,
                        text,
                        analysis,
                    },
                );
                self.send_diagnostics(
                    connection,
                    uri,
                    Vec::new(),
                    Some(params.text_document.version),
                )?;
            }
            "textDocument/didChange" => {
                let params: DidChangeTextDocumentParams =
                    serde_json::from_value(notification.params).into_diagnostic()?;
                let uri = params.text_document.uri.to_string();
                if let Some(document) = self.documents.get_mut(&uri)
                    && let Some(last_change) = params.content_changes.into_iter().last()
                {
                    // Full sync: latest change carries full document text.
                    document.text = last_change.text;
                    document.version = params.text_document.version;
                    document.analysis =
                        analyze_document(&uri, &document.text, DiagnosticsMode::Off);
                    self.send_diagnostics(
                        connection,
                        uri,
                        Vec::new(),
                        Some(params.text_document.version),
                    )?;
                }
            }
            "textDocument/didSave" => {
                let params: DidSaveTextDocumentParams =
                    serde_json::from_value(notification.params).into_diagnostic()?;
                let uri = params.text_document.uri.to_string();
                if let Some(document) = self.documents.get_mut(&uri) {
                    document.analysis =
                        analyze_document(&uri, &document.text, DiagnosticsMode::OnSave);
                    self.publish_diagnostics(connection, &uri)?;
                }
            }
            "textDocument/didClose" => {
                let params: DidCloseTextDocumentParams =
                    serde_json::from_value(notification.params).into_diagnostic()?;
                let uri = params.text_document.uri.to_string();
                self.documents.remove(&uri);
                self.send_diagnostics(connection, uri, Vec::new(), None)?;
            }
            "exit" => {
                if self.shutdown_requested {
                    return Ok(true);
                }
                std::process::exit(1);
            }
            _ => {}
        }
        Ok(false)
    }

    fn handle_request(&mut self, connection: &Connection, request: Request) -> Result<()> {
        match request.method.as_str() {
            "shutdown" => {
                self.shutdown_requested = true;
                let response = Response::new_ok(request.id, ());
                connection
                    .sender
                    .send(Message::Response(response))
                    .into_diagnostic()?;
            }
            "textDocument/completion" => {
                let params: CompletionParams =
                    serde_json::from_value(request.params).into_diagnostic()?;
                let result = self.completion(params);
                self.send_ok(connection, request.id, result)?;
            }
            "textDocument/hover" => {
                let params: HoverParams =
                    serde_json::from_value(request.params).into_diagnostic()?;
                let result = self.hover(params);
                self.send_ok(connection, request.id, result)?;
            }
            "textDocument/definition" => {
                let params: GotoDefinitionParams =
                    serde_json::from_value(request.params).into_diagnostic()?;
                let result = self.definition(params);
                self.send_ok(connection, request.id, result)?;
            }
            "textDocument/inlayHint" => {
                let params: InlayHintParams =
                    serde_json::from_value(request.params).into_diagnostic()?;
                let result = self.inlay_hints(params);
                self.send_ok(connection, request.id, result)?;
            }
            _ => {
                let response = Response::new_err(
                    request.id,
                    ErrorCode::MethodNotFound as i32,
                    "Method not found".to_string(),
                );
                connection
                    .sender
                    .send(Message::Response(response))
                    .into_diagnostic()?;
            }
        }
        Ok(())
    }

    fn send_ok<T: serde::Serialize>(
        &self,
        connection: &Connection,
        id: lsp_server::RequestId,
        result: T,
    ) -> Result<()> {
        let value = serde_json::to_value(result).into_diagnostic()?;
        let response = Response::new_ok(id, value);
        connection
            .sender
            .send(Message::Response(response))
            .into_diagnostic()?;
        Ok(())
    }

    fn completion(&self, params: CompletionParams) -> Option<CompletionResponse> {
        let uri = params.text_document_position.text_document.uri.to_string();
        let document = self.documents.get(&uri)?;
        let offset = document
            .analysis
            .line_index
            .offset_at(&document.text, params.text_document_position.position);
        let prefix = identifier_prefix_at(&document.text, offset);
        let is_member_completion = is_member_access_completion_context(&document.text, offset);

        let mut items = if is_member_completion {
            document.analysis.field_completion_items.clone()
        } else {
            document.analysis.completion_items.clone()
        };
        if !prefix.is_empty() {
            items.retain(|item| item.label.starts_with(&prefix));
        }
        items.sort_by(|a, b| a.label.cmp(&b.label));

        Some(CompletionResponse::Array(items))
    }

    fn hover(&self, params: HoverParams) -> Option<Hover> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .to_string();
        let document = self.documents.get(&uri)?;
        let offset = document.analysis.line_index.offset_at(
            &document.text,
            params.text_document_position_params.position,
        );

        let reference = document
            .analysis
            .symbol_index
            .find_reference_at(offset, document.text.len())?;

        let mut text = if let Some(binding_id) = reference.binding_id {
            if let Some(decl) = document
                .analysis
                .symbol_index
                .declarations_by_id
                .get(&binding_id)
            {
                decl.detail.clone()
            } else {
                reference.detail.clone().unwrap_or_default()
            }
        } else {
            reference.detail.clone().unwrap_or_default()
        };

        if let Some(extra) = &reference.detail
            && !extra.is_empty()
            && !text.contains(extra)
        {
            if !text.is_empty() {
                text.push('\n');
            }
            text.push_str(extra);
        }

        if text.is_empty() {
            return None;
        }

        Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::PlainText,
                value: text,
            }),
            range: Some(span_to_range(
                &document.text,
                &document.analysis.line_index,
                reference.span.start,
                reference.span.end.saturating_sub(reference.span.start),
            )),
        })
    }

    fn definition(&self, params: GotoDefinitionParams) -> Option<GotoDefinitionResponse> {
        let uri = params.text_document_position_params.text_document.uri;
        let uri_key = uri.to_string();
        let document = self.documents.get(&uri_key)?;

        let offset = document.analysis.line_index.offset_at(
            &document.text,
            params.text_document_position_params.position,
        );

        let reference = document
            .analysis
            .symbol_index
            .find_reference_at(offset, document.text.len())?;
        let binding_id = reference.binding_id?;
        let declaration = document
            .analysis
            .symbol_index
            .declarations_by_id
            .get(&binding_id)?;

        let range = span_to_range(
            &document.text,
            &document.analysis.line_index,
            declaration.span.start,
            declaration.span.end.saturating_sub(declaration.span.start),
        );

        Some(GotoDefinitionResponse::Scalar(Location { uri, range }))
    }

    fn publish_diagnostics(&self, connection: &Connection, uri: &str) -> Result<()> {
        let Some(document) = self.documents.get(uri) else {
            return Ok(());
        };

        self.send_diagnostics(
            connection,
            uri.to_string(),
            document.analysis.diagnostics.clone(),
            Some(document.version),
        )
    }

    fn send_diagnostics(
        &self,
        connection: &Connection,
        uri: String,
        diagnostics: Vec<Diagnostic>,
        version: Option<i32>,
    ) -> Result<()> {
        let params = PublishDiagnosticsParams {
            uri: uri.parse().into_diagnostic()?,
            diagnostics,
            version,
        };
        let notification = Notification::new(
            "textDocument/publishDiagnostics".to_string(),
            serde_json::to_value(params).into_diagnostic()?,
        );
        connection
            .sender
            .send(Message::Notification(notification))
            .into_diagnostic()?;
        Ok(())
    }

    fn inlay_hints(&self, params: InlayHintParams) -> Option<Vec<InlayHint>> {
        let uri = params.text_document.uri.to_string();
        let document = self.documents.get(&uri)?;

        let range_start = document
            .analysis
            .line_index
            .offset_at(&document.text, params.range.start);
        let range_end = document
            .analysis
            .line_index
            .offset_at(&document.text, params.range.end);
        let (start, end) = if range_start <= range_end {
            (range_start, range_end)
        } else {
            (range_end, range_start)
        };

        let hints = document
            .analysis
            .inlay_hints
            .iter()
            .filter(|hint| hint.offset >= start && hint.offset <= end)
            .map(|hint| InlayHint {
                position: document
                    .analysis
                    .line_index
                    .position_at(&document.text, hint.offset),
                label: InlayHintLabel::String(hint.label.clone()),
                kind: Some(InlayHintKind::TYPE),
                text_edits: None,
                tooltip: None,
                padding_left: Some(true),
                padding_right: Some(false),
                data: None,
            })
            .collect::<Vec<_>>();

        Some(hints)
    }
}

pub fn run_stdio_server() -> Result<()> {
    let (connection, io_threads) = Connection::stdio();

    let server_capabilities = ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::FULL),
                save: Some(TextDocumentSyncSaveOptions::Supported(true)),
                ..TextDocumentSyncOptions::default()
            },
        )),
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(vec![".".to_string()]),
            ..CompletionOptions::default()
        }),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        definition_provider: Some(OneOf::Left(true)),
        inlay_hint_provider: Some(OneOf::Left(true)),
        ..ServerCapabilities::default()
    };

    connection
        .initialize(serde_json::to_value(server_capabilities).into_diagnostic()?)
        .into_diagnostic()?;

    let mut server = YuuLanguageServer::default();
    for message in &connection.receiver {
        match message {
            Message::Notification(notification) => {
                if server.handle_notification(&connection, notification)? {
                    break;
                }
            }
            Message::Request(request) => {
                server.handle_request(&connection, request)?;
            }
            Message::Response(_) => {}
        }
    }

    drop(connection);
    io_threads.join().into_diagnostic()?;
    Ok(())
}

#[derive(Debug, Clone)]
struct DocumentAnalysis {
    diagnostics: Vec<Diagnostic>,
    symbol_index: SymbolIndex,
    completion_items: Vec<CompletionItem>,
    field_completion_items: Vec<CompletionItem>,
    inlay_hints: Vec<InlayHintData>,
    line_index: LineIndex,
}

#[derive(Debug, Clone)]
struct InlayHintData {
    offset: usize,
    label: String,
}

#[derive(Debug, Clone, Default)]
struct SymbolIndex {
    declarations_by_id: HashMap<NodeId, DeclarationInfo>,
    references: Vec<ReferenceInfo>,
    completion_symbols: Vec<CompletionSymbol>,
    completion_seen: HashSet<String>,
}

#[derive(Debug, Clone)]
struct DeclarationInfo {
    span: Span,
    detail: String,
}

#[derive(Debug, Clone)]
struct CompletionSymbol {
    label: String,
    kind: CompletionItemKind,
    detail: String,
}

#[derive(Debug, Clone)]
struct ReferenceInfo {
    span: Span,
    binding_id: Option<NodeId>,
    detail: Option<String>,
}

impl SymbolIndex {
    fn add_declaration(
        &mut self,
        id: NodeId,
        name: impl Into<String>,
        span: Span,
        detail: impl Into<String>,
        completion_kind: CompletionItemKind,
    ) {
        let name = name.into();
        let detail = detail.into();

        self.declarations_by_id
            .entry(id)
            .and_modify(|existing| {
                if span_length(&span) < span_length(&existing.span) {
                    existing.span = span.clone();
                    existing.detail = detail.clone();
                }
            })
            .or_insert_with(|| DeclarationInfo {
                span: span.clone(),
                detail: detail.clone(),
            });

        self.references.push(ReferenceInfo {
            span,
            binding_id: Some(id),
            detail: Some(detail.clone()),
        });

        let key = format!("{:?}:{}", completion_kind, name);
        if self.completion_seen.insert(key) {
            self.completion_symbols.push(CompletionSymbol {
                label: name,
                kind: completion_kind,
                detail,
            });
        }
    }

    fn add_reference(&mut self, span: Span, binding_id: Option<NodeId>, detail: Option<String>) {
        self.references.push(ReferenceInfo {
            span,
            binding_id,
            detail,
        });
    }

    fn find_reference_at(&self, offset: usize, source_len: usize) -> Option<&ReferenceInfo> {
        self.references
            .iter()
            .filter(|reference| span_contains(&reference.span, offset, source_len))
            .min_by_key(|reference| span_length(&reference.span))
    }
}

#[derive(Debug, Clone, Copy)]
enum DiagnosticsMode {
    Off,
    OnSave,
}

fn analyze_document(
    file_name: &str,
    source: &str,
    diagnostics_mode: DiagnosticsMode,
) -> DocumentAnalysis {
    let mut pipeline = Pipeline::new(source.to_string(), file_name.to_string());
    let line_index = LineIndex::new(source);

    let diagnostics = match diagnostics_mode {
        DiagnosticsMode::Off => {
            let _ = pipeline.prepare_editor_metadata();
            Vec::new()
        }
        DiagnosticsMode::OnSave => match pipeline.collect_errors() {
            Ok(errors) => errors
                .into_iter()
                .map(|error| yuu_error_to_diagnostic(error, source, &line_index))
                .collect(),
            Err(err) => vec![Diagnostic {
                range: Range {
                    start: Position {
                        line: 0,
                        character: 0,
                    },
                    end: Position {
                        line: 0,
                        character: 0,
                    },
                },
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(NumberOrString::String("InternalCompilerError".to_string())),
                code_description: None,
                source: Some("yuu".to_string()),
                message: format!("Compiler failed while producing diagnostics: {}", err),
                related_information: None,
                tags: None,
                data: None,
            }],
        },
    };

    let mut symbol_index = SymbolIndex::default();
    let mut inlay_hints = Vec::new();
    if let Ok(ast) = pipeline.calc_ast() {
        symbol_index = build_symbol_index(ast);
        inlay_hints = build_inlay_hints(ast, source, &line_index);
    }

    let completion_items = build_completion_items(&symbol_index);
    let field_completion_items = build_field_completion_items(&symbol_index);
    DocumentAnalysis {
        diagnostics,
        symbol_index,
        completion_items,
        field_completion_items,
        inlay_hints,
        line_index,
    }
}

fn build_symbol_index(ast: &AST) -> SymbolIndex {
    let mut index = SymbolIndex::default();
    for structural in &ast.structurals {
        collect_structural(&mut index, structural.as_ref(), &structural.metadata);
    }
    index
}

fn build_inlay_hints(ast: &AST, source: &str, line_index: &LineIndex) -> Vec<InlayHintData> {
    let mut hints = Vec::new();
    for structural in &ast.structurals {
        collect_inlay_hints_structural(&mut hints, structural.as_ref(), &structural.metadata);
    }

    // Deduplicate by position + label to avoid duplicate hints from overlapping nodes.
    let mut seen = HashSet::new();
    hints.retain(|hint| seen.insert((hint.offset, hint.label.clone())));

    // Ensure offsets are valid char boundaries for reliable UTF-16 mapping.
    for hint in &mut hints {
        hint.offset = clamp_to_char_boundary(source, hint.offset);
        let _ = line_index.position_at(source, hint.offset);
    }

    hints.sort_by_key(|hint| hint.offset);
    hints
}

fn collect_inlay_hints_structural(
    hints: &mut Vec<InlayHintData>,
    structural: &StructuralNode,
    metadata: &StructuralMetadata,
) {
    match structural {
        StructuralNode::FuncDef(def) => {
            collect_inlay_hints_block(hints, &def.body, metadata);
        }
        _ => {}
    }
}

fn collect_inlay_hints_block(
    hints: &mut Vec<InlayHintData>,
    block: &BlockStmt,
    metadata: &StructuralMetadata,
) {
    for stmt in &block.body {
        collect_inlay_hints_stmt(hints, stmt, metadata);
    }
}

fn collect_inlay_hints_stmt(
    hints: &mut Vec<InlayHintData>,
    stmt: &StmtNode,
    metadata: &StructuralMetadata,
) {
    match stmt {
        StmtNode::Let(let_stmt) => {
            add_binding_type_hint(hints, &let_stmt.binding, metadata);
            collect_inlay_hints_expr(hints, &let_stmt.expr, metadata);
        }
        StmtNode::Def(def_stmt) => {
            let binding_id = metadata.binding_table.get(&def_stmt.ident.id).copied();
            if let Some(binding_id) = binding_id
                && let Some(ty) = metadata.type_info_table.get(binding_id)
            {
                hints.push(InlayHintData {
                    offset: def_stmt.ident.span.end,
                    label: format!(": {}", ty),
                });
            }
            collect_inlay_hints_expr(hints, &def_stmt.expr, metadata);
        }
        StmtNode::If(if_stmt) => {
            collect_inlay_hints_expr(hints, &if_stmt.if_block.condition, metadata);
            collect_inlay_hints_block(hints, &if_stmt.if_block.body, metadata);
            for else_if in &if_stmt.else_if_blocks {
                collect_inlay_hints_expr(hints, &else_if.condition, metadata);
                collect_inlay_hints_block(hints, &else_if.body, metadata);
            }
            if let Some(else_block) = &if_stmt.else_block {
                collect_inlay_hints_block(hints, else_block, metadata);
            }
        }
        StmtNode::While(while_stmt) => {
            collect_inlay_hints_expr(hints, &while_stmt.condition_block.condition, metadata);
            collect_inlay_hints_block(hints, &while_stmt.condition_block.body, metadata);
        }
        StmtNode::Match(match_stmt) => {
            collect_inlay_hints_expr(hints, &match_stmt.scrutinee, metadata);
            for arm in &match_stmt.arms {
                if let Some(binding) = arm.pattern.binding() {
                    add_binding_type_hint(hints, binding, metadata);
                }
                collect_inlay_hints_block(hints, &arm.body, metadata);
            }
            if let Some(default_case) = &match_stmt.default_case {
                collect_inlay_hints_block(hints, default_case, metadata);
            }
        }
        StmtNode::Block(inner) => collect_inlay_hints_block(hints, inner, metadata),
        StmtNode::Atomic(expr) => collect_inlay_hints_expr(hints, expr, metadata),
        StmtNode::Return(ret) => {
            if let Some(expr) = &ret.expr {
                collect_inlay_hints_expr(hints, expr, metadata);
            }
        }
        StmtNode::Defer(defer_stmt) => {
            collect_inlay_hints_expr(hints, &defer_stmt.expr, metadata);
        }
        StmtNode::Decl(_) | StmtNode::Break(_) | StmtNode::Error(_) | StmtNode::LuaMeta(_) => {}
    }
}

fn add_binding_type_hint(
    hints: &mut Vec<InlayHintData>,
    binding: &BindingNode,
    metadata: &StructuralMetadata,
) {
    match binding {
        BindingNode::Ident(ident) => {
            if let Some(ty) = metadata.type_info_table.get(ident.id) {
                hints.push(InlayHintData {
                    offset: ident.span.end,
                    label: format!(": {}", ty),
                });
            }
        }
        BindingNode::LuaMeta(_) => {}
    }
}

fn collect_inlay_hints_expr(
    hints: &mut Vec<InlayHintData>,
    expr: &ExprNode,
    metadata: &StructuralMetadata,
) {
    match expr {
        ExprNode::Binary(binary) => {
            collect_inlay_hints_expr(hints, &binary.left, metadata);
            collect_inlay_hints_expr(hints, &binary.right, metadata);
        }
        ExprNode::Unary(unary) => collect_inlay_hints_expr(hints, &unary.expr, metadata),
        ExprNode::FuncCall(call) => {
            collect_inlay_hints_expr(hints, &call.lhs, metadata);
            for arg in &call.args {
                collect_inlay_hints_expr(hints, arg, metadata);
            }
        }
        ExprNode::Assignment(assign) => {
            collect_inlay_hints_expr(hints, &assign.lhs, metadata);
            collect_inlay_hints_expr(hints, &assign.rhs, metadata);
        }
        ExprNode::StructInstantiation(inst) => {
            for (_field, expr) in &inst.fields {
                collect_inlay_hints_expr(hints, expr, metadata);
            }
        }
        ExprNode::EnumInstantiation(inst) => {
            if let Some(expr) = &inst.data {
                collect_inlay_hints_expr(hints, expr, metadata);
            }
        }
        ExprNode::MemberAccess(access) => collect_inlay_hints_expr(hints, &access.lhs, metadata),
        ExprNode::Deref(deref) => collect_inlay_hints_expr(hints, &deref.expr, metadata),
        ExprNode::AddressOf(address_of) => {
            collect_inlay_hints_expr(hints, &address_of.expr, metadata)
        }
        ExprNode::HeapAlloc(heap) => collect_inlay_hints_expr(hints, &heap.expr, metadata),
        ExprNode::Array(array) => {
            if let Some(init) = &array.init_expr {
                collect_inlay_hints_expr(hints, init, metadata);
            }
            collect_inlay_hints_expr(hints, &array.size, metadata);
        }
        ExprNode::ArrayLiteral(array_lit) => {
            for expr in &array_lit.elements {
                collect_inlay_hints_expr(hints, expr, metadata);
            }
        }
        ExprNode::Cast(cast) => collect_inlay_hints_expr(hints, &cast.expr, metadata),
        ExprNode::Literal(_) | ExprNode::Ident(_) | ExprNode::LuaMeta(_) => {}
    }
}

fn collect_structural(
    index: &mut SymbolIndex,
    structural: &StructuralNode,
    metadata: &StructuralMetadata,
) {
    match structural {
        StructuralNode::FuncDecl(decl) => {
            let detail = function_detail(decl);
            index.add_declaration(
                decl.id,
                decl.name.as_str(),
                decl.span.clone(),
                detail,
                CompletionItemKind::FUNCTION,
            );
            for arg in &decl.args {
                let arg_detail = format!(
                    "parameter {}: {}",
                    arg.name.as_str(),
                    type_node_to_string(&arg.ty)
                );
                index.add_declaration(
                    arg.id,
                    arg.name.as_str(),
                    arg.span.clone(),
                    arg_detail,
                    CompletionItemKind::VARIABLE,
                );
            }
        }
        StructuralNode::FuncDef(def) => {
            let detail = function_detail(&def.decl);
            index.add_declaration(
                def.id,
                def.decl.name.as_str(),
                def.decl.span.clone(),
                detail.clone(),
                CompletionItemKind::FUNCTION,
            );
            index.add_declaration(
                def.decl.id,
                def.decl.name.as_str(),
                def.decl.span.clone(),
                detail,
                CompletionItemKind::FUNCTION,
            );
            for arg in &def.decl.args {
                let arg_detail = format!(
                    "parameter {}: {}",
                    arg.name.as_str(),
                    type_node_to_string(&arg.ty)
                );
                index.add_declaration(
                    arg.id,
                    arg.name.as_str(),
                    arg.span.clone(),
                    arg_detail,
                    CompletionItemKind::VARIABLE,
                );
            }
            collect_block(index, &def.body, metadata);
        }
        StructuralNode::StructDecl(struct_decl) => {
            let detail = format!("struct {}", struct_decl.name.as_str());
            index.add_declaration(
                struct_decl.id,
                struct_decl.name.as_str(),
                struct_decl.span.clone(),
                detail,
                CompletionItemKind::STRUCT,
            );
        }
        StructuralNode::StructDef(struct_def) => {
            let detail = format!("struct {}", struct_def.decl.name.as_str());
            index.add_declaration(
                struct_def.id,
                struct_def.decl.name.as_str(),
                struct_def.decl.span.clone(),
                detail.clone(),
                CompletionItemKind::STRUCT,
            );
            index.add_declaration(
                struct_def.decl.id,
                struct_def.decl.name.as_str(),
                struct_def.decl.span.clone(),
                detail,
                CompletionItemKind::STRUCT,
            );
            for field in &struct_def.fields {
                let field_detail = format!(
                    "field {}: {}",
                    field.name.as_str(),
                    type_node_to_string(&field.ty)
                );
                index.add_declaration(
                    field.id,
                    field.name.as_str(),
                    field.span.clone(),
                    field_detail,
                    CompletionItemKind::FIELD,
                );
            }
        }
        StructuralNode::EnumDef(enum_def) => {
            let detail = format!("enum {}", enum_def.decl.name.as_str());
            index.add_declaration(
                enum_def.id,
                enum_def.decl.name.as_str(),
                enum_def.decl.span.clone(),
                detail.clone(),
                CompletionItemKind::ENUM,
            );
            index.add_declaration(
                enum_def.decl.id,
                enum_def.decl.name.as_str(),
                enum_def.decl.span.clone(),
                detail,
                CompletionItemKind::ENUM,
            );

            for variant in &enum_def.variants {
                let variant_detail = if let Some(data_ty) = &variant.data_type {
                    format!(
                        "variant {}({})",
                        variant.name.as_str(),
                        type_node_to_string(data_ty)
                    )
                } else {
                    format!("variant {}", variant.name.as_str())
                };
                index.add_declaration(
                    variant.id,
                    variant.name.as_str(),
                    variant.span.clone(),
                    variant_detail,
                    CompletionItemKind::ENUM_MEMBER,
                );
            }
        }
        StructuralNode::Error(_) | StructuralNode::LuaMeta(_) => {}
    }
}

fn collect_block(index: &mut SymbolIndex, block: &BlockStmt, metadata: &StructuralMetadata) {
    for stmt in &block.body {
        collect_stmt(index, stmt, metadata);
    }
}

fn collect_stmt(index: &mut SymbolIndex, stmt: &StmtNode, metadata: &StructuralMetadata) {
    match stmt {
        StmtNode::Let(let_stmt) => {
            collect_binding(index, &let_stmt.binding, metadata);
            collect_expr(index, &let_stmt.expr, metadata);
        }
        StmtNode::Atomic(expr) => collect_expr(index, expr, metadata),
        StmtNode::Break(_) => {}
        StmtNode::Return(return_stmt) => {
            if let Some(expr) = &return_stmt.expr {
                collect_expr(index, expr, metadata);
            }
        }
        StmtNode::Defer(defer_stmt) => collect_expr(index, &defer_stmt.expr, metadata),
        StmtNode::Match(match_stmt) => {
            collect_expr(index, &match_stmt.scrutinee, metadata);
            for arm in &match_stmt.arms {
                if let Some(binding) = arm.pattern.binding() {
                    collect_binding(index, binding, metadata);
                }
                collect_block(index, &arm.body, metadata);
            }
            if let Some(default_case) = &match_stmt.default_case {
                collect_block(index, default_case, metadata);
            }
        }
        StmtNode::If(if_stmt) => {
            collect_expr(index, &if_stmt.if_block.condition, metadata);
            collect_block(index, &if_stmt.if_block.body, metadata);
            for else_if in &if_stmt.else_if_blocks {
                collect_expr(index, &else_if.condition, metadata);
                collect_block(index, &else_if.body, metadata);
            }
            if let Some(else_block) = &if_stmt.else_block {
                collect_block(index, else_block, metadata);
            }
        }
        StmtNode::While(while_stmt) => {
            collect_expr(index, &while_stmt.condition_block.condition, metadata);
            collect_block(index, &while_stmt.condition_block.body, metadata);
        }
        StmtNode::Block(block_stmt) => collect_block(index, block_stmt, metadata),
        StmtNode::Decl(decl_stmt) => {
            let detail = variable_detail("decl", &decl_stmt.ident, metadata);
            index.add_declaration(
                decl_stmt.ident.id,
                decl_stmt.ident.name.as_str(),
                decl_stmt.ident.span.clone(),
                detail,
                CompletionItemKind::VARIABLE,
            );
        }
        StmtNode::Def(def_stmt) => {
            let binding_id = metadata.binding_table.get(&def_stmt.ident.id).copied();
            let detail = metadata
                .type_info_table
                .get(def_stmt.ident.id)
                .map(|ty| format!("{}: {}", def_stmt.ident.name.as_str(), ty));
            index.add_reference(def_stmt.ident.span.clone(), binding_id, detail);
            collect_expr(index, &def_stmt.expr, metadata);
        }
        StmtNode::Error(_) | StmtNode::LuaMeta(_) => {}
    }
}

fn collect_binding(index: &mut SymbolIndex, binding: &BindingNode, metadata: &StructuralMetadata) {
    match binding {
        BindingNode::Ident(ident) => {
            let detail = variable_detail("let", ident, metadata);
            index.add_declaration(
                ident.id,
                ident.name.as_str(),
                ident.span.clone(),
                detail,
                CompletionItemKind::VARIABLE,
            );
        }
        BindingNode::LuaMeta(_) => {}
    }
}

fn collect_expr(index: &mut SymbolIndex, expr: &ExprNode, metadata: &StructuralMetadata) {
    match expr {
        ExprNode::Literal(_) => {}
        ExprNode::Binary(binary) => {
            collect_expr(index, &binary.left, metadata);
            collect_expr(index, &binary.right, metadata);
        }
        ExprNode::Unary(unary) => collect_expr(index, &unary.expr, metadata),
        ExprNode::Ident(ident) => {
            let binding_id = metadata.binding_table.get(&ident.id).copied();
            let detail = metadata
                .type_info_table
                .get(ident.id)
                .map(|ty| format!("{}: {}", ident.ident.as_str(), ty));
            index.add_reference(ident.span.clone(), binding_id, detail);
        }
        ExprNode::FuncCall(call) => {
            collect_expr(index, &call.lhs, metadata);
            for arg in &call.args {
                collect_expr(index, arg, metadata);
            }
        }
        ExprNode::Assignment(assignment) => {
            collect_expr(index, &assignment.lhs, metadata);
            collect_expr(index, &assignment.rhs, metadata);
        }
        ExprNode::StructInstantiation(instantiation) => {
            for (_field, expr) in &instantiation.fields {
                collect_expr(index, expr, metadata);
            }
        }
        ExprNode::EnumInstantiation(instantiation) => {
            if let Some(data) = &instantiation.data {
                collect_expr(index, data, metadata);
            }
        }
        ExprNode::MemberAccess(access) => {
            collect_expr(index, &access.lhs, metadata);
            index.add_reference(
                access.field.span.clone(),
                None,
                Some(format!("field {}", access.field.name.as_str())),
            );
        }
        ExprNode::Deref(deref) => collect_expr(index, &deref.expr, metadata),
        ExprNode::AddressOf(address_of) => collect_expr(index, &address_of.expr, metadata),
        ExprNode::HeapAlloc(heap_alloc) => collect_expr(index, &heap_alloc.expr, metadata),
        ExprNode::Array(array_expr) => {
            if let Some(init_expr) = &array_expr.init_expr {
                collect_expr(index, init_expr, metadata);
            }
            collect_expr(index, &array_expr.size, metadata);
        }
        ExprNode::ArrayLiteral(array_lit) => {
            for element in &array_lit.elements {
                collect_expr(index, element, metadata);
            }
        }
        ExprNode::Cast(cast) => collect_expr(index, &cast.expr, metadata),
        ExprNode::LuaMeta(_) => {}
    }
}

fn variable_detail(prefix: &str, ident: &IdentBinding, metadata: &StructuralMetadata) -> String {
    if let Some(ty) = metadata.type_info_table.get(ident.id) {
        format!("{} {}: {}", prefix, ident.name.as_str(), ty)
    } else {
        format!("{} {}", prefix, ident.name.as_str())
    }
}

fn function_detail(decl: &FuncDeclStructural) -> String {
    let args = decl
        .args
        .iter()
        .map(arg_to_string)
        .collect::<Vec<_>>()
        .join(", ");
    let ret = decl
        .ret_ty
        .as_ref()
        .map(|ty| type_node_to_string(ty))
        .unwrap_or_else(|| "nil".to_string());

    let extern_prefix = if decl.is_extern { "extern " } else { "" };
    format!(
        "{}fn {}({}) -> {}",
        extern_prefix,
        decl.name.as_str(),
        args,
        ret
    )
}

fn arg_to_string(arg: &Arg) -> String {
    let mutability = if arg.is_mut { "mut " } else { "" };
    format!(
        "{}{}: {}",
        mutability,
        arg.name.as_str(),
        type_node_to_string(&arg.ty)
    )
}

fn type_node_to_string(ty: &TypeNode) -> String {
    match ty {
        TypeNode::BuiltIn(builtin) => builtin.kind.to_string(),
        TypeNode::Ident(ident) => ident.name.as_str().to_string(),
        TypeNode::Pointer(pointer) => format!("*{}", type_node_to_string(&pointer.pointee)),
        TypeNode::Array(array_ty) => {
            format!("[{}; _]", type_node_to_string(&array_ty.element_type))
        }
        TypeNode::LuaMeta(meta) => {
            if meta.lua_meta.is_active
                && let Some(ty) = &meta.lua_meta.value
            {
                type_node_to_string(ty)
            } else {
                "meta-type".to_string()
            }
        }
    }
}

fn build_completion_items(index: &SymbolIndex) -> Vec<CompletionItem> {
    let mut items = KEYWORD_COMPLETIONS
        .iter()
        .map(|keyword| CompletionItem {
            label: (*keyword).to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some("keyword".to_string()),
            ..CompletionItem::default()
        })
        .collect::<Vec<_>>();

    items.extend(
        index
            .completion_symbols
            .iter()
            .map(|symbol| CompletionItem {
                label: symbol.label.clone(),
                kind: Some(symbol.kind),
                detail: Some(symbol.detail.clone()),
                ..CompletionItem::default()
            }),
    );

    items
}

fn build_field_completion_items(index: &SymbolIndex) -> Vec<CompletionItem> {
    index
        .completion_symbols
        .iter()
        .filter(|symbol| symbol.kind == CompletionItemKind::FIELD)
        .map(|symbol| CompletionItem {
            label: symbol.label.clone(),
            kind: Some(symbol.kind),
            detail: Some(symbol.detail.clone()),
            ..CompletionItem::default()
        })
        .collect()
}

trait MatchArmPatternBinding {
    fn binding(&self) -> Option<&BindingNode>;
}

impl MatchArmPatternBinding for crate::pass_parse::RefutablePatternNode {
    fn binding(&self) -> Option<&BindingNode> {
        match self {
            crate::pass_parse::RefutablePatternNode::Enum(enum_pattern) => enum_pattern
                .binding
                .as_ref()
                .map(|binding| binding.as_ref()),
        }
    }
}

fn yuu_error_to_diagnostic(error: YuuError, source: &str, line_index: &LineIndex) -> Diagnostic {
    let (offset, len) = error
        .labels
        .last()
        .map(|label| (label.offset(), label.len()))
        .unwrap_or((0, 0));

    let mut message = error.message;
    if let Some(help) = error.help {
        message.push('\n');
        message.push_str(&help);
    }

    Diagnostic {
        range: span_to_range(source, line_index, offset, len),
        severity: Some(DiagnosticSeverity::ERROR),
        code: Some(NumberOrString::String(format!("{:?}", error.kind))),
        code_description: None,
        source: Some("yuu".to_string()),
        message,
        related_information: None,
        tags: None,
        data: None,
    }
}

fn span_to_range(source: &str, line_index: &LineIndex, offset: usize, len: usize) -> Range {
    let start = clamp_to_char_boundary(source, offset);
    let end_unclamped = start.saturating_add(len).min(source.len());
    let end = clamp_to_char_boundary(source, end_unclamped);

    Range {
        start: line_index.position_at(source, start),
        end: line_index.position_at(source, end),
    }
}

fn span_contains(span: &Span, offset: usize, source_len: usize) -> bool {
    let start = span.start.min(source_len);
    let mut end = span.end.min(source_len);
    if end <= start {
        end = start.saturating_add(1).min(source_len);
    }
    offset >= start && offset < end
}

fn span_length(span: &Span) -> usize {
    span.end.saturating_sub(span.start)
}

fn clamp_to_char_boundary(text: &str, mut offset: usize) -> usize {
    offset = offset.min(text.len());
    while offset > 0 && !text.is_char_boundary(offset) {
        offset -= 1;
    }
    offset
}

fn identifier_prefix_at(source: &str, offset: usize) -> String {
    let mut start = clamp_to_char_boundary(source, offset);

    while start > 0 {
        let mut prev = start - 1;
        while prev > 0 && !source.is_char_boundary(prev) {
            prev -= 1;
        }

        let Some(ch) = source[prev..start].chars().next() else {
            break;
        };
        if ch.is_ascii_alphanumeric() || ch == '_' {
            start = prev;
        } else {
            break;
        }
    }

    source[start..offset.min(source.len())].to_string()
}

fn is_member_access_completion_context(source: &str, offset: usize) -> bool {
    if offset == 0 || source.is_empty() {
        return false;
    }

    let offset = offset.min(source.len());
    let mut start = offset;

    while start > 0 {
        let ch = source.as_bytes()[start - 1] as char;
        if ch.is_ascii_alphanumeric() || ch == '_' {
            start -= 1;
        } else {
            break;
        }
    }

    let mut idx = start;
    while idx > 0 {
        let ch = source.as_bytes()[idx - 1] as char;
        if ch.is_ascii_whitespace() {
            idx -= 1;
        } else {
            break;
        }
    }

    if idx == 0 || source.as_bytes()[idx - 1] as char != '.' {
        return false;
    }

    if idx < 2 {
        return false;
    }

    let lhs_char = source.as_bytes()[idx - 2] as char;
    !lhs_char.is_ascii_whitespace()
}

#[derive(Debug, Clone)]
struct LineIndex {
    line_starts: Vec<usize>,
}

impl LineIndex {
    fn new(source: &str) -> Self {
        let mut line_starts = Vec::with_capacity(source.lines().count() + 1);
        line_starts.push(0);

        for (idx, ch) in source.char_indices() {
            if ch == '\n' {
                line_starts.push(idx + 1);
            }
        }

        Self { line_starts }
    }

    fn position_at(&self, source: &str, offset: usize) -> Position {
        let offset = offset.min(source.len());
        let line_index = match self.line_starts.binary_search(&offset) {
            Ok(idx) => idx,
            Err(insert_idx) => insert_idx.saturating_sub(1),
        };
        let line_start = self.line_starts[line_index];

        Position {
            line: line_index as u32,
            character: source[line_start..offset].encode_utf16().count() as u32,
        }
    }

    fn offset_at(&self, source: &str, position: Position) -> usize {
        let line = position.line as usize;
        let Some(&line_start) = self.line_starts.get(line) else {
            return source.len();
        };
        let line_end = self
            .line_starts
            .get(line + 1)
            .copied()
            .unwrap_or(source.len());

        let mut utf16_units = 0_u32;
        let slice = &source[line_start..line_end];
        for (rel_idx, ch) in slice.char_indices() {
            let ch_units = ch.len_utf16() as u32;
            if utf16_units + ch_units > position.character {
                return line_start + rel_idx;
            }
            utf16_units += ch_units;
        }

        line_end
    }
}
