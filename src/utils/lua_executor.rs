use mlua::{Lua, LuaSerdeExt, Value};
use serde::{Deserialize, Serialize};
use logos::Span;

use crate::pass_parse::ast::*;
use crate::pass_diagnostics::{YuuError, error::ErrorKind};
use crate::pass_parse::SourceInfo;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LuaContext {
    pub pass_name: String,
}

#[derive(Debug)]
pub enum LuaCommand {
    Emit,
    EmitAndSeal,
    Seal,
    Nil,
}

pub struct LuaExecutor {
    lua: Lua,
}

impl LuaExecutor {
    pub fn new() -> mlua::Result<Self> {
        let lua = Lua::new();
        Ok(Self { lua })
    }

    fn lua_error_to_yuu_error(
        &self,
        err: mlua::Error,
        span: &Span,
        src: &SourceInfo,
        lua_code: &str,
    ) -> YuuError {
        YuuError::builder()
            .kind(ErrorKind::LuaPluginError)
            .message(format!("Lua execution failed: {}", err))
            .source(src.source.clone(), src.file_name.clone())
            .span((span.start, span.end - span.start), "lua plugin error here")
            .help(format!("Lua code:\n{}", lua_code))
            .build()
    }

    pub fn execute_structural(
        &self,
        lua_code: &str,
        context: &LuaContext,
        span: &Span,
        src: &SourceInfo,
    ) -> Result<(LuaCommand, Option<Vec<StructuralNode>>), YuuError> {
        let globals = self.lua.globals();
        globals.set("pass_name", context.pass_name.clone())
            .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?;

        let result = self.lua.load(lua_code).eval::<Value>()
            .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?;

        match result {
            Value::Nil => Ok((LuaCommand::Nil, None)),
            Value::Table(table) => {
                let command: String = table.get("command")
                    .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?;

                match command.as_str() {
                    "Emit" => {
                        let node: Vec<StructuralNode> = self.lua.from_value(table.get("node")
                            .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?)
                            .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?;
                        Ok((LuaCommand::Emit, Some(node)))
                    }
                    "EmitAndSeal" => {
                        let node: Vec<StructuralNode> = self.lua.from_value(table.get("node")
                            .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?)
                            .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?;
                        Ok((LuaCommand::EmitAndSeal, Some(node)))
                    }
                    "Seal" => Ok((LuaCommand::Seal, None)),
                    _ => Err(YuuError::builder()
                        .kind(ErrorKind::LuaPluginError)
                        .message(format!("Unknown command: '{}'", command))
                        .source(src.source.clone(), src.file_name.clone())
                        .span((span.start, span.end - span.start), "lua plugin error")
                        .help(format!("Valid commands are: 'Emit', 'EmitAndSeal', 'Seal', or return nil"))
                        .build()),
                }
            }
            _ => Err(YuuError::builder()
                .kind(ErrorKind::LuaPluginError)
                .message("Lua must return nil or a table with command field")
                .source(src.source.clone(), src.file_name.clone())
                .span((span.start, span.end - span.start), "lua plugin error")
                .help("Return either nil, or { command = \"Emit\", node = ... }")
                .build()),
        }
    }

    pub fn execute_stmt(
        &self,
        lua_code: &str,
        context: &LuaContext,
        span: &Span,
        src: &SourceInfo,
    ) -> Result<(LuaCommand, Option<Vec<StmtNode>>), YuuError> {
        let globals = self.lua.globals();
        globals.set("pass_name", context.pass_name.clone())
            .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?;

        let result = self.lua.load(lua_code).eval::<Value>()
            .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?;

        match result {
            Value::Nil => Ok((LuaCommand::Nil, None)),
            Value::Table(table) => {
                let command: String = table.get("command")
                    .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?;

                match command.as_str() {
                    "Emit" => {
                        let node: Vec<StmtNode> = self.lua.from_value(table.get("node")
                            .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?)
                            .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?;
                        Ok((LuaCommand::Emit, Some(node)))
                    }
                    "EmitAndSeal" => {
                        let node: Vec<StmtNode> = self.lua.from_value(table.get("node")
                            .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?)
                            .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?;
                        Ok((LuaCommand::EmitAndSeal, Some(node)))
                    }
                    "Seal" => Ok((LuaCommand::Seal, None)),
                    _ => Err(YuuError::builder()
                        .kind(ErrorKind::LuaPluginError)
                        .message(format!("Unknown command: '{}'", command))
                        .source(src.source.clone(), src.file_name.clone())
                        .span((span.start, span.end - span.start), "lua plugin error")
                        .help("Valid commands are: 'Emit', 'EmitAndSeal', 'Seal', or return nil")
                        .build()),
                }
            }
            _ => Err(YuuError::builder()
                .kind(ErrorKind::LuaPluginError)
                .message("Lua must return nil or a table with command field")
                .source(src.source.clone(), src.file_name.clone())
                .span((span.start, span.end - span.start), "lua plugin error")
                .help("Return either nil, or { command = \"Emit\", node = ... }")
                .build()),
        }
    }

    pub fn execute_expr(
        &self,
        lua_code: &str,
        context: &LuaContext,
        span: &Span,
        src: &SourceInfo,
    ) -> Result<(LuaCommand, Option<ExprNode>), YuuError> {
        let globals = self.lua.globals();
        globals.set("pass_name", context.pass_name.clone())
            .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?;

        let result = self.lua.load(lua_code).eval::<Value>()
            .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?;

        match result {
            Value::Nil => Ok((LuaCommand::Nil, None)),
            Value::Table(table) => {
                let command: String = table.get("command")
                    .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?;

                match command.as_str() {
                    "Emit" => {
                        let node: ExprNode = self.lua.from_value(table.get("node")
                            .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?)
                            .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?;
                        Ok((LuaCommand::Emit, Some(node)))
                    }
                    "EmitAndSeal" => {
                        let node: ExprNode = self.lua.from_value(table.get("node")
                            .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?)
                            .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?;
                        Ok((LuaCommand::EmitAndSeal, Some(node)))
                    }
                    "Seal" => Ok((LuaCommand::Seal, None)),
                    _ => Err(YuuError::builder()
                        .kind(ErrorKind::LuaPluginError)
                        .message(format!("Unknown command: '{}'", command))
                        .source(src.source.clone(), src.file_name.clone())
                        .span((span.start, span.end - span.start), "lua plugin error")
                        .help("Valid commands are: 'Emit', 'EmitAndSeal', 'Seal', or return nil")
                        .build()),
                }
            }
            _ => Err(YuuError::builder()
                .kind(ErrorKind::LuaPluginError)
                .message("Lua must return nil or a table with command field")
                .source(src.source.clone(), src.file_name.clone())
                .span((span.start, span.end - span.start), "lua plugin error")
                .help("Return either nil, or { command = \"Emit\", node = ... }")
                .build()),
        }
    }

    pub fn execute_type(
        &self,
        lua_code: &str,
        context: &LuaContext,
        span: &Span,
        src: &SourceInfo,
    ) -> Result<(LuaCommand, Option<TypeNode>), YuuError> {
        let globals = self.lua.globals();
        globals.set("pass_name", context.pass_name.clone())
            .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?;

        let result = self.lua.load(lua_code).eval::<Value>()
            .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?;

        match result {
            Value::Nil => Ok((LuaCommand::Nil, None)),
            Value::Table(table) => {
                let command: String = table.get("command")
                    .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?;

                match command.as_str() {
                    "Emit" => {
                        let node: TypeNode = self.lua.from_value(table.get("node")
                            .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?)
                            .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?;
                        Ok((LuaCommand::Emit, Some(node)))
                    }
                    "EmitAndSeal" => {
                        let node: TypeNode = self.lua.from_value(table.get("node")
                            .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?)
                            .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?;
                        Ok((LuaCommand::EmitAndSeal, Some(node)))
                    }
                    "Seal" => Ok((LuaCommand::Seal, None)),
                    _ => Err(YuuError::builder()
                        .kind(ErrorKind::LuaPluginError)
                        .message(format!("Unknown command: '{}'", command))
                        .source(src.source.clone(), src.file_name.clone())
                        .span((span.start, span.end - span.start), "lua plugin error")
                        .help("Valid commands are: 'Emit', 'EmitAndSeal', 'Seal', or return nil")
                        .build()),
                }
            }
            _ => Err(YuuError::builder()
                .kind(ErrorKind::LuaPluginError)
                .message("Lua must return nil or a table with command field")
                .source(src.source.clone(), src.file_name.clone())
                .span((span.start, span.end - span.start), "lua plugin error")
                .help("Return either nil, or { command = \"Emit\", node = ... }")
                .build()),
        }
    }

    pub fn execute_binding(
        &self,
        lua_code: &str,
        context: &LuaContext,
        span: &Span,
        src: &SourceInfo,
    ) -> Result<(LuaCommand, Option<BindingNode>), YuuError> {
        let globals = self.lua.globals();
        globals.set("pass_name", context.pass_name.clone())
            .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?;

        let result = self.lua.load(lua_code).eval::<Value>()
            .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?;

        match result {
            Value::Nil => Ok((LuaCommand::Nil, None)),
            Value::Table(table) => {
                let command: String = table.get("command")
                    .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?;

                match command.as_str() {
                    "Emit" => {
                        let node: BindingNode = self.lua.from_value(table.get("node")
                            .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?)
                            .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?;
                        Ok((LuaCommand::Emit, Some(node)))
                    }
                    "EmitAndSeal" => {
                        let node: BindingNode = self.lua.from_value(table.get("node")
                            .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?)
                            .map_err(|e| self.lua_error_to_yuu_error(e, span, src, lua_code))?;
                        Ok((LuaCommand::EmitAndSeal, Some(node)))
                    }
                    "Seal" => Ok((LuaCommand::Seal, None)),
                    _ => Err(YuuError::builder()
                        .kind(ErrorKind::LuaPluginError)
                        .message(format!("Unknown command: '{}'", command))
                        .source(src.source.clone(), src.file_name.clone())
                        .span((span.start, span.end - span.start), "lua plugin error")
                        .help("Valid commands are: 'Emit', 'EmitAndSeal', 'Seal', or return nil")
                        .build()),
                }
            }
            _ => Err(YuuError::builder()
                .kind(ErrorKind::LuaPluginError)
                .message("Lua must return nil or a table with command field")
                .source(src.source.clone(), src.file_name.clone())
                .span((span.start, span.end - span.start), "lua plugin error")
                .help("Return either nil, or { command = \"Emit\", node = ... }")
                .build()),
        }
    }
}

impl Default for LuaExecutor {
    fn default() -> Self {
        Self::new().expect("Failed to create Lua executor")
    }
}
