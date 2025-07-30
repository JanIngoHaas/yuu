use std::fmt::Display;

use logos::Logos;
use serde::{Deserialize, Serialize};
use ustr::{Ustr, ustr};

// Helper function for parsing i64 with different bases
fn parse_integer(s: &str) -> Option<Integer> {
    let s = s.trim_end_matches('i');
    match s.get(..2) {
        Some("0b") => i64::from_str_radix(&s[2..], 2).ok().map(Integer::I64),
        Some("0o") => i64::from_str_radix(&s[2..], 8).ok().map(Integer::I64),
        Some("0x") => i64::from_str_radix(&s[2..], 16).ok().map(Integer::I64),
        _ => s.parse().ok().map(Integer::I64),
    }
}

#[derive(Serialize, Deserialize, Logos, Debug, PartialEq, Copy, Clone)]
pub enum Integer {
    I64(i64),
    // CannotParseI64_TooManyDigits,
    // U64(u64),
    // U32(u32),
    // U16(u16),
    // U8(u8),
    // I32(i32),
    // I16(i16),
    // I8(i8),
    // UIdx(usize),
    // Idx(isize),
}

#[derive(Serialize, Deserialize, Logos, Debug, PartialEq, Clone)]
#[logos(skip r"\s+")] // Skip whitespace
#[logos(skip r"//.*")]
#[logos(skip r"/\*(?:[^*]|\*[^/])*\*/")]
// This syntax should also be a comment: ^----- or ^-- or ^------ text until newline
#[logos(skip r"\^-[-]+.*")]
#[logos(skip r"<--[-]+.*")]
pub enum TokenKind {
    #[regex(r"\s+[.]")]
    BlockTerminator,

    #[token("break")]
    BreakKw,

    #[token("case")]
    CaseKw,

    #[token("while")]
    WhileKw,

    #[token("enum")]
    EnumKw,

    #[token("match")]
    MatchKw,

    // Float (f32)
    #[regex(r"[0-9]+\.[0-9]+f?", |lex| {
        lex.slice().trim_end_matches('f').parse().ok()
    })]
    #[logos(priority = 2)]
    F32(f32),

    #[regex(r"[0-9]+\.[0-9]+ff", |lex| {
        lex.slice().trim_end_matches('f').parse().ok()
    })]
    #[logos(priority = 2)]
    F64(f64), // Integer (i64) with radix support
    #[regex(r"(0b[01]+|0o[0-7]+|0x[0-9a-fA-F]+|[0-9]+)", |lex| parse_integer(lex.slice()))]
    #[logos(priority = 1)]
    Integer(Integer),

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| ustr(lex.slice()))]
    Ident(Ustr),

    #[token("i64")]
    I64Kw,

    #[token("if")]
    IfKw,

    #[token("else")]
    ElseKw,

    #[token("f32")]
    F32Kw,

    #[token("f64")]
    F64Kw,

    #[token("let")]
    LetKw,

    #[token("mut")]
    MutKw,

    #[token("fn")]
    FnKw,

    #[token("->")]
    Arrow,

    #[token("=>")]
    FatArrow,

    #[token("return")]
    Return,

    #[token(":")]
    Colon,

    #[token("::")]
    DoubleColon,

    #[token("=")]
    Equal,

    #[token("==")]
    EqEq,

    #[token("<")]
    Lt,

    #[token(">")]
    Gt,

    #[token("<=")]
    LtEq,

    #[token(">=")]
    GtEq,

    #[token("!=")]
    NotEq,

    #[token(",")]
    Comma,

    #[token(";")]
    Semicolon,

    #[token("true")]
    TrueKw,

    #[token("false")]
    FalseKw,

    #[token("nil")]
    NilKw,

    #[token("struct")]
    StructKw,

    // Basic operators
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Asterix,
    #[token("/")]
    Slash,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(".")]
    Dot,

    #[token("@")]
    At,

    #[token("#")]
    Hash,

    EOF,
}

impl TokenKind {
    pub fn from_keyword(keyword: &str) -> Option<TokenKind> {
        match keyword {
            "return" => Some(TokenKind::Return),
            "break" => Some(TokenKind::BreakKw),
            _ => None,
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::BreakKw => "'break'".fmt(f),
            TokenKind::F32(x) => write!(f, "'{}f' (f32)", x),
            TokenKind::F64(x) => write!(f, "'{}ff' (f64)", x),
            TokenKind::Integer(Integer::I64(i)) => write!(f, "'{}' (i64)", i),
            TokenKind::Ident(x) => write!(f, "'{}' (identifier)", x),
            TokenKind::I64Kw => "'i64'".fmt(f),
            TokenKind::IfKw => "'if'".fmt(f),
            TokenKind::ElseKw => "'else'".fmt(f),
            TokenKind::F32Kw => "'f32'".fmt(f),
            TokenKind::F64Kw => "'f64'".fmt(f),
            TokenKind::LetKw => "'let'".fmt(f),
            TokenKind::MutKw => "'mut'".fmt(f),
            TokenKind::FnKw => "'fn'".fmt(f),
            TokenKind::Arrow => "'->'".fmt(f),
            TokenKind::FatArrow => "'=>'".fmt(f),
            TokenKind::Return => "'return'".fmt(f),
            TokenKind::Colon => "':'".fmt(f),
            TokenKind::DoubleColon => "'::'".fmt(f),
            TokenKind::Equal => "'='".fmt(f),
            TokenKind::EqEq => "'=='".fmt(f),
            TokenKind::Lt => "'<'".fmt(f),
            TokenKind::Gt => "'>'".fmt(f),
            TokenKind::LtEq => "'<='".fmt(f),
            TokenKind::GtEq => "'>='".fmt(f),
            TokenKind::Comma => "','".fmt(f),
            TokenKind::Semicolon => "';'".fmt(f),
            TokenKind::TrueKw => "'true'".fmt(f),
            TokenKind::FalseKw => "'false'".fmt(f),
            TokenKind::NilKw => "'nil'".fmt(f),
            TokenKind::Plus => "'+'".fmt(f),
            TokenKind::Minus => "'-'".fmt(f),
            TokenKind::Asterix => "'*'".fmt(f),
            TokenKind::Slash => "'/'".fmt(f),
            TokenKind::LParen => "'('".fmt(f),
            TokenKind::RParen => "')'".fmt(f),
            TokenKind::LBrace => "'{'".fmt(f),
            TokenKind::RBrace => "'}'".fmt(f),
            TokenKind::EOF => "'EOF'".fmt(f),
            TokenKind::StructKw => "'struct'".fmt(f),
            TokenKind::Hash => "'#'".fmt(f),
            TokenKind::NotEq => "'!='".fmt(f),
            TokenKind::WhileKw => "'while'".fmt(f),
            TokenKind::EnumKw => "'enum'".fmt(f),
            TokenKind::MatchKw => "'match'".fmt(f),
            TokenKind::Dot => "'.'".fmt(f),
            TokenKind::At => "'@'".fmt(f),
            TokenKind::BlockTerminator => ".".fmt(f),
            TokenKind::CaseKw => "'case'".fmt(f),
        }
    }
}

/// Token with kind and span
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: logos::Span,
}
