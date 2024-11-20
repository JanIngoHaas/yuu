use logos::Logos;

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

#[derive(Logos, Debug, PartialEq, Clone)]
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

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"\s+")] // Skip whitespace
#[logos(skip r"//.*")]
#[logos(skip r"/\*(?:[^*]|\*[^/])*\*/")]
pub enum TokenVariants {
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
    F64(f64),

    // Integer (i64) with radix support
    #[regex(r"(0b[01]+|0o[0-7]+|0x[0-9a-fA-F]+|[0-9]+)?", |lex| parse_integer(lex.slice()))]
    #[logos(priority = 1)]
    Integer(Integer),

    // #[regex(r#""([^"\\]|\\[\\nrt"])*""#, |lex| lex.slice().to_string())]
    // String(String),
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Ident(String),

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

    EOF,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenVariants,
    pub span: logos::Span,
}
