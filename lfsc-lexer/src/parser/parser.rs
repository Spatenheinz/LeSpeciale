use crate::parser::token::*;

pub type PResult<'a, R> = Result<R, ParseError<'a>>;

static EOF: Token = Token::new_eof();

#[derive(Debug)]
pub struct Parser<'a> {
    pub tokens: &'a [Token<'a>],
    it: usize,
    // errors: Option<ParseError>
    // tokens: core::iter::Peekable<Iter<'a, Token>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        // Self { tokens , it: 0, errors: None }
        Self { tokens , it: 0 }
    }

    // pub fn error(&mut self, error: ParseError) {
    //     if self.errors.is_none() {
    //         self.errors = Some(error);
    //     }
    // }
    fn current_token(&self) -> Option<&Token> {
        self.tokens.get(self.it)
    }
    pub fn is_eof(&self) -> bool {
        self.check(TokenKind::EOF)
    }
    pub fn check(&self, expected: TokenKind) -> bool {
        self.peek().kind == expected
    }
    pub fn peek(&self) -> &Token {
        self.current_token().unwrap_or(&EOF)
    }

    pub fn next_kind(&mut self) -> TokenKind {
        self.next().into()
    }

    pub fn peek_kind(&self) -> TokenKind {
        self.peek().into()
    }

    pub fn next(&mut self) -> &Token {
        let token = self.tokens.get(self.it);
        match token {
            Some(tok) => { self.it += 1; tok },
            None => &EOF
        }
    }

    pub fn expect(&mut self, expected: TokenKind) -> PResult<&Token> {
        match self.next() {
            token if token.kind == expected => Ok(token),
            token => Err(ParseError::Unexpected { tok: token.clone() }),
        }
    }

    pub fn optionally(&mut self, expected: TokenKind) -> Option<&Token> {
        if self.check(expected) {
            return Some(self.next())
        }
        None
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ParseError<'a> {
    Unexpected { tok: Token<'a> },
    NCommandTop,
    NotIdent,
    Hack,
    EmptyTerm,
}
