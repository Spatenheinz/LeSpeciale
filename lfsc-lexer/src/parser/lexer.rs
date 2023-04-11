// use crate::parser::token::*;
// use crate::parser::location::*;

// type MResult<T, E> = core::result::Result<T, E>;

// type LexerResult<'a> = MResult<Token<'a>, LexerError>;


// pub fn tokenize<'a>(src: &'a str) -> Vec<Token> {
//     let lex = Lexer::new(src, None).into_iter().peekable();
//     let lexer_result: Result<Vec<Token>, LexerError> = lex
//         .collect::<Vec<Result<Token, LexerError>>>()
//         .into_iter()
//         .collect();
//     lexer_result.unwrap()
// }

// #[derive(Debug)]
// pub struct Lexer<'a> {
//     chars: core::iter::Peekable<core::str::Chars<'a>>,
//     path: Option<String>,
//     line: usize,
//     col: usize,
// }

// impl<'a> Lexer<'a> {
//     pub fn new(chars: &'a str, path: Option<String>) -> Self {
//         Self {
//             chars: chars.chars().peekable(),
//             path,
//             line: 0,
//             col: 0,
//         }
//     }

//     fn loc(&self) -> Location {
//         Location {
//             row: self.line + 1,
//             col: self.col,
//         }
//     }

//     fn skip_whitespace(&mut self) {
//         while let Some(x) = self.chars.peek() {
//             if !x.is_whitespace() {
//                 break;
//             }
//             self.consume_char();
//         }
//     }
//     fn consume_char(&mut self) -> Option<char> {
//         match self.chars.next() {
//             Some(c) => {
//                 self.col += 1;
//                 if c == '\n' {
//                     self.col = 0;
//                     self.line += 1;
//                 }
//                 Some(c)
//             }
//             None => None,
//         }
//     }

//     pub fn next_token(&mut self) -> LexerResult {
//         self.skip_whitespace();
//         if let Some(c) = self.consume_char() {
//             if c == ';' {
//                 self.drop_line();
//                 return self.next_token();
//             } else {
//                 return self.to_token(c);
//             }
//         }
//         Ok(Token {
//             kind: TokenKind::EOF,
//             raw: "",
//             location: self.loc(),
//         })
//     }

//     fn parse_string(&mut self, initial: char) -> Result<Token, LexerError> {
//         let mut ident = initial.to_string();
//         let location = self.loc();
//         while let Some(c) = self.chars.peek() {
//             if c.is_whitespace() || *c == ')' || *c == '(' {
//                 break
//             };
//             ident.push(*c);
//             self.consume_char();
//         }
//         Ok(Token::new(keywords(&ident), &ident[..], location))
//     }

//     fn parse_number(&mut self, initial: char) -> Result<Token, LexerError> {
//         let radix = 10;
//         /*
//          * We can parse either naturals or rationals
//          */
//         let mut numerator = initial.to_string();
//         let mut denominator = "".to_string();
//         let mut rational = false;
//         let loc = self.loc();

//         loop {
//             match self.chars.peek() {
//                 Some(c) if c.is_digit(radix) => {
//                     if rational {
//                         denominator.push(*c);
//                     } else {
//                         numerator.push(*c);
//                     }
//                 }
//                 Some(c) if *c == '/' && !rational => {
//                     rational = true;
//                 }
//                 Some(c) if !c.is_whitespace() && *c != ')' && *c != '(' => {
//                     return Err(LexerError::BadNum { num: numerator, broken: c.to_string() })
//                 }
//                 _ => break,
//             };
//             // we only just peeked but we need to consume
//             self.consume_char();
//         }
//         // we can unwrap since we check that all values are digits
//         // can be an error if too big
//         let p = match numerator.parse::<u32>() {
//             Ok(p) => p,
//             Err(_) => return Err(LexerError::NumParse(numerator)),
//         };
//         let (kind, raw) = if rational {
//             let q = match denominator.parse::<u32>() {
//                 Ok(q) => q,
//                 Err(_) => return Err(LexerError::NumParse(denominator)),
//             };
//             (
//                 Ok(TokenKind::Rational{p, q}),
//                 [numerator, denominator].join("/"),
//             )
//         } else {
//             (Ok(TokenKind::Natural(p)), numerator)
//         };
//         Ok(Token::new(kind?, &raw[..], loc))
//     }

//     fn to_token(&mut self, c: char) -> LexerResult {
//         match c {
//             '(' | ')' | '%' | '!' | '#' | '@' | ':' | '\'' | '^' | '~' => Ok(Token {
//                 kind: c.into(),
//                 raw: c.to_string().as_str(),
//                 location: self.loc(),
//             }),
//             '0'..='9' => self.parse_number(c),
//             _ => self.parse_string(c),
//         }
//     }

//     fn drop_line(&mut self) {
//         while let Some(c) = self.consume_char() {
//             if c == '\n' {
//                 return;
//             }
//         }
//     }
// }

// impl<'a> Iterator for Lexer<'a> {
//     type Item = LexerResult<'a>;
//     fn next(&mut self) -> Option<Self::Item> {
//         match self.next_token() {
//             Ok(Token{ kind: TokenKind::EOF, .. }) => None,
//             c => Some(c),
//         }
//     }
// }

// impl<'a> From<char> for TokenKind<'a> {
//     fn from(item: char) -> Self {
//         match item {
//         '(' => TokenKind::LParen,
//         ')' => TokenKind::RParen,
//         '%' => TokenKind::Percent,
//         '!' => TokenKind::Bang,
//         '#' => TokenKind::Pound,
//         '@' => TokenKind::At,
//         ':' => TokenKind::Colon,
//         '\'' => TokenKind::BackSlash,
//         '^' => TokenKind::Caret,
//         '~' => TokenKind::Tilde,
//         _ => panic!("tried to convert symbol which is not implemented"),
//         }
//     }
// }


// #[derive(Debug, PartialEq, Clone)]
// pub enum LexerError {
//     TmpErr, // MissingSymbol { expected: TokenType, found: Token },

//     NumParse(String),
//     BadNum{ num: String, broken: String},
// }



// #[cfg(test)]
// mod tests {
//     use crate::parser::token::{Token, TokenKind};
//     use crate::{Lexer};
//     use crate::parser::location::Location;

//     #[test]
//     fn simple_number() {
//         let str = "42";
//         assert_eq!(
//             Lexer::new(str, None).next_token(),
//             Ok(Token {
//                 kind: TokenKind::Natural("42".to_owned()),
//                 raw: "42".to_string(),
//                 location: Location { row: 1, col: 1 }
//             })
//         );
//     }
// }
