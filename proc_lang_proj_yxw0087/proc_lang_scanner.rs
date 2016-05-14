use std::str;
use std::fmt;
use std::iter;

pub fn tokenize(s: &str) -> Result<Vec<Token>, LexErr> {
    Lexer::tokenize(s)
}

#[derive(Clone,PartialEq, Debug)]
pub enum Token {
    Lparen,
    Rparen,
    Comma,
    Plus,
    Minus,
    Assign,
    IsZero,
    If,
    Then,
    Else,
    Let,
    LetRec,
    In,
    Proc,
    Identifier(String),
    Integer(i32),
    Boolean(bool),
}

pub struct LexErr {
    message: String,
    line: u32,
    column: u32,
}

impl fmt::Display for LexErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "LexError: {} (line: {}, column: {})", self.message, self.line, self.column)
    }
}
impl fmt::Debug for LexErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "LexError: {} (line: {}, column: {})", self.message, self.line, self.column)
    }
}

macro_rules! lex_error {
    ($lexer:ident, $($arg:tt)*) => (
        return Err(LexErr { message: format!($($arg)*), line: $lexer.line, column: $lexer.column })
    )
}

struct Lexer<'a> {
    chars: iter::Peekable<str::Chars<'a>>,
    current: Option<char>,
    tokens: Vec<Token>,
    line: u32,
    column: u32,
}

impl<'a> Lexer<'a> { 
    fn tokenize(s: &str) -> Result<Vec<Token>, LexErr> {
        let mut lexer = Lexer { chars: s.chars().peekable(),   // creates lexer object
                                current: None, 
                                tokens: Vec::new(), 
                                line: 1, 
                                column: 0 };
        try!(lexer.scan()); // May return from tokenize() w/ Err. If no error, then
                           // scan characters and update lexer tokens.
        Ok(lexer.tokens)
    }

    fn current(&self) -> Option<char> {  // pure selector
        self.current}

    fn advance(&mut self) {  // invokes next(), keeps track of line, col.
        if self.current() == Some('\x0a') {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        self.current = self.chars.next();
    }

    fn scan(&mut self) -> Result<(), LexErr> {
        self.advance(); // set current char to first in char stream
        loop {
            match self.current() { // if eof char stream, break; else process char
                Some(c) => {
                    match c {
                        _ if c.is_whitespace() => { // interesting construct
                            self.advance();         // skip over whitespace
                        },
                        'a' ... 'z' => {
                            let tok = try!(self.scan_id_or_reserved());
                            self.tokens.push(tok);
                        }
                        '(' => {
                            self.tokens.push(Token::Lparen); // add to token vec
                            self.advance();                  // and advance
                        },
                        ')' => {
                            self.tokens.push(Token::Rparen); // add to token vec
                            self.advance();                  // and advance
                        },
                        ',' => {
                            self.tokens.push(Token::Comma);  // add to token vec
                            self.advance();                  // and advance
                        },
                        '-' => {
                            self.tokens.push(Token::Minus);  // add to token vec
                            self.advance();                  // and advance
                        },
                        '+' => {
                            self.tokens.push(Token::Plus);   // add to token vec
                            self.advance();                  // and advance
                        },
                        '=' => {
                            self.tokens.push(Token::Assign); // add to token vec
                            self.advance();                  // and advance
                        },
                        '0' ... '9' => {
                            let tok = try!(self.scan_number());
                            self.tokens.push(tok);
                            //self.tokens.push(Token::Integer(c as i32)); // add to token vec
                            //self.advance();                  // and advance
                        },
                        _ => {
                            lex_error!(self, "Unexpected character: {}", c);
                        },
                    }
                },
                None => break
            }
        };
        Ok(())
    }

    fn scan_id_or_reserved(&mut self) -> Result<Token, LexErr> {
        let mut s = String::new();
        loop {
            match self.current() {
                Some(c) => {
                    match c {
                        'a' ... 'z' => { s.push(c);
                                         self.advance();}
                        _ => {
                            break;
                        },
                    }
                },
                None => break
            }
        }
        if &s[..] == "let" {
            Ok(Token::Let)
        } else if &s[..] == "in" {
            Ok(Token::In)
        } else if &s[..] == "letrec" {
            Ok(Token::LetRec)
        } else if &s[..] == "proc" {
            Ok(Token::Proc)
        } else if &s[..] == "iszero" {
            Ok(Token::IsZero)
        } else if &s[..] == "if" {
            Ok(Token::If)
        } else if &s[..] == "then" {
            Ok(Token::Then)
        } else if &s[..] == "else" {
            Ok(Token::Else)
        } else if &s[..] == "true" {
            Ok(Token::Boolean(true))
        } else if &s[..] == "false" {
            Ok(Token::Boolean(false))
        } else {
            Ok(Token::Identifier(s))
        }
    }
    
    fn scan_number(&mut self) -> Result<Token, LexErr> {
        let mut s = String::new();
        loop {
            match self.current() {
                Some(c) => {
                    match c {
                        '0' ... '9' => { s.push(c);
                                         self.advance();}
                        _ => {
                            break;
                        },
                    }
                },
                None => break
            }
        }
        
        let int = s.parse::<i32>().unwrap(); // convert string to i32
        Ok(Token::Integer(int))
    }
}