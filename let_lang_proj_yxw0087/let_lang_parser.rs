use let_lang_scanner::*;
use let_lang_exp::*;

use std::fmt;
use std::slice;

pub fn parse(tokens: &Vec<Token>) -> Result<LetExp, ParseErr> {
    Parser::parse(tokens)
}

pub struct ParseErr {
    message: String,
}

impl fmt::Display for ParseErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ParseError: {}", self.message)
    }}
impl fmt::Debug for ParseErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ParseError: {}", self.message)
    }}

macro_rules! parse_err {
    ($($arg:tt)*) => (
        return Err(ParseErr { message: format!($($arg)*)})
    )
}

// Parser datatype is struct with one field.
#[derive(Clone)]
struct Parser<'a> {
    tokens: slice::Iter<'a, Token>,
}

impl<'a> Parser<'a> {
    // This is a constructor for a Parser object.
    fn parse(tokens: &Vec<Token>) -> Result<LetExp, ParseErr> {
        let mut parser = Parser { tokens: tokens.iter() };
        parser.parse_letexp()
    }
    
    fn parse_letexp(&mut self) -> Result<LetExp, ParseErr> {
        let option_peek: Option<&Token> = self.tokens.clone().next();
        match option_peek {
            Some(peek_token) => self.parse_letexp_work(peek_token.clone()),
            None             => parse_err!("Unexpected end of input")
        }
    }
    
    fn parse_letexp_work(&mut self, peek_tok: Token) -> Result<LetExp, ParseErr> {
        match peek_tok {
        
            Token::Let  => {
            let exp = try!(self.parse_let());
            Ok(exp)
            },
            
            Token::Minus  => {
            let exp = try!(self.parse_minus());
            Ok(exp)
            },
            
            Token::IsZero  => {
            let exp = try!(self.parse_iszero());
            Ok(exp)
            },
            
            Token::If  => {
            let exp = try!(self.parse_if());
            Ok(exp)
            },
            
            Token::Integer(_i)  => {
            let exp = try!(self.parse_num());
            Ok(exp)
            },
            
            Token::Boolean(_b)  => {
            let exp = try!(self.parse_bool());
            Ok(exp)
            },
            
            Token::Identifier(_s) => {
            let exp = try!(self.parse_var());
            Ok(exp)
            },
            
            _ => parse_err!("Unexpected token type"),
            }
    }
    
    // advances input stream by one token
    fn parse_openp(&mut self) -> Result<Option<LetExp>, ParseErr> {
        let option_tok: Option<&Token> = self.tokens.next();
        match option_tok {
            Some(&Token::Lparen) => Ok(None),
            _                    => parse_err!("Left paren expected")
        }}
    
    // advances input stream by one token
    fn parse_closep(&mut self) -> Result<Option<LetExp>, ParseErr> {
        let option_tok: Option<&Token> = self.tokens.next();
        match option_tok {
            Some(&Token::Rparen) => Ok(None),
            _                    => parse_err!("Right paren expected")
        }}
        
    fn parse_assign(&mut self) -> Result<Option<LetExp>, ParseErr> {
        let option_tok: Option<&Token> = self.tokens.next();
        match option_tok {
            Some(&Token::Assign) => Ok(None),
            _                    => parse_err!("Assignment of variable expected")
        }}
        
    fn parse_in(&mut self) -> Result<Option<LetExp>, ParseErr> {
        let option_tok: Option<&Token> = self.tokens.next();
        match option_tok {
            Some(&Token::In) => Ok(None),
            _                => parse_err!("Keyword 'in' expected")
        }}
    
    fn parse_then(&mut self) -> Result<Option<LetExp>, ParseErr> {
        let option_tok: Option<&Token> = self.tokens.next();
        match option_tok {
            Some(&Token::Then) => Ok(None),
            _                  => parse_err!("Keyword 'then' expected")
        }}
        
    fn parse_else(&mut self) -> Result<Option<LetExp>, ParseErr> {
        let option_tok: Option<&Token> = self.tokens.next();
        match option_tok {
            Some(&Token::Else) => Ok(None),
            _                  => parse_err!("Keyword 'else' expected")
        }}
        
    fn parse_comma(&mut self) -> Result<Option<LetExp>, ParseErr> {
        let option_tok: Option<&Token> = self.tokens.next();
        match option_tok {
            Some(&Token::Comma) => Ok(None),
            _                   => parse_err!("Comma ',' expected")
        }}

    // does not advance input stream
    fn parse_var(&mut self) -> Result<LetExp, ParseErr> {
        let var: Identifier;
        let option_tok: Option<&Token> = self.tokens.next();
        let tok = match option_tok {
                    Some(t) => t,
                    _       => parse_err!("parse_var: Ident expected but EOI found."),
                    };
        match tok.clone() {
            Token::Identifier(s) => var = s,
            _                    => parse_err!("parse_var: Identifier token expected."),
        };
        Ok(LetExp::new_var(&var))
    }
    
    // Gets the variable name.
    // Advances input stream by one token.
    fn get_identifier(&mut self) -> Result<Identifier, ParseErr> {
        let option_tok: Option<&Token> = self.tokens.next();
        
        // Extracts the token from the Option
        let tok: Token = match option_tok {
                            Some(token) => token.clone(),
                            _ => parse_err!("get_identifier: Unexpected token type")
                            };
        // Returns the Identifier in result form.
        let var: String =
            match tok {
                Token::Identifier(s) => s,
                _                    => parse_err!("get_identifier: Unexpected token type"),
                };
        Ok(var)
    }
        
    fn parse_num(&mut self) -> Result<LetExp, ParseErr> {
        let num: i32;
        let option_tok: Option<&Token> = self.tokens.next();
        let tok = match option_tok {
                    Some(t) => t,
                    _       => parse_err!("parse_num: Ident expected but EOI found."),
                    };
        match tok.clone() {
            Token::Integer(i) => num = i,
            _                 => parse_err!("parse_num: Integer token expected."),
        };
        Ok(LetExp::new_const(&num))
    }
    
    /*// Gets the constant.
    // Advances input stream by one token.
    fn get_const(&mut self) -> Result<i32, ParseErr> {
        let option_tok: Option<&Token> = self.tokens.next();
        
        // Extracts the token from the Option
        let tok: Token = match option_tok {
                            Some(token) => token.clone(),
                            _ => parse_err!("get_const: Unexpected token type")
                            };

        let num: i32 =
            match tok {
                Token::Integer(i) => i,
                _                    => parse_err!("get_const: Unexpected token type"),
                };
        Ok(num)
    }*/
        
    fn parse_bool(&mut self) -> Result<LetExp, ParseErr> {
        let boolean: bool;
        let option_tok: Option<&Token> = self.tokens.next();
        let tok = match option_tok {
                    Some(t) => t,
                    _       => parse_err!("parse_num: Ident expected but EOI found."),
                    };
        match tok.clone() {
            Token::Boolean(b) => boolean = b,
            _                 => parse_err!("parse_num: Integer token expected."),
        };
        Ok(LetExp::new_boolean(&boolean))
    }
    
    fn parse_let(&mut self) -> Result<LetExp, ParseErr> {
        let option_tok: Option<&Token> = self.tokens.next();
        let tok: Token = match option_tok {
                    Some(token) => token.clone(), // token is of type &Token
                    _           => parse_err!("parse_let error")
                    };
        match tok {
            Token::Let => (),
            _          => parse_err!("parse_let: Unexpected token type"),
        };
        let v = try!(self.get_identifier());
        try!(self.parse_assign());
        let exp1 = try!(self.parse_letexp());
        try!(self.parse_in());
        let exp2 = try!(self.parse_letexp());

        Ok(LetExp::new_let(&v, &exp1, &exp2))
    }
    
    fn parse_if(&mut self) -> Result<LetExp, ParseErr> {
        let option_tok: Option<&Token> = self.tokens.next();
        let tok: Token = match option_tok {
                    Some(token) => token.clone(), // token is of type &Token
                    _           => parse_err!("parse_if error")
                    };
        match tok {
            Token::If => (),
            _         => parse_err!("parse_if: Unexpected token type"),
        };
        let exp1 = try!(self.parse_letexp());
        try!(self.parse_then());
        let exp2 = try!(self.parse_letexp());
        try!(self.parse_else());
        let exp3 = try!(self.parse_letexp());

        Ok(LetExp::new_if(&exp1, &exp2, &exp3))
    }
    
    fn parse_iszero(&mut self) -> Result<LetExp, ParseErr> {
        let option_tok: Option<&Token> = self.tokens.next();
        let tok: Token = match option_tok {
                    Some(token) => token.clone(), // token is of type &Token
                    _           => parse_err!("parse_iszero error")
                    };
        match tok {
            Token::IsZero => (),
            _         => parse_err!("parse_iszero: Unexpected token type"),
        };

        try!(self.parse_openp());
        let exp = try!(self.parse_letexp());
        try!(self.parse_closep());

        Ok(LetExp::new_iszero(&exp))
    }
    
    fn parse_minus(&mut self) -> Result<LetExp, ParseErr> {
        let option_tok: Option<&Token> = self.tokens.next();
        let tok: Token = match option_tok {
                    Some(token) => token.clone(), // token is of type &Token
                    _           => parse_err!("parse_minus error")
                    };
        match tok {
            Token::Minus => (),
            _            => parse_err!("parse_minus: Unexpected token type"),
        };

        try!(self.parse_openp());
        let exp1 = try!(self.parse_letexp());
        try!(self.parse_comma());
        let exp2 = try!(self.parse_letexp());
        try!(self.parse_closep());

        Ok(LetExp::new_diff(&exp1, &exp2))
    }
}