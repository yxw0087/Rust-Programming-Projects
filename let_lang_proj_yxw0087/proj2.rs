// START OF LEXER //

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
    Minus,
    Assign,
    IsZero,
    If,
    Then,
    Else,
    Let,
    In,
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
                            self.advance();                 // and advance
                        },
                        ')' => {
                            self.tokens.push(Token::Rparen); // add to token vec
                            self.advance();                  // and advance
                        },
                        ',' => {
                            self.tokens.push(Token::Comma); // add to token vec
                            self.advance();                  // and advance
                        },
                        '-' => {
                            self.tokens.push(Token::Minus); // add to token vec
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

// END OF LEXER //

// START OF EXP //

use std::rc::Rc;
//use std::fmt;

pub type Identifier = String;

#[derive(Debug,Clone)]
pub enum LetExp {
    ConstExp(i32),
    Boolean(bool),
    DiffExp(Rc<LetExp>, Rc<LetExp>),
    IsZeroExp(Rc<LetExp>),
    IfExp(Rc<LetExp>, Rc<LetExp>, Rc<LetExp>),
    VarExp(Identifier),
    LetExp(Identifier, Rc<LetExp>, Rc<LetExp>),
}

impl LetExp {
    pub fn new_const(num: &i32) -> Self {
        LetExp::ConstExp(num.clone())
    }
    pub fn new_boolean(boolean: &bool) -> Self {
        LetExp::Boolean(boolean.clone())
    }
    pub fn new_diff(Exp1: &LetExp, Exp2: &LetExp) -> Self {
        LetExp::DiffExp(Rc::new(Exp1.clone()), Rc::new(Exp2.clone()))
    }
    pub fn new_iszero(Exp: &LetExp) -> Self {
        LetExp::IsZeroExp(Rc::new(Exp.clone()))
    }
    pub fn new_if(ifexp: &LetExp, thenexp: &LetExp, elseexp: &LetExp) -> Self {
        LetExp::IfExp(Rc::new(ifexp.clone()), Rc::new(thenexp.clone()), Rc::new(elseexp.clone()))
    }
    pub fn new_var(s: &Identifier) -> Self {
        LetExp::VarExp(s.clone())
    }
    pub fn new_let(s: &Identifier, assignExp: &LetExp, inExp: &LetExp) -> Self {
        LetExp::LetExp(s.clone(), Rc::new(assignExp.clone()), Rc::new(inExp.clone()))
    }
}

// END OF EXP //

// START OF PARSER //

//use let_lang_scanner::*;
//use let_lang_exp::*;

//use std::fmt;
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

// END OF PARSER //

// START OF INTBOOL //

#[derive(Debug,Clone)]
pub enum IntBool {
    Integer(i32),
    Boolean(bool),
}

impl IntBool {
    pub fn new_int(i: i32) -> Self {
        IntBool::Integer(i.clone())
    }
    pub fn new_bool(b: bool) -> Self {
        IntBool::Boolean(b.clone())
    }
}

// END OF INTBOOL //

// START OF ENV //

//use let_lang_exp::*;
//use int_bool::*;
//use std::rc::Rc;
//use std::fmt;

#[derive(Debug,Clone)]
pub enum EnvExp {
    EmptyEnv,
    ExtendEnv(Identifier, IntBool, Rc<EnvExp>),
}

impl EnvExp {
    pub fn new_env() -> Self {
        EnvExp::EmptyEnv
    }
    pub fn extend_env(&self, s:&Identifier, val:&IntBool) -> Self {
        EnvExp::ExtendEnv(s.clone(), val.clone(), Rc::new(self.clone()))
    }
    pub fn apply_env(&self, s:&Identifier) -> Option<IntBool> {
        match self.clone() {
            EnvExp::ExtendEnv(var, val, env) => 
                                       if s[..] == var[..] {
                                        Some(val.clone())
                                       } else {
                                        env.apply_env(s)
                                        },
            EnvExp::EmptyEnv => None,
        }
    }
    pub fn is_null_env(&self) -> bool {
        match self.clone() {
            EnvExp::EmptyEnv  => true,
            _                 => false,
        }
    }
    /*pub fn to_string(&self) -> String {
        match self.clone() {
            EnvExp::EmptyEnv => "[]".to_string(),
            EnvExp::ExtendEnv(var,val,env) => {let mut temp = "[".to_string();
                                                temp.push_str(&(var.to_string()));
                                                temp.push_str(&(", ".to_string()));
                                                temp.push_str(&(val.to_string()));
                                                temp.push_str(&(" ".to_string()));
                                                temp.push_str(&(env.to_string()));
                                                temp.push_str(&("]".to_string()));
                                                temp},
        }
    }*/
}

impl fmt::Display for EnvExp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = format!("");
        let s1 = self.to_string();
        s.push_str(&s1);
        write!(f, "{}", s)
    }
}

// END OF ENV //

// START OF MAIN //

//extern crate lcexp_parser; //folder name

//use root::let_lang_scanner::*;
//use root::let_lang_parser::*;
//use root::let_lang_env::*;
//use root::let_lang_exp::*;
//use root::int_bool::*;

fn expval_num(num:&IntBool) -> Option<i32> {
    match num.clone() {
        IntBool::Integer(i) => Some(i),
        _                   => {println!("Error: expecting integer."); std::process::exit(1)},
    }
}

fn expval_bool(boolean:&IntBool) -> Option<bool> {
    match boolean.clone() {
        IntBool::Boolean(b) => Some(b),
        _                   => {println!("Error: expecting boolean."); std::process::exit(1)},
    }
}

fn value_of(exp:&LetExp, env:&EnvExp) -> Option<IntBool> {

    match exp.clone() {
    
        LetExp::ConstExp(i) => Some(IntBool::Integer(i)),
        
        LetExp::Boolean(b) => Some(IntBool::Boolean(b)),
        
        LetExp::VarExp(s) => Some(env.apply_env(&s).unwrap()), 
        //Will cause compiler to panic if s is not found in the env
        
        LetExp::DiffExp(exp1, exp2) => 
        {
            let val1 = value_of(&exp1, &env);
            let val2 = value_of(&exp2, &env);
            let num1 = expval_num(&val1.unwrap());
            let num2 = expval_num(&val2.unwrap());
            let result = num1.unwrap() - num2.unwrap();
            Some(IntBool::Integer(result))
        },
        
        LetExp::IsZeroExp(exp) =>
        {
            let val = value_of(&exp, &env);
            let num = expval_num(&val.unwrap());
            //Will cause compiler to panic if val is not constant
            if num.unwrap() == 0 {Some(IntBool::Boolean(true))}
            else {Some(IntBool::Boolean(false))}
        },
        
        LetExp::IfExp(exp1, exp2, exp3) =>
        {
            let val = value_of(&exp1, &env);
            if expval_bool(&val.unwrap()).unwrap() {value_of(&exp2, &env)}
            else {value_of(&exp3, &env)}
        },
        
        LetExp::LetExp(s, exp1, exp2) =>
        {
            let val = value_of(&exp1, &env);
            value_of(&exp2, &env.extend_env(&s, &val.unwrap()))
        },
    }
}

#[allow(dead_code)]
fn main() {

    println!("##### Start testing the evaluation of let statement, expected output: Some(Integer(-5)) #####");
    let env = EnvExp::new_env();
    let str = "let x = 7 in let y = 2 in let y = let x = -(x, 1) in -(x, y) in -(-(x, 8), y)";
    let result: Result<Vec<Token>, LexErr> = tokenize(str);
    let tokens = match result {
                    Ok(v)  => v,
                    Err(_e) => return,
                    };
    let parse_tree = parse(&tokens);
    println!("AST of the let statement: {:#?}", &parse_tree);
    let value = value_of(&parse_tree.unwrap(), &env);
    println!("Value: {:?}", value);
    
    println!("\n##### Start testing the evaluation of if statement, expected output: Some(Integer(18)) #####");
    let env = EnvExp::new_env();
    let str = "if iszero(-(x, 11)) then -(y, 2) else -(y, 4)";
    let result: Result<Vec<Token>, LexErr> = tokenize(str);
    let tokens = match result {
                    Ok(v)  => v,
                    Err(_e) => return,
                    };
    let parse_tree = parse(&tokens);
    println!("AST of the if statement: {:#?}", &parse_tree);
    let env1 = env.extend_env(&("y".to_string()), &IntBool::Integer(22));
    let env2 = env1.extend_env(&("x".to_string()), &IntBool::Integer(33));
    let value = value_of(&parse_tree.unwrap(), &env2);
    println!("Value: {:?}", value);
    
    println!("\n##### Extra testing to handle boolean input, expected output: Error #####");
    let env = EnvExp::new_env();
    let str = "-(1, false)";
    let result: Result<Vec<Token>, LexErr> = tokenize(str);
    let tokens = match result {
                    Ok(v)  => v,
                    Err(_e) => return,
                    };
    let parse_tree = parse(&tokens);
    println!("AST of the statement: {:#?}", &parse_tree);
    let value = value_of(&parse_tree.unwrap(), &env);
    println!("Value: {:?}", value);
}