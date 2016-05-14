// START OF SCANNER

use std::str;
use std::fmt;
use std::iter;

pub fn tokenize(s: &str) -> Result<Vec<Token>, LexErr> {
    Lexer::tokenize(s)
}

#[derive(Clone, PartialEq, Debug)]
pub enum Token {
    Lparen,  // (
    Rparen,  // )
    Comma,   // ,
    Plus,    // +
    Minus,   // -
    Assign,  // =
	SemiColon,
    IsZero,
    If,
    Then,
    Else,
    Let,
    In,
    Proc,
    LetRec,
	Begin,
	End,
	Set,
    Identifier(String),
    Integer(i32),
    Boolean(bool)
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

#[allow(dead_code)]
    fn peek(&mut self) -> Option<char> { // peeks next char
        match self.chars.peek() {
            Some(c) => Some(*c),
            None => None
        }}

    fn scan(&mut self) -> Result<(), LexErr> {
        self.advance(); // set current char to first in char stream
        loop {
            match self.current() { // if eof char stream, break; else process char
                Some(c) => {
                    match c {
                        _ if c.is_whitespace() => { // interesting construct
                            self.advance();         // skip over whitespace
                        },
                        '(' => {
                            self.tokens.push(Token::Lparen); // add to token vec
                            self.advance();                  // and advance
                        },
                        ')' => {
                            self.tokens.push(Token::Rparen); // add to token vec
                            self.advance();                  // and advance
                        },
                        ',' => {
                            self.tokens.push(Token::Comma); // add to token vec
                            self.advance();                 // and advance
                        },
                        '=' => {
                            self.tokens.push(Token::Assign); // add to token vec
                            self.advance();                  // and advance
                        },
						';' => {
                            self.tokens.push(Token::SemiColon); // add to token vec
                            self.advance();                     // and advance
                        },
                        'a' ... 'z' | 'A' ... 'Z' => {
                            let tok = try!(self.scan_keywrd_ident_bool());
                            self.tokens.push(tok);
                            try!(self.parse_whitespace_paren_or_eoi());
                        }
                        '-' => {
                            match self.peek() {
                                Some('(') => {
                                    self.tokens.push(Token::Minus); // add to token vec
                                    self.advance();
                                },
                                Some('0'...'9') => {
                                    // skip past the +/- symbol and parse the number
                                    self.advance();
                                    let val = try!(self.parse_number());
                                    self.tokens.push(Token::Integer(if c == '-' { -1 * val } else { val }));
                                    try!(self.parse_whitespace_paren_or_eoi());
                                },
                                _ => {
                                    // not followed by a digit
                                    lex_error!(self, "Isolated minus: {}", c);
                                }
                            }
                        },
                        '+' => {
                         match self.peek() {
                                Some('(') => {
                                    self.tokens.push(Token::Plus); // add to token vec
                                    self.advance();
                                },
                                Some('0'...'9') => {
                                    // skip past the +/- symbol and parse the number
                                    self.advance();
                                    let val = try!(self.parse_number());
                                    self.tokens.push(Token::Integer(val));
                                    try!(self.parse_whitespace_paren_or_eoi());
                                },
                                _ => {
                                    // not followed by a digit
                                    lex_error!(self, "Isolated plus: {}", c);
                                }
                            }
                        },
                        '0' ... '9' => {
                            let val = try!(self.parse_number());
                            self.tokens.push(Token::Integer(val));
                            try!(self.parse_whitespace_paren_or_eoi());
                        }
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
    // scan keyword, identifier, or boolean
    fn scan_keywrd_ident_bool(&mut self) -> Result<Token, LexErr> {
        let mut s = String::new();
        loop {
            match self.current() {
                Some(c) => {
                    match c {
                        'a' ... 'z' => { s.push(c);
                                         self.advance();}
                        'A' ... 'Z' => { s.push(c);
                                         self.advance();}
                        _ => {
                            break;
                        },
                    }
                },
                None => break
            }
        }
        if &s[..] == "iszero" {
            Ok(Token::IsZero)
        } else  
           if &s[..] == "minus" { 
            Ok(Token::Minus)
        } else  
           if &s[..] == "if" { 
            Ok(Token::If)
        } else  
           if &s[..] == "then" { 
            Ok(Token::Then)
        } else  
           if &s[..] == "else" { 
            Ok(Token::Else)
        } else  
           if &s[..] == "let" { 
            Ok(Token::Let)
        } else  
           if &s[..] == "in" { 
            Ok(Token::In)
        } else  
           if &s[..] == "proc" { 
            Ok(Token::Proc)
        } else  
           if &s[..] == "letrec" { 
            Ok(Token::LetRec)
        } else
           if &s[..] == "true" { 
            Ok(Token::Boolean(true))
        } else  
           if &s[..] == "false" { 
            Ok(Token::Boolean(false))
        } else
		   if &s[..] == "begin" { 
            Ok(Token::Begin)
        } else
		   if &s[..] == "end" { 
            Ok(Token::End)
        } else
		   if &s[..] == "set" { 
            Ok(Token::Set)
        } else {
            Ok(Token::Identifier(s))
        }
    }

    fn parse_number(&mut self) -> Result<i32, LexErr> {
        let mut s = String::new();
        loop {
            match self.current() {
                Some(c) => {
                    match c {
                        '0'...'9' => {
                            s.push(c);
                            self.advance();
                        },
                        _ => break
                    }
                },
                None => break
            }
        }
        match s.parse::<i32>() {
            Ok(value) => Ok(value),
            Err(_) => { lex_error!(self, "Not a number: {}", self.current().unwrap()); },
        }
    }

    fn parse_whitespace_paren_or_eoi(&mut self) -> Result<(), LexErr> {
        match self.current() {
            Some(c) => {
                match c {
                    _ if c.is_whitespace() => (),
                    '(' => {
                        self.tokens.push(Token::Lparen);
                        self.advance();
                    },
                    ')' => {
                        self.tokens.push(Token::Rparen);
                        self.advance();
                    },
                    ',' => {
                        self.tokens.push(Token::Comma);
                        self.advance();
                    },
                    ';' => {
                        self.tokens.push(Token::SemiColon);
                        self.advance();
                    },
                    _ => lex_error!(self, "Unexpected char, expected whitespace: {}", c),
                }
            },
            None => ()
        };
        Ok(())
    }
}

// END OF SCANNER

// START OF EXP

//use std::rc::Rc;
//use std::fmt;
//use implicit_ref_ast::implicAST::*;

#[derive(Debug,Clone)]
pub enum implicAST { // a node in an AST
    ConstExp(i32),
    Boolean(bool),
    PlusExp(Rc<implicAST>, Rc<implicAST>),
    DiffExp(Rc<implicAST>, Rc<implicAST>),
    IsZeroExp(Rc<implicAST>),
    IfExp(Rc<implicAST>, Rc<implicAST>, Rc<implicAST>),
    VarExp(String),
    LetExp(String, Rc<implicAST>, Rc<implicAST>),
    ProcExp(Vec<String>, Rc<implicAST>),
    //      bvars        body
    CallExp(Rc<implicAST>, Vec<Rc<implicAST>>),
    //      pname           args
    LetRecExp(Vec<String>, Vec<Vec<String>>, Vec<Rc<implicAST>>, Rc<implicAST>),
    //        pnames       bvars             p_bodies             in_body
    SetExp(String, Rc<implicAST>),
    //        exp1            exp2
    BeginEndExp(Vec<Rc<implicAST>>),
    //        exps
}

impl implicAST {
    pub fn new_const_exp(num: i32) -> Self {
        ConstExp(num)
    }
    pub fn new_boolean(tv: bool) -> Self {
        Boolean(tv)
    }
    pub fn new_plus_exp(arg1: &implicAST, arg2: &implicAST) -> Self {
        PlusExp(Rc::new(arg1.clone()), Rc::new(arg2.clone()))
    }
    pub fn new_diff_exp(arg1: &implicAST, arg2: &implicAST) -> Self {
        DiffExp(Rc::new(arg1.clone()), Rc::new(arg2.clone()))
    }
    pub fn new_iszero(arg: &implicAST) -> Self {
        IsZeroExp(Rc::new(arg.clone()))
    }
    pub fn new_if_exp(arg1: &implicAST, arg2: &implicAST, arg3: &implicAST) -> Self {
        IfExp(Rc::new(arg1.clone()), Rc::new(arg2.clone()), Rc::new(arg3.clone()))
    }
    pub fn new_var_exp(s: &String) -> Self {
        VarExp(s.clone())
    }
    pub fn new_let_exp(s: &String, arg1: &implicAST, arg2: &implicAST) -> Self {
        LetExp(s.clone(), Rc::new(arg1.clone()), Rc::new(arg2.clone()))
    }
    pub fn new_proc_exp(vars: &Vec<String>, 
                        body: &implicAST) -> Self {
        ProcExp(vars.clone(), Rc::new(body.clone()))
    }
    pub fn new_call_exp(rator: &implicAST, 
                        rands: &Vec<implicAST>) -> Self {
        let mut args_v: Vec<Rc<implicAST>> =  Vec::new();
        for i in 0..rands.len() {
            args_v.push(Rc::new(rands[i].clone()));
        }
        CallExp(Rc::new(rator.clone()), args_v)
    }
    pub fn new_letrec_exp(pnames  : &Vec<String>, 
                          b_vars  : &Vec<Vec<String>>, 
                          p_bodies: &Vec<implicAST>,
                          letrec_body: &implicAST) -> Self {
        // cvrt from Vec<implicAST> to Vec<Rc<implicAST>>
        let mut bodies: Vec<Rc<implicAST>> = Vec::new();
        for body in p_bodies {
            bodies.push(Rc::new(body.clone()));
        };
        LetRecExp(pnames.clone(),
                                 b_vars.clone(),
                                 bodies,
                                 Rc::new(letrec_body.clone())
                                )
    }
    pub fn new_set_exp(arg1: &String, arg2: &implicAST) -> Self {
        SetExp(arg1.clone(), Rc::new(arg2.clone()))
    }
    pub fn new_beginend_exp(exps: &Vec<implicAST>) -> Self {
        let mut args_v: Vec<Rc<implicAST>> =  Vec::new();
        for i in 0..exps.len() {
            args_v.push(Rc::new(exps[i].clone()));
        }
        BeginEndExp(args_v)
    }
    pub fn is_legal_rator(&self) -> bool {
        match self.clone() { // must be var or procedure
            VarExp(_s)         => true,
            ProcExp(_s, _e)    => true,
            CallExp(_s, _e)    => true,
            IfExp(_e1,_e2,_e3) => true,
            LetExp(_s,_e1,_e2) => true,
            LetRecExp(_names, _vars, _bodies, _inbody) => true,
          _                    => false,
            }
    }
    pub fn to_string(&self) -> String {
        match self.clone() {
            ConstExp(int)       => int.to_string(),
            Boolean(bool)       => bool.to_string(),
            PlusExp(e1, e2)     => {let mut temp = "+(".to_string();
                                    temp.push_str(&(e1.to_string()));
                                    temp.push_str(&(", ".to_string()));
                                    temp.push_str(&(e2.to_string()));
                                    temp.push_str(&(")".to_string()));
                                    temp}
            DiffExp(e1, e2)     => {let mut temp = "-(".to_string();
                                    temp.push_str(&(e1.to_string()));
                                    temp.push_str(&(", ".to_string()));
                                    temp.push_str(&(e2.to_string()));
                                    temp.push_str(&(")".to_string()));
                                    temp}
            IsZeroExp(e)        => {let mut temp = "iszero(".to_string();
                                    temp.push_str(&(e.to_string()));
                                    temp.push_str(&(")".to_string()));
                                    temp}
            IfExp(e1, e2, e3)   => {let mut temp = "if ".to_string();
                                    temp.push_str(&(e1.to_string()));
                                    temp.push_str(&(" then ".to_string()));
                                    temp.push_str(&(e2.to_string()));
                                    temp.push_str(&(" else ".to_string()));
                                    temp.push_str(&(e3.to_string()));
                                    temp}
            VarExp(var)         => var,
            LetExp(v, e1, e2)   => {let mut temp = "let ".to_string();
                                    temp.push_str(&(v.to_string()));
                                    temp.push_str(&(" = ".to_string()));
                                    temp.push_str(&(e1.to_string()));
                                    temp.push_str(&(" in ".to_string()));
                                    temp.push_str(&(e2.to_string()));
                                    temp}
            ProcExp(vars, b)    => {let mut temp = "proc (".to_string();
                                    for i in 0..vars.len() {
                                        temp.push_str(&vars[i]);
                                        if (i+1) < vars.len() {
                                            temp.push_str(&(", ".to_string()));}
                                        }
                                    temp.push_str(&(") ".to_string()));
                                    temp.push_str(&(b.to_string()));
                                    temp}
            CallExp(e1, e2)     => {let mut temp = "(".to_string();
                                    temp.push_str(&(e1.to_string()));
                                    for rator in e2 {
                                        temp.push_str(&(" ".to_string()));
                                        temp.push_str(&(rator.to_string()));
                                    }
                                    temp.push_str(&(")".to_string()));
                                    temp}
            LetRecExp(p_names, b_vars_lst, p_bodies, letrec_body)     
                                => 
                                {let mut temp = "\nletrec\n".to_string();
                                for i in 0..p_names.len() {
                                    temp.push_str(&("    ".to_string()));
                                    temp.push_str(&(p_names[i].to_string()));
                                    temp.push_str(&(" (".to_string()));
                                    let bvars = b_vars_lst[i].clone();
                                    for i in 0..bvars.len() {
                                        temp.push_str(&(bvars[i].to_string()));
                                        if (i+1) < bvars.len() {
                                            temp.push_str(&(", ".to_string()));
                                            }
                                    }
                                    temp.push_str(&(") = ".to_string()));
                                    temp.push_str(&(p_bodies[i].to_string()));
                                    temp.push_str(&("\n".to_string()));
                                }
                                temp.push_str(&(" in ".to_string()));
                                temp.push_str(&(letrec_body.to_string()));
                                temp}
            SetExp(v, e)   => {let mut temp = "set ".to_string();
                                    temp.push_str(&(v.to_string()));
                                    temp.push_str(&(" = ".to_string()));
                                    temp.push_str(&(e.to_string()));
                                    temp}
            BeginEndExp(exps)   => {let mut temp = "\nbegin\n".to_string();
                                    for i in 0..exps.len()-1 {
                                        temp.push_str(&("    ".to_string()));
                                        temp.push_str(&(exps[i].to_string()));
                                        temp.push_str(&(";".to_string()));
                                        temp.push_str(&("\n".to_string()));
                                    }
                                    temp.push_str(&("    ".to_string()));
                                    temp.push_str(&(exps[exps.len()-1].to_string()));
                                    temp.push_str(&("\nend\n".to_string()));
                                    temp}
        }}}

impl fmt::Display for implicAST {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = format!("");
        let s1 = self.to_string();
        s.push_str(&s1);
        write!(f, "{}", s)
    }
}

// END OF EXP

// START OF PARSER

//use implicit_ref_scanner::*;
//use implicit_ref_ast::*;

//use std::fmt;
use std::rc::Rc;
use std::slice;
//use std::iter;

pub fn parse(tokens: &Vec<Token>) -> Result<implicAST, ParseErr> {
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
    ($parser:ident, $($arg:tt)*) => (
        {println!($($arg)*);
         return Err(ParseErr { message: "PARSE ERR".to_string()})
        }
    )
}

// not used in this file
// Currently used in main.rs.
// pub fn gen_token_parse_err() -> Result<implicAST, ParseErr> {
//     parse_err!("Unable to tokenize.")
// }

#[derive(Debug,Clone)]
struct LetrecDecl {
    pname: String,
    bvars: Vec<String>,
    body:  implicAST,
}

// Parser datatype is struct with one field.
#[derive(Clone)]
struct Parser<'a> {
//    tokens: slice::Iter<'a, Token>, // an iterator that returns tokens
    tokens: iter::Peekable<slice::Iter<'a, Token>>, // iterator that returns tokens
}

impl<'a> Parser<'a> {
//impl Parser {
    // This is a constructor for a Parser object.
    // Builds Parser and then calls parse_exp().
    // "parser" must be mutable b/c the tokens field is updated.
    fn parse(tokens: &Vec<Token>) -> Result<implicAST, ParseErr> {
//        let mut parser = Parser { tokens: tokens.iter() };
        let mut parser = Parser { tokens: tokens.iter().peekable() };
        let ast_root = parser.parse_exp();
        let option_next_tok = parser.tokens.next();
        match option_next_tok {
            Some(tok) => parse_err!(self, "ParseErr toplevel: Extra input at end of parse: {:?}", tok),
            _          => ast_root,
        }
    }
    fn parse_exp(&mut self) -> Result<implicAST, ParseErr> {
        let tok: Token =
            match self.tokens.peek() {
                Some(t) => (**t).clone(),
                None    => parse_err!(self, "Unexpected end of input"),
            };
        self.parse_exp_dispatch(tok)
    }
    // dispatch according to variant type
    fn parse_exp_dispatch(&mut self, peek_tok: Token) -> Result<implicAST, ParseErr> {
        //println!("Peek token: {:?}", peek_tok);
        match peek_tok {
            Token::Integer(_n)    => {
                                     let e = try!(self.parse_const());
                                      Ok(e)},
            Token::Boolean(_b)    => {
                                    let e = try!(self.parse_bool());
                                    Ok(e)},
            Token::Plus           => { // plus_exp
                                    let e = try!(self.parse_plus());
                                    Ok(e)
                                    },
            Token::Minus          => { // diff_exp
                                    let e = try!(self.parse_diff());
                                    Ok(e)
                                    },
            Token::IsZero         => { // iszero exp
                                    let e = try!(self.parse_iszero());
                                    Ok(e)
                                    },
            Token::If             => { // If-then-else expression
                                    let e = try!(self.parse_if_then_else());
                                    Ok(e)
                                    },
            Token::Identifier(_s) => { // identifier cannot be a reserved word
                                    let e = try!(self.parse_var());
                                    Ok(e)
                                    },
            Token::Let            => { // let-in expression
                                    let e = try!(self.parse_let_in());
                                    Ok(e)
                                    },
            Token::Proc           => { // proc expression
                                    let e = try!(self.parse_proc());
                                    Ok(e)
                                    },
            Token::Lparen         => { // call expression
                                    let e = try!(self.parse_call());
                                    Ok(e)
                                    },
            Token::LetRec         => { // letrec expression
                                    let e = try!(self.parse_letrec());
                                    Ok(e)
                                    },
            Token::Begin          => { // begin-end expression
                                    let e = try!(self.parse_beginend());
                                    Ok(e)
                                    },
            Token::Set            => { // set expression
                                    let e = try!(self.parse_set());
                                    Ok(e)
                                    },
            _                     => parse_err!(self, "dispatch: Unexpected token type {:?}", peek_tok),
            }
    }
    
     fn parse_const(&mut self) -> Result<implicAST, ParseErr> {
        let val: i32;
        let option_tok: Option<&Token> = self.tokens.next();
        let tok = match option_tok {
                    Some(t) => t,
                    _       => parse_err!(self, "parse_integer: Int expected but EOI found."),
                    };
        // extract var name string
        match tok.clone() {
            Token::Integer(i) => val = i,
            _                 => parse_err!(self, "parse_integer: Int token expected."),
        };
        Ok(implicAST::new_const_exp(val))
    }

   fn parse_bool(&mut self) -> Result<implicAST, ParseErr> {
        let val: bool;
        let option_tok: Option<&Token> = self.tokens.next();
        let tok = match option_tok {
                    Some(t) => t,
                    _       => parse_err!(self, "parse_bool: Bool expected but EOI found."),
                    };
        // extract var name string
        match tok.clone() {
            Token::Boolean(b) => val = b,
            _                 => parse_err!(self, "parse_var: Identifier tok expected."),
        };
        Ok(implicAST::new_boolean(val))
    }
    fn parse_var(&mut self) -> Result<implicAST, ParseErr> {
        let var: String;
        let option_tok: Option<&Token> = self.tokens.next();
        let tok = match option_tok {
                    Some(t) => t,
                    _       => parse_err!(self, "parse_var: Ident expected but EOI found."),
                    };
        // extract the variable name string
        match tok.clone() {
            Token::Identifier(s) => var = s,
            _                    => parse_err!(self, "parse_var: Identifier tok expected."),
        };
        Ok(implicAST::new_var_exp(&var))
    }
    
    // Gets the variable name.
    // Advances input stream by one token.
    fn get_string(&mut self) -> Result<String, ParseErr> {
        let option_tok: Option<&Token> = self.tokens.next();
        
        // Extracts the token from the Option
        let tok: Token = match option_tok {
                            Some(token) => token.clone(),
                            _ => parse_err!(self, "get_string1: Unexpected EOI")
                            };
        // Returns the Identifier in result form.
        let var: String =
            match tok {
                Token::Identifier(s) => s,
                _                    => parse_err!(self, "get_string2: Unexpected token type"),
                };
        Ok(var)
        }
        
    // Tries to match a token. Generates error messages.
    fn match_token(&mut self, tok: &Token)  -> Result<Option<implicAST>, ParseErr> {
        let option_tok: Option<&Token> = self.tokens.next();
//        println!("match_token. option_tok: {:?}", option_tok);
        match option_tok {
             Some(tok2) => {if tok == tok2 {
                                Ok(None)
                            } else {
                                //parse_err!(self, "Expected {:?} but found {:?}", tok, tok2);
                                println!("ParseErr: expected {:?} but found {:?}", tok, tok2);
                                return Err(ParseErr {message: "ParseErr!".to_string()})
                            }},
             _            => parse_err!(self, "ParseErr: Expected {:?} but found EOI", tok)
         }}
        
    fn parse_plus(&mut self) -> Result<implicAST, ParseErr> {
        //println!("Parse plus called.");
        try!(self.match_token(&Token::Plus));
        try!(self.match_token(&Token::Lparen));
        //println!("Parse plus found Lparen");
        let e1 = try!(self.parse_exp());
        //println!("Parse plus: about to look for comma");
        try!(self.match_token(&Token::Comma));
//         let result = self.match_token(&Token::Comma);
//         let meaningoflife = 42;
//         match result {
//             Ok(_v)  => (),
//             Err(_e) => { println!("About to execute parse err macro");
//                         parse_err!(self, "From parse_plus: missing comma {}", meaningoflife);}
//         }
        //println!("Parse plus: after about to look for comma");
        let e2 = try!(self.parse_exp());
        try!(self.match_token(&Token::Rparen));
        Ok(implicAST::new_plus_exp(&e1, &e2))
    }
    fn parse_diff(&mut self) -> Result<implicAST, ParseErr> {
        try!(self.match_token(&Token::Minus));
        try!(self.match_token(&Token::Lparen));
        let e1 = try!(self.parse_exp());
        try!(self.match_token(&Token::Comma));
        let e2 = try!(self.parse_exp());
        try!(self.match_token(&Token::Rparen));
        Ok(implicAST::new_diff_exp(&e1, &e2))
    }
    fn parse_iszero(&mut self) -> Result<implicAST, ParseErr> {
        try!(self.match_token(&Token::IsZero));
        try!(self.match_token(&Token::Lparen));
        let e = try!(self.parse_exp());
        try!(self.match_token(&Token::Rparen));
        Ok(implicAST::new_iszero(&e))
    }
    fn parse_if_then_else(&mut self) -> Result<implicAST, ParseErr> {
        try!(self.match_token(&Token::If));
        let e1 = try!(self.parse_exp());
        try!(self.match_token(&Token::Then));
        let e2 = try!(self.parse_exp());
        try!(self.match_token(&Token::Else));
        let e3 = try!(self.parse_exp());
        Ok(implicAST::new_if_exp(&e1, &e2, &e3))
    }
    fn parse_proc(&mut self) -> Result<implicAST, ParseErr> {
        //println!("Parse proc called!");
        try!(self.match_token(&Token::Proc));
        try!(self.match_token(&Token::Lparen));
        let mut vars: Vec<String> = Vec::new();
        let pk: Token =
            match self.tokens.peek() {
                Some(t) => (**t).clone(),
                None    => parse_err!(self, "parse_letrec. Unexpected EOI"),
            };
        if pk != Token::Rparen {
        loop { // one or more args
            let result = self.get_string();
            match result {
                Ok(s)   => vars.push(s),
                Err(_e) => break,
            };
            let pk: Token =
                match self.tokens.peek() {
                    Some(t) => (**t).clone(),
                    None    => parse_err!(self, "parse_letrec. Unexpected EOI"),
                };
            if pk == Token::Rparen { 
                break 
            } else {
                if pk != Token::Comma {
                    parse_err!(self, "Comma expected.")
                } else {
                let _dummy =self.match_token(&Token::Comma);
           }}
        }
        }
        try!(self.match_token(&Token::Rparen));
        let e1: implicAST = try!(self.parse_exp());
        //println!("Parse proc returned.");
        Ok(implicAST::new_proc_exp(&vars, &e1))
    }

    fn parse_let_in(&mut self) -> Result<implicAST, ParseErr> {
        //println!("Parse let called.");
        try!(self.match_token(&Token::Let));
        let s = try!(self.get_string());
        try!(self.match_token(&Token::Assign));
        //println!("Parse let matched =.");
        let e1 = try!(self.parse_exp());
        try!(self.match_token(&Token::In));
        //println!("Parse let matched in.");
        let e2 = try!(self.parse_exp());
        //println!("Parse let returned.");
        Ok(implicAST::new_let_exp(&s, &e1, &e2))
    }
    fn parse_single_letrec_decl(&mut self) -> Result<LetrecDecl, ParseErr> {
        let p_name: String = try!(self.get_string());
//        println!("parse_letrec. p_name: {}", p_name);
        try!(self.match_token(&Token::Lparen));
        let mut b_vars: Vec<String> = Vec::new();
        let pk: Token =
            match self.tokens.peek() {
                Some(t) => (**t).clone(),
                None    => parse_err!(self, "parse_letrec. Unexpected EOI"),
            };
        if pk != Token::Rparen {
        loop { // zero or more args
            let result = self.get_string();
            match result {
                Ok(s)   => b_vars.push(s),
                Err(_e) => break,
            };
            let pk: Token =
                match self.tokens.peek() {
                    Some(t) => (**t).clone(),
                    None    => parse_err!(self, "parse_single_letrec_decl. Unexpected EOI"),
                };
//            println!("pk: {:?}", pk);
            if pk == Token::Rparen { 
                break 
            } else {
                if pk != Token::Comma {
                    parse_err!(self, "Comma expected.")
                } else {
//                println!("Comma found");
                let _dummy = self.match_token(&Token::Comma);
           }}
        }}
//        println!("parse_letrec. b_vars: {:?}", b_vars);
        try!(self.match_token(&Token::Rparen));
        try!(self.match_token(&Token::Assign));
        let p_body = try!(self.parse_exp());
//        println!("parse_letrec. p_body: {}", p_body);
    Ok(LetrecDecl { pname: p_name, 
                    bvars: b_vars,
                    body : p_body})
    }
    fn parse_letrec(&mut self) -> Result<implicAST, ParseErr> {
        try!(self.match_token(&Token::LetRec));
        let mut declarations: Vec<LetrecDecl> = Vec::new();
        declarations.push(try!(self.parse_single_letrec_decl()));
        loop {
            let pk_in_token = 
                match self.tokens.peek() {
                    Some(t) => (**t).clone(),
                    None    => parse_err!(self, "parse_letrec: Unexpected EOI."),
                };
            if pk_in_token == Token::In {
                break
            } else {
                declarations.push(try!(self.parse_single_letrec_decl()));
            }
        };
        try!(self.match_token(&Token::In));
        let letrec_body = try!(self.parse_exp());
        Ok(build_ast_for_letrec(&declarations, letrec_body))
    }
        
    fn parse_call(&mut self) -> Result<implicAST, ParseErr> {
        //println!("Parse call called.");
        try!(self.match_token(&Token::Lparen));
        let e1: implicAST = try!(self.parse_exp());
        if !e1.is_legal_rator() {
            println!("From parse call: e1:{:#?}", e1);
            parse_err!(self, "illegal rator detected in parse_call")
        } else {
          let mut rands: Vec<implicAST> = Vec::new();
          loop { // zero or more args
            let result = self.parse_exp();
            match result {
                Ok(ast) => rands.push(ast),
                Err(_e) => break,
            }}
           try!(self.match_token(&Token::Rparen));
           //println!("Matched Rparen from parse call.");
           Ok(implicAST::new_call_exp(&e1, &rands))
          }}
          
    fn parse_beginend(&mut self) -> Result<implicAST, ParseErr> {
        try!(self.match_token(&Token::Begin));
        let mut exps: Vec<implicAST> = Vec::new();
        let e: implicAST = try!(self.parse_exp()); // must have at least 1 exp
        exps.push(e);
        
        let pk: Token =
            match self.tokens.peek() {
                Some(t) => (**t).clone(),
                None    => parse_err!(self, "parse_beginend. Unexpected EOI"),
            };
        if pk == Token::SemiColon {
            let _dummy =self.match_token(&Token::SemiColon);
            let pk: Token =
                match self.tokens.peek() {
                    Some(t) => (**t).clone(),
                    None    => parse_err!(self, "parse_begined. Unexpected EOI"),
                };
            if pk == Token::End { 
                parse_err!(self, "SemiColon should not be expected at the last expression.") 
            }
        } else {
            if pk != Token::End {
                parse_err!(self, "SemiColon expected.")
            }
        }
        loop { // zero or more exps
            let result = self.parse_exp();
            match result {
                Ok(exp) => exps.push(exp),
                Err(_e) => break,
            };
            let pk: Token =
                match self.tokens.peek() {
                    Some(t) => (**t).clone(),
                    None    => parse_err!(self, "parse_beginend. Unexpected EOI"),
                };
            if pk == Token::End { 
                break 
            } else {
                if pk != Token::SemiColon {
                    parse_err!(self, "SemiColon expected.")
                } else {
                let _dummy =self.match_token(&Token::SemiColon);
                let pk: Token =
                match self.tokens.peek() {
                    Some(t) => (**t).clone(),
                    None    => parse_err!(self, "parse_beginend. Unexpected EOI"),
                };
                if pk == Token::End { 
                    parse_err!(self, "SemiColon should not be expected at the last expression.") 
                }
           }}
        }
        try!(self.match_token(&Token::End));
        Ok(implicAST::new_beginend_exp(&exps))
    }
    
    fn parse_set(&mut self) -> Result<implicAST, ParseErr> {
        try!(self.match_token(&Token::Set));
        let var: String;
        let option_tok: Option<&Token> = self.tokens.next();
        let tok = match option_tok {
                    Some(t) => t,
                    _       => parse_err!(self, "parse_var: Ident expected but EOI found."),
                    };
        // extract the variable name string
        match tok.clone() {
            Token::Identifier(s) => var = s,
            _                    => parse_err!(self, "parse_var: Identifier tok expected."),
        };
        try!(self.match_token(&Token::Assign));
        let exp = try!(self.parse_exp());
        Ok(implicAST::new_set_exp(&var, &exp))
    }
}

fn build_ast_for_letrec(decs: &Vec<LetrecDecl>,
                        lr_body: implicAST) -> implicAST {
    let mut p_names : Vec<String>      = Vec::new();
    let mut b_vars  : Vec<Vec<String>> = Vec::new();
    let mut p_bodies: Vec<Rc<implicAST>> = Vec::new();
    for dec in decs  {
        p_names.push((dec.pname).clone());
        b_vars.push((dec.bvars).clone());
        p_bodies.push(Rc::new((dec.body).clone()));
    };
    implicAST::LetRecExp(p_names.clone(),
                             b_vars.clone(),
                             p_bodies,
                             Rc::new(lr_body))
}

// END OF PARSER

// START OF ENV

//use std::rc::Rc;
//use std::fmt;
//use implicit_ref_ast::*;
use /*implicit_ref_env::*/implicENV::*;

#[derive(Debug,Clone)]
pub enum implicENV { // an environment has three variants
    EmptyEnv,
    ExtendEnv(String, DenVal, Rc<implicENV>),
    ExtendEnvRec(Vec<String>,           // pname
                 Vec<Vec<String>>,      // bvars
                 Vec<Rc<implicAST>>,   
                 Rc<implicENV>),
}

impl implicENV {
    // Constructors for the three variants
    pub fn new_env() -> Self {
        EmptyEnv
    }
    pub fn extend_env(&self, s:&String, val: &DenVal) -> Self {
        ExtendEnv(s.clone(), val.clone(), Rc::new(self.clone()))
    }
    pub fn extend_env_rec(&self, 
                          pnames: &Vec<String>, 
                          b_vars: &Vec<Vec<String>>, 
                          bodies: &Vec<Rc<implicAST>>) -> Self {
        ExtendEnvRec(pnames.clone(),        // p_names
                     b_vars.clone(),        // b_vars
                     bodies.clone(),        // bodies
                     Rc::new(self.clone())  // saved env
                    )
    }
    pub fn apply_env(&self, s:&String) -> Option<DenVal> {
        match self.clone() {
            EmptyEnv => None,
            ExtendEnv(var, val, env) => 
                                       if s[..] == var[..] {
                                        Some(val)
                                       } else {
                                        env.apply_env(s)
                                        },
            ExtendEnvRec(pnames, bvars, bodies, saved_env) 
                         => {
                         //println!("\nAPPLY_ENV entered. ***");
                         /*println!("    matched ExtendEnvRec. ***");
                         println!("    s_var  : {}", s);
                         println!("    pnames : {:?}", pnames);
                         println!("    bvars  : {:?}",  bvars);
                         println!("    bodies :");
                         for body in (bodies.clone()) {
                            println!("             {:?}", body);
                            }*/
                         //println!("  saved_env: {:?}", saved_env);
                         let mut opt_proc: Option<DenVal> = None;
                         // search pnames
                         // if found return correct vars and body
                         //println!("len is: {}", bodies.len());
                         for i in 0..pnames.len() {
                            let pname: String = pnames[i].clone();
                            if s[..] == pname[..] {
                                opt_proc =
                                Some(DenVal::Procedure(
                                                        bvars[i].clone(),
                                                        bodies[i].clone(),
                                                        Rc::new((*self).clone())
                                                        )); 
                            }};
                         if opt_proc.is_none() {
                            opt_proc = saved_env.apply_env(s)
                         };
                         //println!("proc is: {:?}", opt_proc.clone());
                         /*match opt_proc.clone() {
                            Some(v) => println!("   proc:\n{:?}", v),
                            None    => println!("No val in env!!!"),
                         }*/
                         //println!("APPLY_ENV returned. ***");
                         opt_proc  // return Option<DenVal>
                        },
        }}
    pub fn is_null_env(&self) -> bool {
        match self.clone() {
            EmptyEnv  => true,
            _         => false,
        }}
    pub fn to_string(&self) -> String {
        match self.clone() {
            EmptyEnv => "[]".to_string(),
            ExtendEnv(var,val,env) => {let mut temp = "[ ".to_string();
                                       temp.push_str(&(var.to_string()));
                                       temp.push_str(&("=".to_string()));
                                       temp.push_str(&(val.to_string()));
                                       temp.push_str(&(" ".to_string()));
                                       temp.push_str(&(env.to_string()));
                                       temp.push_str(&(" ]".to_string()));
                                       temp},
            ExtendEnvRec(p_names, b_vars_lst, p_bodies, saved_env)
                         =>
                         {let mut temp = "[".to_string();
                         for i in 0..p_names.len() {
                            if i!=0 {temp.push_str(&("; ".to_string()))};
                            temp.push_str(&(p_names[i].to_string()));
                            temp.push_str(&("=(".to_string()));
                            let bvars = b_vars_lst[i].clone();
                            for i in 0..bvars.len() {
                                temp.push_str(&(bvars[i].to_string()));
                                if (i+1) < bvars.len() {
                                    temp.push_str(&(", ".to_string()));
                                }
                            }
                            temp.push_str(&(") ".to_string()));
                            temp.push_str(&(p_bodies[i].to_string()));
                         }
                         temp.push_str(&("] ".to_string()));
                         temp.push_str(&(saved_env.to_string()));
                         temp}
        }}}

impl fmt::Display for implicENV {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = format!("");
        let s1 = self.to_string();
        s.push_str(&s1);
        write!(f, "{}", s)
    }}

#[derive(Debug,Clone)]
pub enum DenVal {
    Integer(i32),
    Boolean(bool),
    //Ref(Rc<DenVal>),
    Procedure(Vec<String>, Rc<implicAST>, Rc<implicENV>),
    //        bvars        body            env
}

/*impl DenVal {
    pub fn to_string(&self) -> String {
        match self.clone() {
            DenVal::Integer(i) => i.to_string(),
            DenVal::Boolean(b) => b.to_string(),
            DenVal::Procedure(vars, exp, env) => 
                              {let mut temp = "proc (".to_string();
                               for i in 0..vars.len() {
                                    temp.push_str(&(vars[i]));
                                    if (i+1) < vars.len() {
                                        temp.push_str(&(", ".to_string()));}
                               }
                               temp.push_str(&(") ".to_string()));
                               temp.push_str(&(exp.to_string()));
                               temp.push_str(&("\n".to_string()));
                               temp.push_str(&(env.to_string()));
                               temp
                               }}}}*/

impl fmt::Display for DenVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = format!("");
        let s1 = self.to_string();
        s.push_str(&s1);
        write!(f, "{}", s)
    }}

#[cfg(test)]
mod test {
    use super::implicENV;
    use super::DenVal;
    
    #[test]
    fn basic_tests() {
        let null_env = implicENV::new_env();
        assert!(null_env.is_null_env());
        
        let env2 = null_env.extend_env(&("var1".to_string()), 
                                    &DenVal::Integer(25));
        assert!(!(env2.is_null_env()));
    }
}

// END OF ENV

// START OF STORE

//use implicit_ref_env::*;
#[derive(Debug,Clone)]
pub struct ExplStor {
    contents: Vec<DenVal>,
}

impl<'a> ExplStor {
    pub fn new_stor() -> Self {
        let st: ExplStor = ExplStor { contents: Vec::new()};
        st
    }
    pub fn new_ref(&mut self, v: &'a DenVal) -> usize {
        let next_ref: usize = self.contents.len();
        self.contents.push(v.clone());
        next_ref
    }
    pub fn deref(&self, r: usize) -> DenVal {
        (self.contents[r]).clone()
    }
    pub fn setref(&mut self, r: usize, v: &DenVal) {
        if (r+1) > self.contents.len() {
            panic!("setref out of range: vector len = {}, r={}", self.contents.len(), r);
        } else {
        self.contents[r] = v.clone();
        }
    }
    pub fn len(&self) -> usize {
        self.contents.len()
    }
}

// END OF STORE

// START OF EVAL

//use implicit_ref_scanner::*;
//use implicit_ref_parser::*;
//use implicit_ref_ast::*;
use /*implicit_ref_exp::*/implicAST::*;
//use implicit_ref_env::*;

//use std::fmt;
//use std::rc::Rc;

pub fn evaluate(ast: &implicAST) -> Result<DenVal, EvalERR> {
    //println!("Starting to evaluate AST.\n");
    value_of(ast, &implicENV::new_env(), &mut ExplStor::new_stor())
}

pub struct EvalERR {
    message: String,
}

impl fmt::Display for EvalERR {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "EvalError: {}", self.message)
    }}
impl fmt::Debug for EvalERR {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "EvalError: {}", self.message)
    }}

macro_rules! eval_err {
    ($($arg:tt)*) => (
        return Err(EvalERR { message: format!($($arg)*)})
    )
}

fn value_of(ast: &implicAST, env: &implicENV, stor: &mut ExplStor) -> Result<DenVal, EvalERR> {
    match ast.clone() {
        ConstExp(int)           => Ok(DenVal::Integer(int)),
        Boolean(b)              => Ok(DenVal::Boolean(b)),
        PlusExp(e1, e2)         => value_of_plus_exp(&(*e1), &(*e2), env, stor),
        DiffExp(e1, e2)         => value_of_diff_exp(&(*e1), &(*e2), env, stor),
        IsZeroExp(e)            => value_of_iszero(&(*e), env, stor),
        IfExp(e1,e2,e3)         => value_of_if(&(*e1),&(*e2),&(*e3), env, stor),
        VarExp(s)               => value_of_var(&s, env, stor),
        LetExp(s,e1,e2)         => value_of_let(&s, &(*e1), &(*e2), env, stor),
        ProcExp(vars, e)        => value_of_proc(&vars, &(*e), env, stor),
        CallExp(rator, rands)   => value_of_call(&(*rator), &rands, env, stor),
        LetRecExp(pnames, bvars, p_bodies, in_body)
                                =>
                    value_of_letrec(&pnames, &bvars, &p_bodies, &in_body, env, stor),
        SetExp(s, exp)          => value_of_set(&s, &(*exp), env, stor),
        BeginEndExp(exps)       => value_of_beginend(&exps, env, stor),
    }}

fn value_of_var(var: &String, env: &implicENV, stor: &mut ExplStor) -> Result<DenVal,EvalERR> {
    match env.apply_env(var) {
        Some(val) => {match val {
        DenVal::Integer(i) => {let exp = stor.deref(i as usize);
                               match exp {
                                   DenVal::Integer(i)  => Ok(DenVal::Integer(i)),
                                   DenVal::Boolean(b)  => Ok(DenVal::Boolean(b)),
                                   DenVal::Procedure(vars, exp2, env)
                                                       => Ok(DenVal::Procedure(vars, exp2, env)),
                               }
                              },
        _                  => Ok(val),
    }},
        None    => eval_err!("value_of_var: Unbound var: {:?}", var),
    }
}

// Value is a representation of the procedure
fn value_of_proc(vars: &Vec<String>, 
                 body: &implicAST, 
                 env: &implicENV, stor: &mut ExplStor) -> Result<DenVal,EvalERR> {
    Ok(DenVal::Procedure(vars.clone(), 
        Rc::new(body.clone()), 
        Rc::new(env.clone())))
}

fn value_of_call(rator: &implicAST, 
                 rands: &Vec<Rc<implicAST>>, 
                 env: &implicENV, stor: &mut ExplStor)       -> Result<DenVal,EvalERR> {
    //println!("\nVALUE_OF_CALL entered.");
    //println!("    rator: {:?}", rator);
    //println!("    rands: {:?}", rands);
    //println!("    env  :\n{:#?}", env);
    //println!("        Entering match rator:");
    // check if operator is anything that can eval to a procedure
    let closure = try!(
        match rator.clone() {
            VarExp(s_var)   => value_of_var(&s_var, env, stor),
            ProcExp(v, e)   => value_of_proc(&v, &e, env, stor),
            CallExp(v, e)   => value_of_call(&(*v).clone(), &e, env, stor),
            IfExp(e1,e2,e3) => value_of_if(&(*e1),&(*e2),&(*e3), env, stor),
            LetExp(s,e1,e2) => value_of_let(&s, &(*e1), &(*e2), env, stor),
            LetRecExp(pnames, bvars, pbodies, inbody)
                            => value_of_letrec(&pnames, &bvars, &pbodies, &inbody, env, stor),
            BeginEndExp(exps)
                            => value_of_beginend(&exps, env, stor),
            _               => eval_err!("In value_of_call. rator {:?} has no value.", 
                                         rator
                                        ),
            });
    //println!("\nclosure: \n{:?}", closure);
    
    let mut evald_rands: Vec<DenVal> = Vec::new();
    for rand in rands {
        let rand = try!(value_of(rand, env, stor));
        let new_rand = stor.new_ref(&rand) as i32;
        let new = DenVal::Integer(new_rand);
        evald_rands.push(new);
    };
    
    //println!("Calling apply_proc from value_of_call");
    apply_proc(&closure, &evald_rands, stor)
}

// does not eval rands.
// they are evald before apply_proc is called
// environment is stored in closure
fn apply_proc(closure: &DenVal, rands: &Vec<DenVal>, stor: &mut ExplStor) -> Result<DenVal, EvalERR> {
    //println!("\nAPPLY_PROC entered.");
    //println!("closure: \n{:?}", closure);
    //println!("\nEvald rands : {:?}", rands);
    let mut closure_vars: Vec<String> = Vec::new();
    let mut closure_body: implicAST = implicAST::new_boolean(true);
    let mut closure_env:  implicENV  = implicENV::new_env();
    match closure.clone() {
        DenVal::Procedure(vars, body, env) => {closure_vars = vars;
                                               closure_body = (*body).clone();
                                               closure_env  = (*env).clone();
                                               let dummy: bool = true;
                                               Some(dummy)}, // unused
        _ => None,
    };
    /*if rands.len() != closure_vars.len() {
        eval_err!("rands.len(): {} != closure_vars.len(): {}", 
                    rands.len(), closure_vars.len());
    };*/
    let extended_env = extend_env_rands(&closure_vars, rands, &closure_env);
    value_of(&closure_body, &extended_env, stor)
    }

fn extend_env_rands(vars:  &Vec<String>,
                    rands: &Vec<DenVal>, 
                    env:   &implicENV) -> implicENV {
    let mut extended_env: implicENV = env.clone();
    for i in 0..vars.len() {
        extended_env = extended_env.extend_env(&(vars[i].to_string()),
                                               &rands[i]);
    }
    extended_env
}
    
fn value_of_let(s: &String, 
                e1: &implicAST, 
                e2: &implicAST, 
                env: &implicENV, stor: &mut ExplStor) -> Result<DenVal,EvalERR>{
    let val: DenVal = try!(value_of(e1, env, stor));
    let new_val = stor.new_ref(&val) as i32;
    let new = (DenVal::Integer(new_val));
    let new_env = env.extend_env(s, &new);
    value_of(e2, &new_env, stor)
}

fn value_of_letrec(pnames: &Vec<String>,  
                   b_vars: &Vec<Vec<String>>, 
                   bodies: &Vec<Rc<implicAST>>, 
                   letrec_body: &implicAST,
                   env: &implicENV, stor: &mut ExplStor) -> Result<DenVal,EvalERR> {
    //println!("VALUE_OF_LETREC entered.");
    /*println!("    pnames: {:?}", pnames);
    println!("    b_vars: {:?}", b_vars);
    println!("    bodies:");
    for body in bodies {
        println!("            {:?}", body);
    }
    println!("    l_body: {}", letrec_body);
    println!("    env   : {}", env);*/
    //println!("    About to evaluate l_body: {}", letrec_body);
    let new_env = env.extend_env_rec(pnames, b_vars, bodies);
    value_of(letrec_body, &new_env, stor)
}

fn value_of_if(e1: &implicAST, 
               e2: &implicAST, 
               e3: &implicAST, 
               env: &implicENV, stor: &mut ExplStor) -> Result<DenVal,EvalERR> {
    /*println!("\nVALUE_OF_IF entered.");
    println!("    e1 : {:?}", e1);
    println!("    e2 : {:?}", e2);
    println!("    e3 : {:?}", e3);
    println!("    env: {}", env);*/
    let bool_test = try!(value_of(e1, env, stor));
    let bool_var = 
        match bool_test {
            DenVal::Boolean(b) => b,
            _ => eval_err!("value_of_if. Not a boolean value: {:?}", bool_test),
        };
    if bool_var {
        value_of(e2, env, stor)
    } else {
        value_of(e3, env, stor)
    }}

fn value_of_iszero(e: &implicAST, env: &implicENV, stor: &mut ExplStor) -> Result<DenVal,EvalERR> {
    let val = try!(value_of(e, env, stor));
    match val {
        DenVal::Integer(i)   => Ok(DenVal::Boolean(i == 0)),
            _                => eval_err!("Expected integer {:?}", val),
    }}

// typechecked difference
fn value_of_diff_exp(arg1: &implicAST, 
                     arg2: &implicAST, 
                     env: &implicENV, stor: &mut ExplStor) -> Result<DenVal,EvalERR> {
    /*println!("\nVALUE_OF_DIFF_EXP entered.");
    println!("    arg1: {}", arg1);
    println!("    arg2: {}", arg2);
    println!("    env : {}", env);*/
    let val1 = try!(value_of(arg1, env, stor));
    let val2 = try!(value_of(arg2, env, stor));
    value_of_diff_exp_checked(&val1, &val2)
}

// typechecked sum
fn value_of_plus_exp(arg1: &implicAST, 
                     arg2: &implicAST, 
                     env: &implicENV, stor: &mut ExplStor) -> Result<DenVal,EvalERR> {
    /*println!("\nVALUE_OF_PLUS_EXP entered.");
    println!("    arg1: {}", arg1);
    println!("    arg2: {}", arg2);
    println!("    env : {}", env);*/
    let val1 = try!(value_of(arg1, env, stor));
    let val2 = try!(value_of(arg2, env, stor));
    value_of_plus_exp_checked(&val1, &val2)
}

// type checked
fn value_of_diff_exp_checked(a1: &DenVal, a2: &DenVal) -> Result<DenVal,EvalERR> {
    let a1_int_val: i32 = 
        match *a1 {
            DenVal::Integer(i) => i,
            _  => eval_err!("value_of_diff_exp_checked. Int expected: a1={:?}", a1),
            };
    let a2_int_val: i32 = 
        match *a2 {
            DenVal::Integer(i) => i,
            _ => eval_err!("value_of_diff_exp_checked. Int expected: a2={:?}", a2),
            };
    Ok(DenVal::Integer(a1_int_val - a2_int_val))
}

// type checked
fn value_of_plus_exp_checked(a1: &DenVal, a2: &DenVal) -> Result<DenVal,EvalERR> {
    let a1_int_val: i32 = 
        match *a1 {
            DenVal::Integer(i) => i,
            _ => eval_err!("value_of_plus_exp_checked. Int expected: a1={:?}", a1),
            };
    let a2_int_val: i32 = 
        match *a2 {
            DenVal::Integer(i) => i,
            _ => eval_err!("value_of_plus_exp_checked. Int expected: a2={:?}", a2),
            };
    Ok(DenVal::Integer(a1_int_val + a2_int_val))
}

fn value_of_beginend(exps: &Vec<Rc<implicAST>>, env: &implicENV, stor: &mut ExplStor) -> Result<DenVal,EvalERR> {
    for i in 0..exps.len()-1 {
        value_of(&exps[i], &env, stor);
    }
    value_of(&exps[exps.len()-1], &env, stor)
}

fn value_of_set(var: &String, exp: &implicAST, env: &implicENV, stor: &mut ExplStor) -> Result<DenVal,EvalERR> {
    match env.apply_env(var) {
        Some(val1) => {let val2 = value_of(&(*exp), env, stor);
    match val1 {
        DenVal::Integer(i) => {stor.setref(i as usize, &val2.unwrap());
                               Ok(DenVal::Integer(23))
                              },
        _                  => eval_err!("Expected integer for set's arg1."),
    }},
        None    => eval_err!("value_of_var: Unbound var: {:?}", var),
    }
}

// END OF EVAL

// START OF MAIN

//extern crate ImplicitRef_yxw0087;  // letrec lang w/ multidecl and n-arg procs

//use ImplicitRef_yxw0087::implicit_ref_scanner::*;
//use ImplicitRef_yxw0087::implicit_ref_parser::*;
//use ImplicitRef_yxw0087::implicit_ref_ast::*;
//use ImplicitRef_yxw0087::implicit_ref_eval::*;

fn show_toks(s: &String) {
    //println!("Lexing: {}", s);
    let result = tokenize(s);
    match result {
        Ok(v)  => println!("Tokens: {:?}\n", v),
        Err(e) => println!("Token err: {:#?}\n", e),
    };}

fn show_parse(s: &String) -> Result<implicAST, &'static str> {
    //println!("Parsing: \n{}", s);
    let tok_result = tokenize(s);
    match tok_result {
        Ok(v)   => {//println!("Tokens: {:?}\n", v);
                    let ast_result = parse(&v);
                    match ast_result {
                        Ok(v2)  => { println!("AST: \n{:#?}\n", v2);
                                     println!("Stringified AST: {}\n", v2);
                                     Ok(v2)
                                     },
                        Err(_e) => Err("Parse error."),
                    }},
//                     println!("AST: \n{:#?}\n", ast);
//                     println!("Stringified AST: {}\n", ast);
//                     Ok(ast)},
        Err(_e) => Err("Tokenize error."),
    }}

#[allow(dead_code)]
fn main() {
    let global_str =
    "let x = 0
     in letrec even () = if iszero(x) then 1
                                             else begin
                                                    set x = -(x, 1);
                                                    (odd)
                                                  end
                odd () = if iszero(x) then 0
                                             else begin
                                                    set x = -(x, 1);
                                                    (even)
                                                  end
        in begin
            set x = 5;
            (odd)
           end
    ";
    let tree_result = show_parse(&(global_str.to_string()));
    let tree = tree_result.unwrap();
    let val = evaluate(&tree);
    let denval = match val {
        Ok(DenVal::Integer(v)) => v,
        _     => 0,
    };
    assert_eq!(denval, 1);

    let hidden_str =
    "let g = let count = 0
             in proc () begin
                            set count = -(count, -1);
                            count
                        end
     in let a = (g 11)
        in let b = (g 11)
           in -(a, b)
    ";
    let tree_result = show_parse(&(hidden_str.to_string()));
    let tree = tree_result.unwrap();
    let val = evaluate(&tree);
    let denval = match val {
        Ok(DenVal::Integer(v)) => v,
        _     => 0,
    };
    assert_eq!(denval, -1);


    let sim_str =
    "let f = proc (x) 
                proc (y)
                    begin
                        set x = +(x, 1);
                        -(x, y)
                    end
     in ((f 44) 33)
    ";
    let tree_result = show_parse(&(sim_str.to_string()));
    let tree = tree_result.unwrap();
    let val = evaluate(&tree);
    let denval = match val {
        Ok(DenVal::Integer(v)) => v,
        _     => 0,
    };
    assert_eq!(denval, 12);


    let sim_str =
    "let x = 22
     in let f = proc (z) let zz = -(z, x)
                         in zz
        in -((f 66), (f 55))
    ";
    let tree_result = show_parse(&(sim_str.to_string()));
    let tree = tree_result.unwrap();
    let val = evaluate(&tree);
    let denval = match val {
        Ok(DenVal::Integer(v)) => v,
        _     => 0,
    };
    assert_eq!(denval, 11);

}