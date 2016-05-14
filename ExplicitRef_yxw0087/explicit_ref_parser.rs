use explicit_ref_scanner::*;
use explicit_ref_ast::*;

use std::fmt;
use std::rc::Rc;
use std::slice;
use std::iter;

pub fn parse(tokens: &Vec<Token>) -> Result<ExplicAST, ParseErr> {
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
// pub fn gen_token_parse_err() -> Result<ExplicAST, ParseErr> {
//     parse_err!("Unable to tokenize.")
// }

#[derive(Debug,Clone)]
struct LetrecDecl {
    pname: String,
    bvars: Vec<String>,
    body:  ExplicAST,
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
    fn parse(tokens: &Vec<Token>) -> Result<ExplicAST, ParseErr> {
//        let mut parser = Parser { tokens: tokens.iter() };
        let mut parser = Parser { tokens: tokens.iter().peekable() };
        let ast_root = parser.parse_exp();
        let option_next_tok = parser.tokens.next();
        match option_next_tok {
            Some(tok) => parse_err!(self, "ParseErr toplevel: Extra input at end of parse: {:?}", tok),
            _          => ast_root,
        }
    }
    fn parse_exp(&mut self) -> Result<ExplicAST, ParseErr> {
        let tok: Token =
            match self.tokens.peek() {
                Some(t) => (**t).clone(),
                None    => parse_err!(self, "Unexpected end of input"),
            };
        self.parse_exp_dispatch(tok)
    }
    // dispatch according to variant type
    fn parse_exp_dispatch(&mut self, peek_tok: Token) -> Result<ExplicAST, ParseErr> {
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
            Token::Setref         => { // setref expression
                                    let e = try!(self.parse_setref());
                                    Ok(e)
                                    },
            Token::Newref         => { // newref expression
                                    let e = try!(self.parse_newref());
                                    Ok(e)
                                    },
            Token::Deref          => { // deref expression
                                    let e = try!(self.parse_deref());
                                    Ok(e)
                                    }
            _                     => parse_err!(self, "dispatch: Unexpected token type {:?}", peek_tok),
            }
    }
    
     fn parse_const(&mut self) -> Result<ExplicAST, ParseErr> {
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
        Ok(ExplicAST::new_const_exp(val))
    }

   fn parse_bool(&mut self) -> Result<ExplicAST, ParseErr> {
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
        Ok(ExplicAST::new_boolean(val))
    }
    fn parse_var(&mut self) -> Result<ExplicAST, ParseErr> {
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
        Ok(ExplicAST::new_var_exp(&var))
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
    fn match_token(&mut self, tok: &Token)  -> Result<Option<ExplicAST>, ParseErr> {
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
        
    fn parse_plus(&mut self) -> Result<ExplicAST, ParseErr> {
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
        Ok(ExplicAST::new_plus_exp(&e1, &e2))
    }
    fn parse_diff(&mut self) -> Result<ExplicAST, ParseErr> {
        try!(self.match_token(&Token::Minus));
        try!(self.match_token(&Token::Lparen));
        let e1 = try!(self.parse_exp());
        try!(self.match_token(&Token::Comma));
        let e2 = try!(self.parse_exp());
        try!(self.match_token(&Token::Rparen));
        Ok(ExplicAST::new_diff_exp(&e1, &e2))
    }
    fn parse_iszero(&mut self) -> Result<ExplicAST, ParseErr> {
        try!(self.match_token(&Token::IsZero));
        try!(self.match_token(&Token::Lparen));
        let e = try!(self.parse_exp());
        try!(self.match_token(&Token::Rparen));
        Ok(ExplicAST::new_iszero(&e))
    }
    fn parse_if_then_else(&mut self) -> Result<ExplicAST, ParseErr> {
        try!(self.match_token(&Token::If));
        let e1 = try!(self.parse_exp());
        try!(self.match_token(&Token::Then));
        let e2 = try!(self.parse_exp());
        try!(self.match_token(&Token::Else));
        let e3 = try!(self.parse_exp());
        Ok(ExplicAST::new_if_exp(&e1, &e2, &e3))
    }
    fn parse_proc(&mut self) -> Result<ExplicAST, ParseErr> {
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
        let e1: ExplicAST = try!(self.parse_exp());
        //println!("Parse proc returned.");
        Ok(ExplicAST::new_proc_exp(&vars, &e1))
    }

    fn parse_let_in(&mut self) -> Result<ExplicAST, ParseErr> {
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
        Ok(ExplicAST::new_let_exp(&s, &e1, &e2))
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
    fn parse_letrec(&mut self) -> Result<ExplicAST, ParseErr> {
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
        
    fn parse_call(&mut self) -> Result<ExplicAST, ParseErr> {
        //println!("Parse call called.");
        try!(self.match_token(&Token::Lparen));
        let e1: ExplicAST = try!(self.parse_exp());
        if !e1.is_legal_rator() {
            println!("From parse call: e1:{:#?}", e1);
            parse_err!(self, "illegal rator detected in parse_call")
        } else {
          let mut rands: Vec<ExplicAST> = Vec::new();
          loop { // zero or more args
            let result = self.parse_exp();
            match result {
                Ok(ast) => rands.push(ast),
                Err(_e) => break,
            }}
           try!(self.match_token(&Token::Rparen));
           //println!("Matched Rparen from parse call.");
           Ok(ExplicAST::new_call_exp(&e1, &rands))
          }}
          
    fn parse_beginend(&mut self) -> Result<ExplicAST, ParseErr> {
        try!(self.match_token(&Token::Begin));
        let mut exps: Vec<ExplicAST> = Vec::new();
        let e: ExplicAST = try!(self.parse_exp()); // must have at least 1 exp
        exps.push(e);
        let pk: Token =
            match self.tokens.peek() {
                Some(t) => (**t).clone(),
                None    => parse_err!(self, "parse_letrec. Unexpected EOI"),
            };
        if pk == Token::SemiColon {
            let _dummy =self.match_token(&Token::SemiColon);
            let pk: Token =
                match self.tokens.peek() {
                    Some(t) => (**t).clone(),
                    None    => parse_err!(self, "parse_letrec. Unexpected EOI"),
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
                    None    => parse_err!(self, "parse_letrec. Unexpected EOI"),
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
                    None    => parse_err!(self, "parse_letrec. Unexpected EOI"),
                };
                if pk == Token::End { 
                    parse_err!(self, "SemiColon should not be expected at the last expression.") 
                }
           }}
        }
        try!(self.match_token(&Token::End));
        Ok(ExplicAST::new_beginend_exp(&exps))
    }
    
    fn parse_newref(&mut self) -> Result<ExplicAST, ParseErr> {
        try!(self.match_token(&Token::Newref));
        try!(self.match_token(&Token::Lparen));
        let e = try!(self.parse_exp());
        try!(self.match_token(&Token::Rparen));
        Ok(ExplicAST::new_newref_exp(&e))
    }
    
    fn parse_deref(&mut self) -> Result<ExplicAST, ParseErr> {
        try!(self.match_token(&Token::Deref));
        try!(self.match_token(&Token::Lparen));
        let e = try!(self.parse_exp());
        try!(self.match_token(&Token::Rparen));
        Ok(ExplicAST::new_deref_exp(&e))
    }
    
    fn parse_setref(&mut self) -> Result<ExplicAST, ParseErr> {
        try!(self.match_token(&Token::Setref));
        try!(self.match_token(&Token::Lparen));
        let e1 = try!(self.parse_exp());
        try!(self.match_token(&Token::Comma));
        let e2 = try!(self.parse_exp());
        try!(self.match_token(&Token::Rparen));
        Ok(ExplicAST::new_setref_exp(&e1, &e2))
    }
}

fn build_ast_for_letrec(decs: &Vec<LetrecDecl>,
                        lr_body: ExplicAST) -> ExplicAST {
    let mut p_names : Vec<String>      = Vec::new();
    let mut b_vars  : Vec<Vec<String>> = Vec::new();
    let mut p_bodies: Vec<Rc<ExplicAST>> = Vec::new();
    for dec in decs  {
        p_names.push((dec.pname).clone());
        b_vars.push((dec.bvars).clone());
        p_bodies.push(Rc::new((dec.body).clone()));
    };
    ExplicAST::LetRecExp(p_names.clone(),
                             b_vars.clone(),
                             p_bodies,
                             Rc::new(lr_body))
}