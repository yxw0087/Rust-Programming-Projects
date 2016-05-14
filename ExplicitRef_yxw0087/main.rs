extern crate ExplicitRef_yxw0087;

use ExplicitRef_yxw0087::explicit_ref_scanner::*;
use ExplicitRef_yxw0087::explicit_ref_parser::*;
use ExplicitRef_yxw0087::explicit_ref_env::*;
use ExplicitRef_yxw0087::explicit_ref_ast::*;
use ExplicitRef_yxw0087::explicit_ref_eval::*;
use ExplicitRef_yxw0087::explicit_ref_store::*;

fn show_toks(s: &String) {
    //println!("Lexing: {}", s);
    let result = tokenize(s);
    match result {
        Ok(v)  => println!("Tokens: {:?}\n", v),
        Err(e) => println!("Token err: {:#?}\n", e),
    };}
// fn show_parse(s: &String) -> Result<ExplicAST, ParseErr> {
//     println!("Parsing: \n{}", s);
//     let tok_result = tokenize(s);
//     match tok_result {
//         Ok(v)   => {//println!("Tokens: {:?}\n", v);
//                     let ast = try!(parse(&v));
//                     println!("AST in debug format: \n{:#?}\n", ast);
//                     println!("Stringified AST: {}\n", ast);
//                     Ok(ast)},
//         Err(_e) => gen_token_parse_err(),
//     }}

fn show_parse(s: &String) -> Result<ExplicAST, &'static str> {
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
#[test]
fn test_gobal_var_shared_by_two_fns() { // page 105
    let global_str =
    "let x = newref(0)
     in letrec even () = if iszero(deref(x)) then 1
                                             else begin
                                                    setref(x, -(deref(x), 1));
                                                    (odd)
                                                  end
                odd () = if iszero(deref(x)) then 0
                                             else begin
                                                    setref(x, -(deref(x), 1));
                                                    (even)
                                                  end
        in begin
            setref(x, 5);
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
}

#[test]
fn test_hidden_state() { // page 105
    let hidden_str =
    "let g = let count = newref(0)
             in proc () begin
                            setref(count, -(deref(count), -1));
                            deref(count)
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
}

#[test]
fn test_reference_chain() { // page 106
    let refchain_str =
    "let x = newref(newref(0))
     in begin
            setref(deref(x), 11);
            deref(deref(x))
        end
    ";
    let tree_result = show_parse(&(refchain_str.to_string()));
    let tree = tree_result.unwrap();
    let val = evaluate(&tree);
    let denval = match val {
        Ok(DenVal::Integer(v)) => v,
        _     => 0,
    };
    assert_eq!(denval, 11);
}

#[test]
fn test_simulation_trace() { // page 114
    let sim_str =
    "let x = newref(22)
     in let f = proc (z) let zz = newref(-(z,deref(x)))
                         in deref(zz)
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
}