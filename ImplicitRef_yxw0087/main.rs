extern crate ImplicitRef_yxw0087;

use ImplicitRef_yxw0087::implicit_ref_scanner::*;
use ImplicitRef_yxw0087::implicit_ref_parser::*;
use ImplicitRef_yxw0087::implicit_ref_ast::*;
use ImplicitRef_yxw0087::implicit_ref_eval::*;

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

#[test]
fn test_gobal_var_shared_by_two_fns() { // page 117
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
}

#[test]
fn test_hidden_state() { // page 117
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
}

#[test]
fn test_simulation_trace() { // page 120
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
}

#[test]
fn test_simulation_trace() { // page 114 translated to Implicit
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

}