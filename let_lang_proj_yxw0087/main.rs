use root::let_lang_scanner::*;
use root::let_lang_parser::*;
use root::let_lang_env::*;
use root::let_lang_exp::*;
use root::int_bool::*;

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
    let str2 = "-(1, false)";
    let result: Result<Vec<Token>, LexErr> = tokenize(str2);
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