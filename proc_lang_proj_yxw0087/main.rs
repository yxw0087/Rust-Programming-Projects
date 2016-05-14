use root::proc_lang_scanner::*;
use root::proc_lang_parser::*;
use root::proc_lang_env::*;
use root::proc_lang_exp::*;

fn expval_num(num:&IntBoolProc) -> Option<i32> {
    match num.clone() {
        IntBoolProc::Integer(i) => Some(i),
        _                   => {println!("Error: expecting integer."); std::process::exit(1)},
    }
}

fn expval_bool(boolean:&IntBoolProc) -> Option<bool> {
    match boolean.clone() {
        IntBoolProc::Boolean(b) => Some(b),
        _                   => {println!("Error: expecting boolean."); std::process::exit(1)},
    }
}

fn value_of(exp:&ProcLangExp, env:&EnvExp) -> Option<IntBoolProc> {

    match exp.clone() {
    
        ProcLangExp::ConstExp(i) => Some(IntBoolProc::Integer(i)),
        
        ProcLangExp::Boolean(b) => Some(IntBoolProc::Boolean(b)),
        
        ProcLangExp::VarExp(s) => Some(env.apply_env(&s).unwrap()), 
        //Will cause compiler to panic if s is not found in the env
        
        ProcLangExp::DiffExp(exp1, exp2) => 
        {
            let val1 = value_of(&exp1, &env);
            let val2 = value_of(&exp2, &env);
            let num1 = expval_num(&val1.unwrap());
            let num2 = expval_num(&val2.unwrap());
            let result = num1.unwrap() - num2.unwrap();
            Some(IntBoolProc::Integer(result))
        },
        
        ProcLangExp::PlusExp(exp1, exp2) => 
        {
            let val1 = value_of(&exp1, &env);
            let val2 = value_of(&exp2, &env);
            let num1 = expval_num(&val1.unwrap());
            let num2 = expval_num(&val2.unwrap());
            let result = num1.unwrap() + num2.unwrap();
            Some(IntBoolProc::Integer(result))
        },
        
        ProcLangExp::IsZeroExp(exp) =>
        {
            let val = value_of(&exp, &env);
            let num = expval_num(&val.unwrap());
            //Will cause compiler to panic if val is not constant
            if num.unwrap() == 0 {Some(IntBoolProc::Boolean(true))}
            else {Some(IntBoolProc::Boolean(false))}
        },
        
        ProcLangExp::IfExp(exp1, exp2, exp3) =>
        {
            let val = value_of(&exp1, &env);
            if expval_bool(&val.unwrap()).unwrap() {value_of(&exp2, &env)}
            else {value_of(&exp3, &env)}
        },
        
        ProcLangExp::LetExp(s, exp1, exp2) =>
        {
            let val = value_of(&exp1, &env);
            value_of(&exp2, &env.extend_env(&s, &val.unwrap()))
        },
        
        ProcLangExp::LetRecExp(s1, s2, exp1, exp2) =>
        {
            value_of(&exp2, &env.extend_env_rec(&s1, &s2, &exp1))
        },
        
        ProcLangExp::ProcExp(s, exp) =>
        {
            value_of_proc(&s, &exp, &env)
        },
        
        ProcLangExp::CallExp(exp1, exp2) =>
        {
            value_of_call(&exp1, &exp2, &env)
        },
    }
}

fn value_of_proc(v: &String, body: &ProcLangExp, env: &EnvExp) -> Option<IntBoolProc> {
    Some(IntBoolProc::Procedure(v.clone(), Rc::new(body.clone()), Rc::new(env.clone())))
}

fn value_of_call(rator: &ProcLangExp, rand: &ProcLangExp, env: &EnvExp) -> Option<IntBoolProc> {
    let opt_closure =
                match rator.clone() {
                    ProcLangExp::VarExp(s)     => env.apply_env(&s),
                    ProcLangExp::ProcExp(v, e) => Some(IntBoolProc::Procedure(v.clone(),
                                                  Rc::new((*e).clone()),
                                                  Rc::new(env.clone()))),
                    _                     => None,
                };
    let opt_evald_rand = value_of(&rand, &env);
    if opt_closure.is_none() || opt_evald_rand.is_none() { None }
    else { apply_procedure(&(opt_closure.unwrap()), &(opt_evald_rand.unwrap())) }
}

fn apply_procedure(closure: &IntBoolProc, rand: &IntBoolProc) -> Option<IntBoolProc> {
    match closure.clone() {
        IntBoolProc::Procedure(v, b, env) =>
            value_of(&(*b).clone(), &(env.clone()).extend_env(&(v.to_string()), rand)),
        _   => None,
    }
}

#[allow(dead_code)]
fn main() {

    println!("##### Start testing the evaluation of proc_lang statement, expected output: Some(Integer(-100)) #####");
    let env = EnvExp::new_env();
    let str = "let x = 200
               in let f = proc (z) -(z, x)
                  in let x = 100
                     in let g = proc (z) -(z, x)
                        in -((f 1), (g 1))";
    let result: Result<Vec<Token>, LexErr> = tokenize(str);
    let tokens = match result {
                    Ok(v)  => v,
                    Err(_e) => return,
                    };
    let parse_tree = parse(&tokens);
    println!("AST of the proc_lang statement: {:#?}", &parse_tree);
    let value = value_of(&parse_tree.unwrap(), &env);
    println!("Value: {:?}", value);
    
    println!("\n##### Start testing the evaluation of letrec_lang statement, expected output: Some(Integer(12)) #####");
    let env = EnvExp::new_env();
    let str = "letrec double(x) = if iszero(x)
                                  then 0
                                  else +((double -(x, 1)), 2)
               in (double 6)";
    let result: Result<Vec<Token>, LexErr> = tokenize(str);
    let tokens = match result {
                    Ok(v)  => v,
                    Err(_e) => return,
                    };
    let parse_tree = parse(&tokens);
    println!("AST of the letrec_lang statement: {:#?}", &parse_tree);
    let value = value_of(&parse_tree.unwrap(), &env);
    println!("Value: {:?}", value);
}