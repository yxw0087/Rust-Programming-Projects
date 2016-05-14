use implicit_ref_scanner::*;
use implicit_ref_parser::*;
use implicit_ref_ast::*;
use implicit_ref_exp::implicAST::*;
use implicit_ref_env::*;

use std::fmt;
use std::rc::Rc;

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