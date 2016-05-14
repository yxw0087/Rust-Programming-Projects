use std::rc::Rc;
use std::fmt;
use explicit_ref_ast::ExplicAST::*;

#[derive(Debug,Clone)]
pub enum ExplicAST { // a node in an AST
    ConstExp(i32),
    Boolean(bool),
    PlusExp(Rc<ExplicAST>, Rc<ExplicAST>),
    DiffExp(Rc<ExplicAST>, Rc<ExplicAST>),
    IsZeroExp(Rc<ExplicAST>),
    IfExp(Rc<ExplicAST>, Rc<ExplicAST>, Rc<ExplicAST>),
    VarExp(String),
    LetExp(String, Rc<ExplicAST>, Rc<ExplicAST>),
    ProcExp(Vec<String>, Rc<ExplicAST>),
    //      bvars        body
    CallExp(Rc<ExplicAST>, Vec<Rc<ExplicAST>>),
    //      pname           args
    LetRecExp(Vec<String>, Vec<Vec<String>>, Vec<Rc<ExplicAST>>, Rc<ExplicAST>),
    //        pnames       bvars             p_bodies             in_body
    NewrefExp(Rc<ExplicAST>),
    //        exp1
    DerefExp(Rc<ExplicAST>),
    //        exp1
    SetrefExp(Rc<ExplicAST>, Rc<ExplicAST>),
    //        exp1            exp2
    BeginEndExp(Vec<Rc<ExplicAST>>),
    //        exps
}

impl ExplicAST {
    pub fn new_const_exp(num: i32) -> Self {
        ConstExp(num)
    }
    pub fn new_boolean(tv: bool) -> Self {
        Boolean(tv)
    }
    pub fn new_plus_exp(arg1: &ExplicAST, arg2: &ExplicAST) -> Self {
        PlusExp(Rc::new(arg1.clone()), Rc::new(arg2.clone()))
    }
    pub fn new_diff_exp(arg1: &ExplicAST, arg2: &ExplicAST) -> Self {
        DiffExp(Rc::new(arg1.clone()), Rc::new(arg2.clone()))
    }
    pub fn new_iszero(arg: &ExplicAST) -> Self {
        IsZeroExp(Rc::new(arg.clone()))
    }
    pub fn new_if_exp(arg1: &ExplicAST, arg2: &ExplicAST, arg3: &ExplicAST) -> Self {
        IfExp(Rc::new(arg1.clone()), Rc::new(arg2.clone()), Rc::new(arg3.clone()))
    }
    pub fn new_var_exp(s: &String) -> Self {
        VarExp(s.clone())
    }
    pub fn new_let_exp(s: &String, arg1: &ExplicAST, arg2: &ExplicAST) -> Self {
        LetExp(s.clone(), Rc::new(arg1.clone()), Rc::new(arg2.clone()))
    }
    pub fn new_proc_exp(vars: &Vec<String>, 
                        body: &ExplicAST) -> Self {
        ProcExp(vars.clone(), Rc::new(body.clone()))
    }
    pub fn new_call_exp(rator: &ExplicAST, 
                        rands: &Vec<ExplicAST>) -> Self {
        let mut args_v: Vec<Rc<ExplicAST>> =  Vec::new();
        for i in 0..rands.len() {
            args_v.push(Rc::new(rands[i].clone()));
        }
        CallExp(Rc::new(rator.clone()), args_v)
    }
    pub fn new_letrec_exp(pnames  : &Vec<String>, 
                          b_vars  : &Vec<Vec<String>>, 
                          p_bodies: &Vec<ExplicAST>,
                          letrec_body: &ExplicAST) -> Self {
        // cvrt from Vec<ExplicAST> to Vec<Rc<ExplicAST>>
        let mut bodies: Vec<Rc<ExplicAST>> = Vec::new();
        for body in p_bodies {
            bodies.push(Rc::new(body.clone()));
        };
        LetRecExp(pnames.clone(),
                                 b_vars.clone(),
                                 bodies,
                                 Rc::new(letrec_body.clone())
                                )
    }
    pub fn new_newref_exp(arg: &ExplicAST) -> Self {
        NewrefExp(Rc::new(arg.clone()))
    }
    pub fn new_deref_exp(arg: &ExplicAST) -> Self {
        DerefExp(Rc::new(arg.clone()))
    }
    pub fn new_setref_exp(arg1: &ExplicAST, arg2: &ExplicAST) -> Self {
        SetrefExp(Rc::new(arg1.clone()), Rc::new(arg2.clone()))
    }
    pub fn new_beginend_exp(exps: &Vec<ExplicAST>) -> Self {
        let mut args_v: Vec<Rc<ExplicAST>> =  Vec::new();
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
            NewrefExp(e)        => {let mut temp = "newref(".to_string();
                                    temp.push_str(&(e.to_string()));
                                    temp.push_str(&(")".to_string()));
                                    temp}
            DerefExp(e)         => {let mut temp = "deref(".to_string();
                                    temp.push_str(&(e.to_string()));
                                    temp.push_str(&(")".to_string()));
                                    temp}
            SetrefExp(e1, e2)   => {let mut temp = "setref(".to_string();
                                    temp.push_str(&(e1.to_string()));
                                    temp.push_str(&(", ".to_string()));
                                    temp.push_str(&(e2.to_string()));
                                    temp.push_str(&(")".to_string()));
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

impl fmt::Display for ExplicAST {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = format!("");
        let s1 = self.to_string();
        s.push_str(&s1);
        write!(f, "{}", s)
    }
}