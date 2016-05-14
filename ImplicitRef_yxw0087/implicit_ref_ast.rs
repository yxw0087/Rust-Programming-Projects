use std::rc::Rc;
use std::fmt;
use implicit_ref_ast::implicAST::*;

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