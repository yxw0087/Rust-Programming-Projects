use std::rc::Rc;
use std::fmt;

pub type Identifier = String;

#[derive(Debug,Clone)]
pub enum ProcLangExp {
    ConstExp(i32),
    Boolean(bool),
    DiffExp(Rc<ProcLangExp>, Rc<ProcLangExp>),
    PlusExp(Rc<ProcLangExp>, Rc<ProcLangExp>),
    IsZeroExp(Rc<ProcLangExp>),
    IfExp(Rc<ProcLangExp>, Rc<ProcLangExp>, Rc<ProcLangExp>),
    VarExp(Identifier),
    LetExp(Identifier, Rc<ProcLangExp>, Rc<ProcLangExp>),
    ProcExp(Identifier, Rc<ProcLangExp>),
    CallExp(Rc<ProcLangExp>, Rc<ProcLangExp>),
    LetRecExp(Identifier, Identifier, Rc<ProcLangExp>, Rc<ProcLangExp>),
}

impl ProcLangExp {
    pub fn new_const(num: &i32) -> Self {
        ProcLangExp::ConstExp(num.clone())
    }
    pub fn new_boolean(boolean: &bool) -> Self {
        ProcLangExp::Boolean(boolean.clone())
    }
    pub fn new_diff(Exp1: &ProcLangExp, Exp2: &ProcLangExp) -> Self {
        ProcLangExp::DiffExp(Rc::new(Exp1.clone()), Rc::new(Exp2.clone()))
    }
    pub fn new_plus(Exp1: &ProcLangExp, Exp2: &ProcLangExp) -> Self {
        ProcLangExp::PlusExp(Rc::new(Exp1.clone()), Rc::new(Exp2.clone()))
    }
    pub fn new_iszero(Exp: &ProcLangExp) -> Self {
        ProcLangExp::IsZeroExp(Rc::new(Exp.clone()))
    }
    pub fn new_if(ifexp: &ProcLangExp, thenexp: &ProcLangExp, elseexp: &ProcLangExp) -> Self {
        ProcLangExp::IfExp(Rc::new(ifexp.clone()), Rc::new(thenexp.clone()), Rc::new(elseexp.clone()))
    }
    pub fn new_var(s: &Identifier) -> Self {
        ProcLangExp::VarExp(s.clone())
    }
    pub fn new_let(s: &Identifier, assignExp: &ProcLangExp, inExp: &ProcLangExp) -> Self {
        ProcLangExp::LetExp(s.clone(), Rc::new(assignExp.clone()), Rc::new(inExp.clone()))
    }
    pub fn new_proc(s: &Identifier, Exp: &ProcLangExp) -> Self {
        ProcLangExp::ProcExp(s.clone(), Rc::new(Exp.clone()))
    }
    pub fn new_call(Exp1: &ProcLangExp, Exp2: &ProcLangExp) -> Self {
        ProcLangExp::CallExp(Rc::new(Exp1.clone()), Rc::new(Exp2.clone()))
    }
    pub fn new_letrec(s1: &Identifier, s2: &Identifier, Exp1: &ProcLangExp, Exp2: &ProcLangExp) -> Self {
        ProcLangExp::LetRecExp(s1.clone(), s2.clone(), Rc::new(Exp1.clone()), Rc::new(Exp2.clone()))
    }
}