use std::rc::Rc;
use std::fmt;

pub type Identifier = String;

#[derive(Debug,Clone)]
pub enum LetExp {
    ConstExp(i32),
    Boolean(bool),
    DiffExp(Rc<LetExp>, Rc<LetExp>),
    IsZeroExp(Rc<LetExp>),
    IfExp(Rc<LetExp>, Rc<LetExp>, Rc<LetExp>),
    VarExp(Identifier),
    LetExp(Identifier, Rc<LetExp>, Rc<LetExp>),
}

impl LetExp {
    pub fn new_const(num: &i32) -> Self {
        LetExp::ConstExp(num.clone())
    }
    pub fn new_boolean(boolean: &bool) -> Self {
        LetExp::Boolean(boolean.clone())
    }
    pub fn new_diff(Exp1: &LetExp, Exp2: &LetExp) -> Self {
        LetExp::DiffExp(Rc::new(Exp1.clone()), Rc::new(Exp2.clone()))
    }
    pub fn new_iszero(Exp: &LetExp) -> Self {
        LetExp::IsZeroExp(Rc::new(Exp.clone()))
    }
    pub fn new_if(ifexp: &LetExp, thenexp: &LetExp, elseexp: &LetExp) -> Self {
        LetExp::IfExp(Rc::new(ifexp.clone()), Rc::new(thenexp.clone()), Rc::new(elseexp.clone()))
    }
    pub fn new_var(s: &Identifier) -> Self {
        LetExp::VarExp(s.clone())
    }
    pub fn new_let(s: &Identifier, assignExp: &LetExp, inExp: &LetExp) -> Self {
        LetExp::LetExp(s.clone(), Rc::new(assignExp.clone()), Rc::new(inExp.clone()))
    }
}