use proc_lang_exp::*;
use std::rc::Rc;
use std::fmt;

#[derive(Debug,Clone)]
pub enum IntBoolProc {
    Integer(i32),
    Boolean(bool),
    Procedure(String, Rc<ProcLangExp>, Rc<EnvExp>),
}

#[derive(Debug,Clone)]
pub enum EnvExp {
    EmptyEnv,
    ExtendEnv(Identifier, IntBoolProc, Rc<EnvExp>),
    ExtendEnvRec(Identifier, Identifier, Rc<ProcLangExp>, Rc<EnvExp>),
}

impl EnvExp {
    pub fn new_env() -> Self {
        EnvExp::EmptyEnv
    }
    pub fn extend_env(&self, s:&Identifier, val:&IntBoolProc) -> Self {
        EnvExp::ExtendEnv(s.clone(), val.clone(), Rc::new(self.clone()))
    }
    pub fn extend_env_rec(&self, s1:&Identifier, s2:&Identifier, body:&ProcLangExp) -> Self {
        EnvExp::ExtendEnvRec(s1.clone(), s2.clone(), Rc::new(body.clone()), Rc::new(self.clone()))
    }
    pub fn apply_env(&self, s:&Identifier) -> Option<IntBoolProc> {
        match self.clone() {
            EnvExp::ExtendEnv(var, val, env) => 
                                       if s[..] == var[..] {
                                        Some(val.clone())
                                       } else {
                                        env.apply_env(s)
                                        },
            EnvExp::ExtendEnvRec(name, var, body, env) => 
                                       if s[..] == name[..] {
                                        Some(IntBoolProc::Procedure(var, 
                                             Rc::new((*body).clone()), 
                                             Rc::new((*self).clone())))
                                       } else {
                                        self.apply_env(s)
                                        },
            EnvExp::EmptyEnv => None,
        }
    }
    pub fn is_null_env(&self) -> bool {
        match self.clone() {
            EnvExp::EmptyEnv  => true,
            _                 => false,
        }
    }
}