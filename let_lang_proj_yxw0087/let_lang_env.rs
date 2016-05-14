use let_lang_exp::*;
use int_bool::*;
use std::rc::Rc;
use std::fmt;

#[derive(Debug,Clone)]
pub enum EnvExp {
    EmptyEnv,
    ExtendEnv(Identifier, IntBool, Rc<EnvExp>),
}

impl EnvExp {
    pub fn new_env() -> Self {
        EnvExp::EmptyEnv
    }
    pub fn extend_env(&self, s:&Identifier, val:&IntBool) -> Self {
        EnvExp::ExtendEnv(s.clone(), val.clone(), Rc::new(self.clone()))
    }
    pub fn apply_env(&self, s:&Identifier) -> Option<IntBool> {
        match self.clone() {
            EnvExp::ExtendEnv(var, val, env) => 
                                       if s[..] == var[..] {
                                        Some(val.clone())
                                       } else {
                                        env.apply_env(s)
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
    /*pub fn to_string(&self) -> String {
        match self.clone() {
            EnvExp::EmptyEnv => "[]".to_string(),
            EnvExp::ExtendEnv(var,val,env) => {let mut temp = "[".to_string();
                                                temp.push_str(&(var.to_string()));
                                                temp.push_str(&(", ".to_string()));
                                                temp.push_str(&(val.to_string()));
                                                temp.push_str(&(" ".to_string()));
                                                temp.push_str(&(env.to_string()));
                                                temp.push_str(&("]".to_string()));
                                                temp},
        }
    }*/
}

impl fmt::Display for EnvExp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = format!("");
        let s1 = self.to_string();
        s.push_str(&s1);
        write!(f, "{}", s)
    }
}