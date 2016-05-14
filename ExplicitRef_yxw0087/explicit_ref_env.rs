use std::rc::Rc;
use std::fmt;
use explicit_ref_ast::*;
use explicit_ref_env::ExplicENV::*;

#[derive(Debug,Clone)]
pub enum ExplicENV { // an environment has three variants
    EmptyEnv,
    ExtendEnv(String, DenVal, Rc<ExplicENV>),
    ExtendEnvRec(Vec<String>,           // pname
                 Vec<Vec<String>>,      // bvars
                 Vec<Rc<ExplicAST>>,   
                 Rc<ExplicENV>),
}

impl ExplicENV {
    // Constructors for the three variants
    pub fn new_env() -> Self {
        EmptyEnv
    }
    pub fn extend_env(&self, s:&String, val: &DenVal) -> Self {
        ExtendEnv(s.clone(), val.clone(), Rc::new(self.clone()))
    }
    pub fn extend_env_rec(&self, 
                          pnames: &Vec<String>, 
                          b_vars: &Vec<Vec<String>>, 
                          bodies: &Vec<Rc<ExplicAST>>) -> Self {
        ExtendEnvRec(pnames.clone(),        // p_names
                     b_vars.clone(),        // b_vars
                     bodies.clone(),        // bodies
                     Rc::new(self.clone())  // saved env
                    )
    }
    pub fn apply_env(&self, s:&String) -> Option<DenVal> {
        match self.clone() {
            EmptyEnv => None,
            ExtendEnv(var, val, env) => 
                                       if s[..] == var[..] {
                                        Some(val)
                                       } else {
                                        env.apply_env(s)
                                        },
            ExtendEnvRec(pnames, bvars, bodies, saved_env) 
                         => {
                         //println!("\nAPPLY_ENV entered. ***");
                         /*println!("    matched ExtendEnvRec. ***");
                         println!("    s_var  : {}", s);
                         println!("    pnames : {:?}", pnames);
                         println!("    bvars  : {:?}",  bvars);
                         println!("    bodies :");
                         for body in (bodies.clone()) {
                            println!("             {:?}", body);
                            }*/
                         //println!("  saved_env: {:?}", saved_env);
                         let mut opt_proc: Option<DenVal> = None;
                         // search pnames
                         // if found return correct vars and body
                         //println!("len is: {}", bodies.len());
                         for i in 0..pnames.len() {
                            let pname: String = pnames[i].clone();
                            if s[..] == pname[..] {
                                opt_proc =
                                Some(DenVal::Procedure(
                                                        bvars[i].clone(),
                                                        bodies[i].clone(),
                                                        Rc::new((*self).clone())
                                                        )); 
                            }};
                         if opt_proc.is_none() {
                            opt_proc = saved_env.apply_env(s)
                         };
                         //println!("proc is: {:?}", opt_proc.clone());
                         /*match opt_proc.clone() {
                            Some(v) => println!("   proc:\n{:?}", v),
                            None    => println!("No val in env!!!"),
                         }*/
                         //println!("APPLY_ENV returned. ***");
                         opt_proc  // return Option<DenVal>
                        },
        }}
    pub fn is_null_env(&self) -> bool {
        match self.clone() {
            EmptyEnv  => true,
            _         => false,
        }}
    pub fn to_string(&self) -> String {
        match self.clone() {
            EmptyEnv => "[]".to_string(),
            ExtendEnv(var,val,env) => {let mut temp = "[ ".to_string();
                                       temp.push_str(&(var.to_string()));
                                       temp.push_str(&("=".to_string()));
                                       temp.push_str(&(val.to_string()));
                                       temp.push_str(&(" ".to_string()));
                                       temp.push_str(&(env.to_string()));
                                       temp.push_str(&(" ]".to_string()));
                                       temp},
            ExtendEnvRec(p_names, b_vars_lst, p_bodies, saved_env)
                         =>
                         {let mut temp = "[".to_string();
                         for i in 0..p_names.len() {
                            if i!=0 {temp.push_str(&("; ".to_string()))};
                            temp.push_str(&(p_names[i].to_string()));
                            temp.push_str(&("=(".to_string()));
                            let bvars = b_vars_lst[i].clone();
                            for i in 0..bvars.len() {
                                temp.push_str(&(bvars[i].to_string()));
                                if (i+1) < bvars.len() {
                                    temp.push_str(&(", ".to_string()));
                                }
                            }
                            temp.push_str(&(") ".to_string()));
                            temp.push_str(&(p_bodies[i].to_string()));
                         }
                         temp.push_str(&("] ".to_string()));
                         temp.push_str(&(saved_env.to_string()));
                         temp}
        }}}

impl fmt::Display for ExplicENV {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = format!("");
        let s1 = self.to_string();
        s.push_str(&s1);
        write!(f, "{}", s)
    }}

#[derive(Debug,Clone)]
pub enum DenVal {
    Integer(i32),
    Boolean(bool),
    Ref(Rc<DenVal>),
    Procedure(Vec<String>, Rc<ExplicAST>, Rc<ExplicENV>),
    //        bvars        body            env
}

/*impl DenVal {
    pub fn to_string(&self) -> String {
        match self.clone() {
            DenVal::Integer(i) => i.to_string(),
            DenVal::Boolean(b) => b.to_string(),
            DenVal::Procedure(vars, exp, env) => 
                              {let mut temp = "proc (".to_string();
                               for i in 0..vars.len() {
                                    temp.push_str(&(vars[i]));
                                    if (i+1) < vars.len() {
                                        temp.push_str(&(", ".to_string()));}
                               }
                               temp.push_str(&(") ".to_string()));
                               temp.push_str(&(exp.to_string()));
                               temp.push_str(&("\n".to_string()));
                               temp.push_str(&(env.to_string()));
                               temp
                               }}}}*/

impl fmt::Display for DenVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = format!("");
        let s1 = self.to_string();
        s.push_str(&s1);
        write!(f, "{}", s)
    }}

#[cfg(test)]
mod test {
    use super::ExplicENV;
    use super::DenVal;
    
    #[test]
    fn basic_tests() {
        let null_env = ExplicENV::new_env();
        assert!(null_env.is_null_env());
        
        let env2 = null_env.extend_env(&("var1".to_string()), 
                                    &DenVal::Integer(25));
        assert!(!(env2.is_null_env()));
    }
}