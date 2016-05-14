use explicit_ref_env::*;

#[derive(Debug,Clone)]
pub struct ExplStor {
    contents: Vec<DenVal>,
}

impl<'a> ExplStor {
    pub fn new_stor() -> Self {
        let st: ExplStor = ExplStor { contents: Vec::new()};
        st
    }
    pub fn new_ref(&mut self, v: &'a DenVal) -> usize {
        let next_ref: usize = self.contents.len();
        self.contents.push(v.clone());
        next_ref
    }
    pub fn deref(&self, r: usize) -> DenVal {
        (self.contents[r]).clone()
    }
    pub fn setref(&mut self, r: usize, v: &DenVal) {
        if (r+1) > self.contents.len() {
            panic!("setref out of range: vector len = {}, r={}", self.contents.len(), r);
        } else {
        self.contents[r] = v.clone();
        }
    }
    pub fn len(&self) -> usize {
        self.contents.len()
    }
}