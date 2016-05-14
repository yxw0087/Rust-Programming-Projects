#[derive(Debug,Clone)]
pub enum IntBool {
    Integer(i32),
    Boolean(bool),
}

impl IntBool {
    pub fn new_int(i: i32) -> Self {
        IntBool::Integer(i.clone())
    }
    pub fn new_bool(b: bool) -> Self {
        IntBool::Boolean(b.clone())
    }
}