use std::vec::Vec;
use std::fmt;

#[derive(Debug,Clone)]
struct iList {
    head: Vec<i32>,
}

#[derive(Debug)]
struct Iter {
    next: iList,
}

impl iList {
    fn new() -> Self {
        iList {head: Vec::new()}
    }
    
    fn cons(&mut self, val: i32) -> Self {
        self.head.insert(0, val);
        iList {head: self.head.clone()}
    }
    
    fn car(&self) -> Option<i32> {
        let mut temp = self.head.clone();
        temp.reverse();
        temp.pop()
    }
    
    fn cdr(&self) -> Self {
        let mut temp = self.head.clone(); 
        temp.reverse();
        temp.pop();
        temp.reverse();
        iList {head: temp}
    }
    
    fn null(&self) -> bool {
        self.head.is_empty()
    }
	
	fn iter(&self) -> Iter {
		Iter {next: self.clone()}	
	}
}

impl fmt::Display for iList {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let items = self.iter().collect::<Vec<i32>>();
		let num_of_items = items.len();
		let space = format!(" ");
		let mut s = format!(" ");
		for num in items {
			let tmp_str = num.to_string();
			s.push_str(&tmp_str);
			s.push_str(&space);
		}
		write!(f, "({})", s)
	}
}

impl Iterator for Iter {
	type Item = i32;
	fn next(&mut self) -> Option<Self::Item> {
		let x = self.next.clone();
		self.next = self.next.cdr();
		if x.null() {
			None
		} else {
			Some(x.car().unwrap())
		}
	}
}

fn nth_element(lst: &iList, n: u32) -> i32 {
    if lst.null() {
        -1
    } else {
        if n == 0 {lst.car().unwrap()}
        else {nth_element(&(lst.cdr()), n-1)}
    }
}

fn append(l1: &iList, l2: &iList) -> iList {
    if l1.null() {
        l2.clone()
    } else {
        append(&l1.cdr(), l2).cons(l1.car().unwrap())
    }
}

fn main() {
    println!("###Begin testing the interface functions new, cons, car, cdr and null###");
    let mut test: iList = iList::new();
    println!("\n###Testing new, expected output: ( )###");
    println!("List = {}", test);
    println!("\n###Testing null part 1, expected output: Yes###");
    if test.null() {println!("Yes")}
    else {println!("No")}
    test = test.cons(1);
    println!("\n###Testing cons, added an element to list.###");
    println!("\n###Testing null part 2, expected output: No###");
    if test.null() {println!("Yes")}
    else {println!("No")}
    test = test.cons(2);
    test = test.cons(3);
    test = test.cons(4);
    println!("\n###Added 3 more elements to list.###");
    println!("List = {}", test);
    println!("\n###Testing cdr, expected output: ( 3 2 1 )###");
	let rest = test.cdr();
	println!("Rest = {}", rest);
	let first = test.car();
	println!("\n###Testing car, expected output: First = 4###");
	match first {
	    Some(x) => println!("First = {}", x),
	    None    => println!("Empty list."),
	}
	println!("\n###Testing car with an empty list, expected output: Empty list.###");
	let mut test2: iList = iList::new();
	let first = test2.car();
	match first {
	    Some(x) => println!("First = {}", x),
	    None    => println!("Empty list."),
	}
	println!("\n###Printing the list at the end to test that no internal changes have been made.###");
	println!("List = {}", test);
	println!("\n###End of testing the interface functions.###");
	
	println!("\n###Begin testing the application functions nth_element and append###");
	println!("\n###Testing nth_element.###");
	let testnth = nth_element(&mut test,2);
	println!("Element of = {} at index = {} is {}", test, 2, testnth);
	let testnth = nth_element(&mut test2,1);
	println!("Element of = {} at index = {} is {}", test2, 1, testnth);
	println!("\n###Testing append.###");
	let mut test2: iList = iList::new().cons(0);
	let testappend = append(&mut test, &test2);
	println!("Appending {} to {} is {}", test2, test, testappend);
	println!("\n###End of testing the application functions.###");
}