use std::fmt::write;
use std::sync::Arc;

use x9::ffi::{ExprHelper, ForeignData, IntoX9Function, X9Interpreter};
use x9::ast::Expr;

// This example shows how to use x9's FFI interface.
// The usual steps are:
// 1. Having a compatible error type
// 2. Implementing ForeignData to map from your datatype to x9's Expr and back
// 3. Adding functions to the interpreter in terms of your datatype
// 4. Setting up the interpreter and running programs

// Step 1: Setup an error type. We need to be able to return
//         errors when a conversion fails.
#[derive(Debug)]
struct MyError(String);

impl std::fmt::Display for MyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for MyError {}

// Step 2: Define the data we want x9 to understand. We're going to map
//         from MyData to x9's Expr type and vice-versa.
#[derive(Debug, Clone, PartialEq)]
enum MyData {
    Int(u64),
    String(String),
}

// Step 3: Implement ForeignData for our data type.
impl ForeignData for MyData {
    fn to_x9(&self) -> Result<Expr, Box<dyn std::error::Error + Send>> {
        let res = match self {
            MyData::Int(i) => Expr::Num((*i).into()),
            MyData::String(s) => Expr::string(s.to_string()),
        };
        Ok(res)
    }

    fn from_x9(expr: &Expr) -> Result<Self, Box<dyn std::error::Error + Send>> {
        let res = match expr {
            Expr::Num(n) => MyData::Int(n.to_u64()?),
            Expr::Integer(n) => MyData::Int(*n as u64),
            Expr::String(s) => MyData::String(s.to_string()),
            bad_type => {
                return Err(Box::new(MyError(format!(
                    "Cannot convert {} to MyData!",
                    bad_type
                ))))
            }
        };
        Ok(res)
    }
}

// Step 4: We're going to add our own function to the interpreter
fn setup_interpreter(interpreter: &X9Interpreter) {
    let mydata_sum = |args: Vec<MyData>| {
        let res = match (&args[0], &args[1]) {
            (MyData::Int(l), MyData::Int(r)) => MyData::Int(l + r),
            (MyData::Int(l), MyData::String(r)) => MyData::String(format!("{}{}", l, r)),
            (MyData::String(l), MyData::Int(r)) => MyData::String(format!("{}{}", l, r)),
            (MyData::String(l), MyData::String(r)) => MyData::String(format!("{}{}", l, r)),
        };
        Ok(res) // we need to return a result
    };

    interpreter.add_function_ptr("mydata-sum", 2, Arc::new(mydata_sum));
}

fn main() {
    // Make a new interpreter
    let interpreter = X9Interpreter::new();

    // Add our function to it
    setup_interpreter(&interpreter)
}


