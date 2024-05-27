use anyhow::anyhow;

use crate::ast::{Expr, Num, Integer, LispResult};

pub fn parse_num(input: &str) -> LispResult<(Expr, usize)> {
    let next_whitespace_or_end_of_string = input
        .chars()
        .position(|c| !(c == '.' || c.is_numeric() || c == '-'))
        .unwrap_or(input.len());

    //  slices the input string from the start to the position found in the previous step. 
    let input = &input[0..next_whitespace_or_end_of_string];

    if let Ok(res) = input.parse::<Integer>() {
        return Ok((Expr::num(res), next_whitespace_or_end_of_string));
    }

    if let Ok(res) = input.parse::<Num>() {
        return Ok((Expr::num(res), next_whitespace_or_end_of_string));
    }
    Err(anyhow!("Cannot convert: \"{}\" into an int", input))
}

fn parse_symbol(input: &str) -> LispResult<(Expr, usize)> {
    if input.is_empty() || input.chars().next().unwrap().is_numeric() {
        return Err(anyhow!("Invalid symbol: {}", input));
    }

    let output_str: String = input.chars().take_while(|&c| is_symbol_char(c)).collect();
    let end_index = output_str.len();

    let res = match output_str.as_str() {
        "true" => Expr::Bool(true),
        "false" => Expr::Bool(false),
        "nil" => Expr::Nil,
        _ => Expr::Symbol(output_str.into()),
    };
    Ok((res, end_index))
}

fn is_symbol_char(c: char) -> bool {
    match c {
        '(' | ')' | '"' | '\'' | ';' | ' ' => false,
        sym => !sym.is_whitespace(),
    }
}


fn parse_expr(input: &str) -> LispResult<(Expr, usize)> {
    todo!()
}