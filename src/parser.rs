use anyhow::anyhow;

use crate::ast::{Expr, Integer, LispResult, Num};

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

fn parse_string(input: &str) -> LispResult<(Expr, usize)> {
    // TODO: Clean this up
    if input.len() < 2 {
        return Err(anyhow!("Could not parse the string: {}", input));
    }
    if !input.starts_with('"') {
        return Err(anyhow!(
            "Parse string called on something that does not start with a quote: {}",
            input
        ));
    }

    let mut curr_pos = 1; 
    let mut output_str = String::new();
    let mut chars_iterator = input.chars().skip(1).peekable();

    while let Some(curr_char) = chars_iterator.next() {
        curr_pos += 1;
        match curr_char {
            '\\' => {
                if let Some(&next_char) = chars_iterator.peek() {
                    curr_pos += 1;
                    chars_iterator.next();
                    match next_char {
                        '\\' => output_str.push('\\'),
                        'n' => output_str.push('\n'),
                        'r' => output_str.push('\r'),
                        '"' => output_str.push('"'),
                        _ => return Err(anyhow!("Invalid escape sequence: \\{}", next_char)),
                    }
                } else {
                    return Err(anyhow!("Incomplete escape sequence at end of input: {}", input));
                }
            },
            '"' => break,
            _ => output_str.push(curr_char),
        }
    }

    if curr_pos >= input.len() || input.chars().nth(curr_pos - 1) != Some('"') {
        return Err(anyhow!("String not terminated properly: {}", input));
    }

    Ok((Expr::string(output_str), curr_pos))
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
