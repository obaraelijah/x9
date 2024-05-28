use anyhow::anyhow;
use im::{vector, Vector};

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
                    return Err(anyhow!(
                        "Incomplete escape sequence at end of input: {}",
                        input
                    ));
                }
            }
            '"' => break,
            _ => output_str.push(curr_char),
        }
    }

    if curr_pos >= input.len() || input.chars().nth(curr_pos - 1) != Some('"') {
        return Err(anyhow!("String not terminated properly: {}", input));
    }

    Ok((Expr::string(output_str), curr_pos))
}

//  Lisp symbolic expressions
fn parse_sexp(input: &str) -> LispResult<(Expr, usize)> {
    if input.is_empty() {
        return Err(anyhow!("Attempted to parse sexp on empty string!"));
    }
    let mut char_iterator = input.chars().peekable();
    if char_iterator.next() != Some('(') {
        return Err(anyhow!(
            "Attempted to parse sexp not starting with a brace! {}",
            input
        ));
    }
    let mut curr_pos = 1;
    let mut contents = Vector::new();
    loop {
        let curr_input_slice = &input[curr_pos..];
        let next_pos = next_non_whitespace_and_comment_pos(curr_input_slice);
        for _ in 0..next_pos {
            char_iterator.next();
            curr_pos += 1;
        }
        match char_iterator.peek() {
            None => return Err(anyhow!("Unexpected end of sexp! {}", input)),
            Some(curr_char) => {
                if curr_char == &')' {
                    curr_pos += 1;
                    break;
                }
                let (next_item, new_pos) = parse_expr(&input[curr_pos..])?;
                contents.push_back(next_item);
                for _ in 0..new_pos {
                    char_iterator.next();
                }
                curr_pos += new_pos;
            }
        }
    }
    Ok((Expr::List(contents), curr_pos))
}

fn parse_dict(input: &str) -> LispResult<(Expr, usize)> {
    if input.is_empty() {
        return Err(anyhow!("Attempted to parse a dict on an empty string!"));
    }

    if !input.starts_with('{') {
        return Err(anyhow!(
            "Attempted to parse a dict from a string not starting with a curly brace! {}",
            input
        ));
    }

    let mut curr_pos = 1;
}

fn next_non_whitespace_and_comment_pos(input: &str) -> usize {
    let mut output_pos = 0;
    loop {
        let input = &input[output_pos..];
        if input.is_empty() {
            break;
        }
        let leading_char = input.chars().next().unwrap();
        if leading_char == ';' {
            output_pos += input
                .chars()
                .position(|c| c == '\n')
                .unwrap_or(input.len() - 1)
                + 1;
        } else if leading_char.is_whitespace() {
            output_pos += input.chars().take_while(|c| c.is_whitespace()).count();
        } else {
            break;
        }
    }
    output_pos
}

fn is_symbol_char(c: char) -> bool {
    match c {
        '(' | ')' | '"' | '\'' | ';' | ' ' => false,
        sym => !sym.is_whitespace(),
    }
}

// parse a single Lisp expression from a string
fn parse_expr(input: &str) -> LispResult<(Expr, usize)> {
    if input.is_empty() {
        return Err(anyhow!("Attempted to parse an empty input!"));
    }

    let first_char = input.chars().next().unwrap();
    let second_char_is_numeric = input
        .chars()
        .nth(1)
        .map(|c| c.is_numeric())
        .unwrap_or(false);

    let (item, next_pos) = match first_char {
        _num if first_char.is_numeric() || (first_char == '-' && second_char_is_numeric) => {
            parse_num(input)?
        }
        '"' => parse_string(input)?,
        '(' => parse_sexp(input)?,
        '{' => parse_dict(input)?,
        '#' => {
            let (inner_sexp, next_pos) = parse_sexp(&input[1..])?;
            (
                Expr::List(vector![Expr::Symbol("anon-fn-sugar".into()), inner_sexp]),
                next_pos + 1,
            )
        }
        '@' => {
            let (inner_sexp, next_pos) = parse_sexp(&input[1..])?;
            (
                inner_sexp.push_front(Expr::Symbol("partial".into()))?,
                next_pos + 1,
            )
        }
        '^' => {
            let (inner_sexp, next_pos) = parse_sexp(&input[1..])?;
            let mut list = inner_sexp.get_list()?;
            list.push_front(Expr::Symbol("tuple".into()));
            (Expr::List(list), next_pos + 1)
        }
    };
    Ok((item, next_pos))
}
