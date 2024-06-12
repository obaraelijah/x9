use rustyline::{
    completion::{Completer, Pair}, error::ReadlineError, highlight::{Highlighter, MatchingBracketHighlighter}, hint::Hinter, validate::{self, MatchingBracketValidator, Validator}, Context
};
use rustyline_derive::Helper;
use structopt::StructOpt;
use std::borrow::Cow;

use crate::ast::SymbolTable;

#[derive(Debug, StructOpt)]
#[structopt(name = "x9", about = "x9 Programming Language")]
pub struct Options {
    pub files: Vec<String>,
    #[structopt(
        short = "e",
        long,
        help = "Execute the file(s), and then load the interpreter"
    )]
    pub load_file: bool,

    #[structopt(
        short = "f",
        long = "format",
        help = "WIP: Format some incoming x9 on stdin"
    )]
    pub formatter: bool,

    #[structopt(short = "d", long = "debugger", help = "WIP: :^)")]
    pub debugger: bool,
}

impl Default for Options {
    fn default() -> Self {
        Options {
            files: Vec::with_capacity(0),
            load_file: false,
            formatter: false,
            debugger: false,
        }
    }
}

#[derive(Helper)]
struct Completions {
    sym_table: SymbolTable,
    highlighter: MatchingBracketHighlighter,
    validator: MatchingBracketValidator,
    coloured_prompt: String,
}

impl Completions {
    fn new(sym_table: SymbolTable) -> Self {
        Self {
            sym_table,
            highlighter: MatchingBracketHighlighter::new(),
            validator: MatchingBracketValidator::new(),
            coloured_prompt: ">>>".into(),
        }
    }
}

impl Completer for Completions {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &Context<'_>,
    ) -> Result<(usize, Vec<Pair>), ReadlineError> {
        if pos < line.len() || line.is_empty() {
            return Ok((pos, vec![]));
        }
        let last_sym = line
            .chars()
            .rev()
            .position(|c| c == '(' || c == ' ' || c == ')')
            .unwrap_or(pos);
        let line_fragment = &line[(pos - last_sym)..pos];
        if line_fragment.is_empty() {
            return Ok((pos, vec![]));
        }
        Ok((
            pos,
            self.sym_table
                .query_symbol_starts_with(line_fragment)
                .into_iter()
                .map(|matched| Pair {
                    display: matched.clone(),
                    replacement: matched[last_sym..].into(),
                })
                .collect(),
        ))
    }
}

impl Highlighter for Completions {
    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        default: bool,
    ) -> Cow<'b, str> {
        if default {
            Cow::Borrowed(&self.coloured_prompt)
        } else {
            Cow::Borrowed(prompt)
        }
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Cow::Owned("\x1b[1m".to_owned() + hint + "\x1b[m")
    }

    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        self.highlighter.highlight(line, pos)
    }

    fn highlight_char(&self, line: &str, pos: usize, forced: bool) -> bool {
        self.highlighter.highlight_char(line, pos, forced)
    }
}

impl Validator for Completions {
    fn validate(
        &self,
        ctx: &mut validate::ValidationContext,
    ) -> rustyline::Result<validate::ValidationResult> {
        self.validator.validate(ctx)
    }

    fn validate_while_typing(&self) -> bool {
        self.validator.validate_while_typing()
    }
}

impl Hinter for Completions {
    type Hint = String;

    fn hint(&self, line: &str, pos: usize, _ctx: &rustyline::Context<'_>) -> Option<Self::Hint> {
        if pos < line.len() || line.is_empty() {
            return None;
        }
        let last_sym = line
            .chars()
            .rev()
            .position(|c| c == '(' || c == ' ' || c == ')')
            .unwrap_or(pos);
        let line_fragment = &line[(pos - last_sym)..pos];
        if line_fragment.is_empty() {
            return None;
        }
        self.sym_table
            .query_symbol_starts_with(line_fragment)
            .into_iter()
            .next()
            .map(|s| s[last_sym..].to_owned())
    }
}
