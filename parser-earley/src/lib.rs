#![feature(hash_set_entry)]

extern crate common;

use std::{collections::HashMap, fmt::Write, hash::Hash, usize};

use common::*;


struct ParserState<'l, T: Token> {
    stages: HashMap<usize, ParserStage<'l, T>>,
}

impl <'l, T: Token> ParserState<'l, T> {
    fn stage(&mut self, i: usize) -> &mut ParserStage<'l, T> {
        self.stages.entry(i).or_insert_with(|| {
            ParserStage { steps: Vec::new(), processed: 0, }
        });

        self.stages.get_mut(&i).unwrap()
    }
}

struct ParserStage<'l, T: Token> {
    steps: Vec<ParserStep<'l, T>>,
    processed: usize,
}

impl <'l, T: Token> ParserStage<'l, T> {
    
    fn push(&mut self, step: ParserStep<'l, T>) {
        if !self.steps.contains(&step) {
            self.steps.push(step);
        }
    }   

    fn next(&mut self) -> Option<&ParserStep<'l, T>> {
        if self.has_next() {
            let step = &self.steps[self.processed];
            self.processed += 1;

            Some(step)
        } else {
            None
        }
    }

    fn has_next(&self) -> bool {
        self.processed < self.steps.len()
    }

}

struct ParserStep<'l, T: Token> {
    lang: &'l Language<T>,
    rule: RuleKey,
    h: usize,
    i: usize,
}

impl <'l, T: Token> PartialEq for ParserStep<'l,  T> {
    fn eq(&self, other: &Self) -> bool {
        self.h == other.h && self.i == other.i && self.rule == other.rule
    }
}

impl <'l, T: Token> Eq for ParserStep<'l, T> {
}

impl <'l, T: Token> Hash for ParserStep<'l, T> {
    fn hash<H>(&self, hash: &mut H) where H: std::hash::Hasher { 
        hash.write_usize(self.h);
        hash.write_usize(self.i);
        self.rule.hash(hash);
    }
}

impl <'l, T: Token> ParserStep<'l, T> {
    fn symbol(&self) -> Option<SymbolKey> {
        let rule = self.lang.get_rule(self.rule);
        rule.rhs.get(self.i - self.h).copied()
    }
}

fn print_rule<T: Token>(out: &mut String, lang: &Language<T>, rule: &DeriveRule<T>,
                        h: usize, i: usize, n: usize) {

    let mut s = String::new();

    write!(&mut s, "{} -> ", lang.get_symbol(rule.lhs)).unwrap();

    for idx in 0..rule.rhs.len() {
        let rhs = lang.symbols[rule.rhs[idx]];
        if idx == i - h {
            write!(&mut s, "•").unwrap();
        }
        write!(&mut s, "{}", rhs).unwrap();
    }
    if n == i - h || i - h == rule.rhs.len() {
        write!(&mut s, "•").unwrap();
    }

    write!(out, "{:20} [{}, {}] ", s, h, i).unwrap();
}

pub fn check<T: Token>(lang: &Language<T>, starting: RuleKey, tokens: &[T]) 
    -> Result<(bool, String), std::fmt::Error> {

    let mut state: ParserState<'_, T> = ParserState { stages: HashMap::new() };
    let n = tokens.len();

    let mut out = String::new();
    let mut accepts = false;

    state.stage(0).push(ParserStep {
        lang,
        rule: starting,
        h: 0, i: 0,
    });

    for i in 0..(1 + n) {
        writeln!(&mut out, "\ni = {}", i)?;

        while state.stage(i).has_next() {

            let (step_h, step_lhs, step_rule, sym_key) = {
                let stage = state.stage(i);
                let step = stage.next().unwrap();
                let sym_key = step.symbol();
                
                assert_eq!(step.i, i);

                let rule = lang.get_rule(step.rule);
                print_rule(&mut out, lang, rule, step.h, i, n);
                write!(&mut out, "\t")?;

                (step.h, rule.lhs, step.rule, sym_key)
            };

            match sym_key.map(|key| lang.symbols[key]) {
                Some(Symbol::Terminal(sym)) => {
                    if i == n || sym != tokens[i] {
                        writeln!(&mut out, "Dropper")?;
                        continue
                    }
                    
                    writeln!(&mut out, "Scanner")?;
                    state.stage(i + 1).push(ParserStep {
                        lang, 
                        rule: step_rule,
                        h: step_h,
                        i: i + 1,
                    })
                }
                Some(Symbol::NonTerminal(_)) => {
                    writeln!(&mut out, "Predictor")?;
                    let sym_key = sym_key.unwrap();

                    for (key, _) in &lang.rules[sym_key] {
                        state.stage(i).push(ParserStep {
                            lang, 
                            rule: RuleKey(sym_key, key),
                            h: i,
                            i,
                        })
                    }

                }
                None => {
                    writeln!(&mut out, "Completer")?;
                    let mut buf = Vec::new();
                    
                    for step in &state.stage(step_h).steps {
                        if step.symbol() == Some(step_lhs) {
                            buf.push(ParserStep {
                                lang,
                                rule: step.rule,
                                h: step.h,
                                i,
                            }); 
                        }
                    }

                    for step in buf.into_iter() {
                        state.stage(i).push(step);
                    }
                }
            };

            if step_h == 0 && i == n && step_rule == starting {
                writeln!(&mut out, "Accepting!")?;
                accepts = true;
            }
        }
    }

    Ok((accepts, out))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_check() {
        let mut lang = Language::new('S');

        let sym_S = lang.starting;
        let sym_0 = lang.terminal('0');
        let sym_1 = lang.terminal('1');
        let sym_A = lang.non_terminal('A');
        let sym_B = lang.non_terminal('B');

        let starting = lang.rule(sym_S, &[sym_A][..]);
        lang.rule(sym_A, &[sym_0, sym_B][..]);
        lang.rule(sym_A, &[sym_1, sym_B][..]);
        lang.rule(sym_B, &[sym_0][..]);
        lang.rule(sym_B, &[sym_1][..]);

        println!("{}", lang);

        let ret = check(&lang, starting, &['0', '1'][..]);
        assert!(ret.is_ok());
        let (accepts, list) = ret.unwrap();
        println!("{}", list);
        assert!(accepts);
    }

    #[test]
    fn test_check2() {
        let mut lang = Language::new('S');

        let sym_S = lang.starting;
        let sym_a = lang.terminal('a');
        let sym_plus = lang.terminal('+');
        let sym_star = lang.terminal('*');
        let sym_E = lang.non_terminal('E');
        let sym_T = lang.non_terminal('T');
        let sym_P = lang.non_terminal('P');

        let starting = lang.rule(sym_S, &[sym_E][..]);
        lang.rule(sym_E, &[sym_T][..]);
        lang.rule(sym_E, &[sym_E, sym_plus, sym_T][..]);
        lang.rule(sym_T, &[sym_P][..]);
        lang.rule(sym_T, &[sym_T, sym_star, sym_P][..]);
        lang.rule(sym_P, &[sym_a][..]);

        println!("{}", lang);

        let ret = check(&lang, starting, &['a', '+', 'a', '*', 'a'][..]);
        assert!(ret.is_ok());
        let (accepts, list) = ret.unwrap();
        println!("{}", list);
        assert!(accepts);
    }

}