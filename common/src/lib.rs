#![feature(hash_set_entry)]
#![feature(inherent_associated_types)]

extern crate slotmap;

use std::{cmp::{Eq, Ord}, collections::{HashSet, BTreeMap}, fmt::{Display, Write}, hash::Hash, marker::PhantomData};
use slotmap::{SecondaryMap, SlotMap};


pub trait Token : Hash + Eq + Ord + Copy + Display {
}

impl Token for char {}

slotmap::new_key_type! {
    pub struct SymbolKey;
    pub struct InnerRuleKey;
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct RuleKey(pub SymbolKey, pub InnerRuleKey);

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Symbol<T: Token> {
    Terminal(T),
    NonTerminal(T),
}

impl <T: Token> Display for Symbol<T> where T: Display {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbol::Terminal(ref token) =>
                write!(f, "{}", token),
            
            Symbol::NonTerminal(ref token) =>
                write!(f, "{}", token),
        }
    }
}


#[derive(Debug, Hash, PartialEq, Eq)]
pub struct DeriveRule<T: Token> {
    pub lhs: SymbolKey,
    pub rhs: Vec<SymbolKey>,
    _phantom: PhantomData<T>,
}

impl <T: Token> DeriveRule<T> {

    pub fn new(lhs: SymbolKey, rhs: Vec<SymbolKey>) -> Self {
        Self { lhs, rhs, _phantom: PhantomData }
    }
}

pub struct Language<T: Token> {
	pub symbols: SlotMap<SymbolKey, Symbol<T>>,
	pub rules: SecondaryMap<SymbolKey, SlotMap<InnerRuleKey, DeriveRule<T>>>,
	pub starting: SymbolKey,
}

impl <T: Token> Language<T> {

	pub fn new(starting: T) -> Self {
		let mut symbols = SlotMap::with_key();
		let starting = Symbol::NonTerminal(starting);
		let starting = symbols.insert(starting);

		Self { symbols, starting, rules: SecondaryMap::new() }
	}

	pub fn terminal(&mut self, token: T) -> SymbolKey {
		let sym = Symbol::Terminal(token);
		self.symbols.insert(sym)
	}

	pub fn non_terminal(&mut self, token: T) -> SymbolKey {
		let sym = Symbol::NonTerminal(token);
		self.symbols.insert(sym)
	}

	pub fn rule<L: Into<Vec<SymbolKey>>> (&mut self, lhs: SymbolKey, rhs: L) -> RuleKey {
        assert!(/* not */ !matches!(self.symbols[lhs], Symbol::Terminal(_)));

		let rules = self.rules
            .entry(lhs)
            .unwrap()
            .or_insert_with(SlotMap::<InnerRuleKey, DeriveRule<T>>::with_key);

        let rule = DeriveRule::new(lhs, rhs.into());

        let inner_key = rules.insert(rule);

        RuleKey(lhs, inner_key)
	}

    pub fn get_rule(&self, rule_key: RuleKey) -> &DeriveRule<T> {
        &self.rules[rule_key.0][rule_key.1]
    }

    pub fn get_symbol(&self, symbol_key: SymbolKey) -> &Symbol<T> {
        &self.symbols[symbol_key]
    }

}

impl <T: Token> Display for Language<T> 
    where T: Display {

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (lhs, rules) in self.rules.iter() {
            for (_, rule) in rules {
                let lhs = self.symbols[lhs];
                write!(f, "{} -> ", lhs)?;

                for rhs in &rule.rhs {
                    let rhs = self.symbols[*rhs];
                    write!(f, "{}", rhs)?;
                }

                writeln!(f)?;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

	#[test]
	fn test_language() {
		let mut lang = Language::new('S');

		let sym_0 = lang.terminal('0');
		let sym_1 = lang.terminal('1');
		let sym_a = lang.non_terminal('a');


		lang.rule(sym_a, vec![sym_0, sym_1]);
		lang.rule(sym_a, &[sym_1, sym_0][..]);

        let mut s = String::new();

        write!(&mut s, "{}", lang).unwrap();

        println!("{:?}", s);

        assert!(s == "a -> 10\na -> 01\n" || s == "a -> 01\na -> 10\n")

	}
}
