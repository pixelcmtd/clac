extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::iterators::*;
use pest::*;
use std::io::{self, Read};

#[derive(Parser)]
#[grammar = "λ.pest"]
struct ΛParser;

#[derive(Debug)]
enum ΛNode {
    Symbol(String),
    Lambda(Box<ΛNode>, Box<ΛNode>),
    Application(Box<ΛNode>, Box<ΛNode>),
}

impl ΛNode {
    fn from_parse_tree(tree: Pair<Rule>) -> Option<Self> {
        match tree.as_rule() {
            Rule::WHITESPACE | Rule::params => None,
            Rule::variable => Some(ΛNode::Symbol(String::from(tree.as_str()))),
            Rule::item | Rule::body | Rule::pexpr => {
                ΛNode::from_parse_tree(tree.into_inner().next()?)
            }
            Rule::expr => {
                let mut items = tree.into_inner().rev();
                let mut expr = ΛNode::from_parse_tree(items.next()?)?;
                for item in items {
                    expr = ΛNode::Application(
                        Box::from(ΛNode::from_parse_tree(item)?),
                        Box::from(expr),
                    );
                }
                Some(expr)
            }
            Rule::func => {
                let mut inner = tree.into_inner().filter(|x| match x.as_rule() {
                    Rule::params | Rule::body => true,
                    _ => false,
                });
                let mut params = inner.next()?.into_inner().rev();
                let body = inner.next()?;
                let mut func = ΛNode::Lambda(
                    Box::from(ΛNode::from_parse_tree(params.next()?)?),
                    Box::from(ΛNode::from_parse_tree(body)?),
                );
                for param in params {
                    func =
                        ΛNode::Lambda(Box::from(ΛNode::from_parse_tree(param)?), Box::from(func));
                }
                Some(func)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_λs() {
        for s in [
            include_str!("../test/I.λ"),
            include_str!("../test/2.λ"),
            include_str!("../test/succ.λ"),
        ] {
            let _pairs = ΛParser::parse(Rule::func, s).unwrap_or_else(|e| panic!("{}", e));
        }
        // TODO: actually test
    }
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let pairs = ΛParser::parse(Rule::func, &buffer).unwrap_or_else(|e| panic!("{}", e));

    for pair in pairs {
        println!("{:?}", pair);
        println!("{:?}", ΛNode::from_parse_tree(pair));
    }

    Ok(())
}
