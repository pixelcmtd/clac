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
    fn from_parse_tree(tree: Pair<Rule>) -> Self {
        match tree.as_rule() {
            Rule::variable => ΛNode::Symbol(String::from(tree.as_str())),
            Rule::item => ΛNode::from_parse_tree(tree.into_inner().next().unwrap()),
            // TODO:
            Rule::body => {
                let mut items = tree.into_inner().rev();
                println!("{:?}", items);
                let mut body = ΛNode::from_parse_tree(items.next().unwrap());
                for item in items {
                    body = ΛNode::Application(
                        Box::from(ΛNode::from_parse_tree(item)),
                        Box::from(body),
                    );
                }
                body
            }
            Rule::func => {
                let mut inner = tree.into_inner().filter(|x| match x.as_rule() {
                    Rule::params => true,
                    Rule::body => true,
                    _ => false,
                });
                let mut params = inner.next().unwrap().into_inner().rev();
                let body = inner.next().unwrap();
                println!("{:?}", params);
                let mut func = ΛNode::Lambda(
                    Box::from(ΛNode::from_parse_tree(params.next().unwrap())),
                    Box::from(ΛNode::from_parse_tree(body)),
                );
                for param in params {
                    func =
                        ΛNode::Lambda(Box::from(ΛNode::from_parse_tree(param)), Box::from(func));
                }
                func
            }
            // TODO: parse the applications
            _ => ΛNode::Symbol(String::from("")),
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

    println!("{:?}", pairs);

    for pair in pairs {
        println!("{:?}", pair);

        // A pair is a combination of the rule which matched and a span of input
        println!("Rule:    {:?}", pair.as_rule());
        println!("Span:    {:?}", pair.as_span());
        println!("Text:    {}", pair.as_str());

        println!("{:?}", ΛNode::from_parse_tree(pair));
    }

    Ok(())
}
