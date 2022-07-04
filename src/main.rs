extern crate pest;
#[macro_use]
extern crate pest_derive;

use parse_int::parse;
use pest::iterators::*;
use pest::*;
use rand::seq::SliceRandom;
use std::collections::HashMap;
use std::io::{self, Read};

#[derive(Parser)]
#[grammar = "λ.pest"]
struct ΛParser;

#[derive(Clone, Debug, PartialEq)]
enum ΛNode {
    Σ(String, ΤNode),
    Λ(String, Box<ΛNode>, ΤNode),
    Α(Box<ΛNode>, Box<ΛNode>),
    Χ(String, Box<ΛNode>),
    Τ(String, ΤNode),
}

#[derive(Clone, Debug, PartialEq)]
enum ΤNode {
    Χ,
    Σ(String),
    Λ(Box<ΤNode>, Box<ΤNode>),
    Α(Box<ΤNode>, Box<ΤNode>),
    Υ(Box<ΤNode>, Box<ΤNode>),
    Δ(String, Box<ΤNode>),
}

impl ΤNode {
    // TODO: this is CLEARLY VERY bad and broken, once types are properly implemented, we can test
    fn to_string(&self) -> String {
        match self {
            ΤNode::Χ => String::from("Χ"),
            ΤNode::Σ(s) => s.clone(),
            ΤNode::Λ(a, b) => a.to_string() + " → " + &b.to_string(),
            ΤNode::Α(f, a) => f.to_string() + " " + &a.to_string(),
            ΤNode::Δ(a, b) => String::from("δ") + a + "." + &b.to_string(),
            ΤNode::Υ(l, r) => l.to_string() + " ∪ " + &r.to_string(),
        }
    }
}

// TODO: partialeq for ΤNode and ΛNode
// TODO: builtin `=`
//impl PartialEq for ΛNode {
//    // TODO: α-conversion (or η-reduction, maybe β-reduction)
//    fn eq(&self, other: &Self) -> bool {
//        match self {
//            ΛNode::Symbol(s) => match other {
//                ΛNode::Symbol(o) => s == o,
//                _ => false,
//            },
//            ΛNode::Lambda(sa, sb) => match other {
//                ΛNode::Lambda(oa, ob) => sa == oa && sb == ob,
//                _ => false,
//            },
//            ΛNode::Application(sf, sa) => match other {
//                ΛNode::Application(of, oa) => sf == of && sa == oa,
//                _ => false,
//            },
//        }
//    }
//}

impl ΛNode {
    fn π_expand(params: Vec<String>, body: ΛNode) -> ΛNode {
        let mut params = params.into_iter().rev();
        let mut func = ΛNode::λ(params.next().unwrap(), body, ΤNode::Χ);
        for param in params {
            func = ΛNode::λ(String::from(param.as_str()), func, ΤNode::Χ);
        }
        func
    }

    fn λ(param: String, body: ΛNode, ty: ΤNode) -> ΛNode {
        ΛNode::Λ(param, Box::from(body), ty)
    }

    fn α(func: ΛNode, arg: ΛNode) -> ΛNode {
        ΛNode::Α(Box::from(func), Box::from(arg))
    }

    fn χ(name: String, body: ΛNode) -> ΛNode {
        ΛNode::Χ(name, Box::from(body))
    }

    fn ι(mut n: u32) -> ΛNode {
        let mut num = ΛNode::Σ(String::from("x"), ΤNode::Σ(String::from("α")));
        while n > 0 {
            num = ΛNode::α(
                ΛNode::Σ(
                    String::from("f"),
                    ΤNode::Λ(
                        Box::from(ΤNode::Σ(String::from("α"))),
                        Box::from(ΤNode::Σ(String::from("α"))),
                    ),
                ),
                num,
            );
            n -= 1;
        }
        ΛNode::λ(
            String::from("f"),
            ΛNode::λ(
                String::from("x"),
                num,
                ΤNode::Λ(
                    Box::from(ΤNode::Σ(String::from("α"))),
                    Box::from(ΤNode::Σ(String::from("α"))),
                ),
            ),
            ΤNode::Δ(
                String::from("α"),
                Box::from(ΤNode::Λ(
                    Box::from(ΤNode::Λ(
                        Box::from(ΤNode::Σ(String::from("α"))),
                        Box::from(ΤNode::Σ(String::from("α"))),
                    )),
                    Box::from(ΤNode::Λ(
                        Box::from(ΤNode::Σ(String::from("α"))),
                        Box::from(ΤNode::Σ(String::from("α"))),
                    )),
                )),
            ),
        )
    }

    fn ty(&self) -> &ΤNode {
        match self {
            ΛNode::Σ(_, t) | ΛNode::Λ(_, _, t) | ΛNode::Τ(_, t) => t,
            // TODO:
            ΛNode::Α(_, _) => &ΤNode::Χ,
            ΛNode::Χ(_, v) => v.ty(),
        }
    }

    fn from_parse_tree(tree: Pair<Rule>) -> Option<Self> {
        match tree.as_rule() {
            Rule::WHITESPACE
            | Rule::params
            | Rule::mparams
            | Rule::statement
            | Rule::statements
            | Rule::COMMENT
            | Rule::EOI => None,
            Rule::variable | Rule::mvariable => Some(match parse::<u32>(tree.as_str()) {
                Ok(n) => ΛNode::ι(n),
                Err(_) => ΛNode::Σ(String::from(tree.as_str()), ΤNode::Χ),
            }),
            Rule::item | Rule::mitem | Rule::body | Rule::mbody => {
                ΛNode::from_parse_tree(tree.into_inner().next()?)
            }
            Rule::expr | Rule::mexpr => {
                let mut items: Vec<Pair<Rule>> = tree.into_inner().rev().collect();
                Some(if items.len() == 1 {
                    ΛNode::from_parse_tree(items.pop()?)?
                } else {
                    let mut expr = ΛNode::α(
                        ΛNode::from_parse_tree(items.pop()?)?,
                        ΛNode::from_parse_tree(items.pop()?)?,
                    );
                    for item in items {
                        expr = ΛNode::α(expr, ΛNode::from_parse_tree(item)?);
                    }
                    expr
                })
            }
            Rule::func | Rule::mfunc => {
                let mut inner = tree.into_inner().filter(|x| match x.as_rule() {
                    Rule::params | Rule::mparams | Rule::body | Rule::mbody => true,
                    _ => false,
                });
                let params = inner.next()?.into_inner();
                let body = inner.next()?;
                Some(ΛNode::π_expand(
                    params.map(|x| String::from(x.as_str())).collect(),
                    ΛNode::from_parse_tree(body)?,
                ))
            }
            Rule::vardef => {
                let mut inner = tree.into_inner();
                let name = inner.next()?.as_str();
                let body = ΛNode::from_parse_tree(inner.next()?)?;
                Some(ΛNode::χ(String::from(name), body))
            }
        }
    }

    fn contains(&self, free_variable: &String) -> bool {
        match self {
            ΛNode::Σ(s, _) => *free_variable == *s,
            ΛNode::Λ(arg, body, _) => *arg != *free_variable && body.contains(free_variable),
            ΛNode::Α(func, arg) => func.contains(free_variable) || arg.contains(free_variable),
            ΛNode::Χ(name, body) => *name == *free_variable || body.contains(free_variable),
            ΛNode::Τ(name, ty) => *name == *free_variable, // TODO: || ty.contains
        }
    }

    fn α_rename(self, from: &String, to: &String) -> ΛNode {
        match self.clone() {
            ΛNode::Σ(s, t) => {
                if s == *from {
                    ΛNode::Σ(to.clone(), t)
                } else {
                    self
                }
            }
            ΛNode::Λ(a, b, t) => {
                if a == *from {
                    ΛNode::λ(to.clone(), b.α_rename(from, to), t)
                } else {
                    b.α_rename(from, to)
                }
            }
            ΛNode::Α(f, a) => ΛNode::α(f.α_rename(from, to), a.α_rename(from, to)),
            // TODO: check if these 2 can cause shadowing problems
            ΛNode::Χ(n, e) => ΛNode::χ(n, e.α_rename(from, to)),
            ΛNode::Τ(_, _) => self,
        }
    }

    fn to_string(&self) -> String {
        match self {
            ΛNode::Σ(s, _) => s.clone(),
            ΛNode::Χ(name, expr) => name.clone() + " ← " + &expr.to_string(),
            ΛNode::Τ(name, ty) => name.clone() + " ∈ " + &ty.to_string(),
            ΛNode::Λ(arg, body, _) => String::from("λ") + arg + "." + &body.to_string(),
            ΛNode::Α(func, arg) => {
                (match **func {
                    ΛNode::Σ(_, _) | ΛNode::Α(_, _) => func.to_string(),
                    ΛNode::Λ(_, _, _) | ΛNode::Χ(_, _) | ΛNode::Τ(_, _) => {
                        String::from("(") + &func.to_string() + ")"
                    }
                } + &(match **arg {
                    ΛNode::Σ(_, _) => {
                        String::from(match **func {
                            ΛNode::Σ(_, _) | ΛNode::Α(_, _) => " ",
                            _ => "",
                        }) + &arg.to_string()
                    }
                    ΛNode::Λ(_, _, _) | ΛNode::Α(_, _) | ΛNode::Χ(_, _) | ΛNode::Τ(_, _) => {
                        String::from(" (") + &arg.to_string() + ")"
                    }
                }))
            }
        }
    }

    fn reduce(&self) -> ΛNode {
        reduce(self, &String::new(), &ΛNode::Σ(String::new(), ΤNode::Χ))
    }
}

fn reduce(node: &ΛNode, name: &String, arg: &ΛNode) -> ΛNode {
    match node {
        ΛNode::Σ(ref s, _) => if *s == *name { arg } else { node }.clone(),
        // TODO: rethink whether this is actually a good idea (it probably is)
        ΛNode::Χ(n, e) => ΛNode::χ(
            n.clone(),
            if n != name {
                reduce(e, name, arg)
            } else {
                e.reduce()
            },
        ),
        ΛNode::Τ(_, _) => node.clone(),
        ΛNode::Λ(param, body, _) => match &**body {
            ΛNode::Α(func, prm) => {
                if match &**prm {
                    ΛNode::Σ(s, _) => *param == *s && !func.contains(&s.clone()),
                    _ => false,
                } {
                    reduce(&*func, name, arg)
                } else if *param == *name {
                    node.clone()
                } else {
                    ΛNode::λ(param.clone(), reduce(body, name, arg), node.ty().clone())
                }
            }
            _ => {
                // stop β-reduction if the param is shadowed
                if *param == *name {
                    node.clone()
                } else {
                    ΛNode::λ(param.clone(), reduce(body, name, arg), node.ty().clone())
                }
            }
        },
        ΛNode::Α(func, param) => {
            let func = reduce(func, name, arg);
            let param = reduce(param, name, arg);
            match func {
                // FIXME: this causes problems with shadowing
                // TODO: use α-renaming
                ΛNode::Λ(fparam, body, _) => {
                    // TODO:
                    // while fparam == *name {}
                    let x = reduce(&*body, &fparam, &param);
                    let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                        .chars()
                        .collect::<Vec<char>>();
                    let mut name = name.clone();
                    let mut n = 0;
                    while fparam == name || x.contains(&name) {
                        name = chars
                            .choose_multiple(&mut rand::thread_rng(), n / 50 + 1)
                            .fold(String::new(), |s, c| s + &c.to_string());
                        n += 1;
                    }
                    reduce(&x, &name, arg)
                }
                _ => ΛNode::α(func, param),
            }
        }
    }
}

struct ΛCalculus {
    vardefs: HashMap<String, ΛNode>,
    typedefs: HashMap<String, ΤNode>,
}

impl ΛCalculus {
    fn new() -> Self {
        ΛCalculus {
            vardefs: HashMap::new(),
            typedefs: HashMap::new(),
        }
    }

    fn parse(statements: &str) -> Vec<ΛNode> {
        ΛParser::parse(Rule::statements, statements)
            .unwrap_or_else(|e| panic!("{}", e))
            .map(ΛNode::from_parse_tree)
            .filter(|x| x.is_some())
            .map(|x| x.unwrap())
            .collect()
    }

    fn eval(&mut self, tree: ΛNode) -> ΛNode {
        self._eval(tree, Vec::new())
    }

    fn _eval(&mut self, in_tree: ΛNode, mut vars: Vec<String>) -> ΛNode {
        let tree = in_tree.reduce();
        match tree.clone() {
            ΛNode::Α(func, arg) => {
                ΛNode::α(self._eval(*func, vars.clone()), self._eval(*arg, vars)).reduce()
            }
            ΛNode::Λ(arg, body, _) => {
                vars.push(arg.clone());
                ΛNode::λ(arg, self._eval(*body, vars), tree.ty().clone()).reduce()
            }
            ΛNode::Χ(name, expr) => {
                let expr = self._eval(*expr, vars);
                self.vardefs.insert(name.clone(), expr.clone());
                self.typedefs.insert(name, expr.ty().clone());
                expr
            }
            ΛNode::Τ(_, _) => tree,
            ΛNode::Σ(s, _) => match self.vardefs.get(&s) {
                Some(n) => {
                    if vars.contains(&s) {
                        tree
                    } else {
                        self._eval(n.clone(), vars)
                    }
                }
                None => tree,
            },
        }
    }
}

// TODO: more and better tests
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_λs() {
        for case in [
            include_str!("../test/I.λ"),
            include_str!("../test/2.λ"),
            include_str!("../test/succ.λ"),
            include_str!("../test/KII.λ"),
            include_str!("../test/1+1.λ"),
            include_str!("../test/Ix.λ"),
            include_str!("../test/2÷2.λ"),
        ] {
            let mut split = case.split(" → ");
            let tree = &ΛCalculus::parse(split.next().unwrap())[0];
            let normal_form = ΛCalculus::parse(split.next().unwrap())[0].clone();
            assert_eq!(
                tree.to_string(),
                ΛCalculus::parse(&tree.to_string())[0].clone().to_string()
            );
            assert_eq!(tree.reduce().to_string(), normal_form.to_string());
        }
    }
}

fn main() -> io::Result<()> {
    // TODO: cli args
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let exprs = ΛCalculus::parse(&buffer);
    let mut calc = ΛCalculus::new();

    for expr in exprs {
        println!("expression:     {:?}", expr);
        println!("as string:      {}", expr.to_string());
        println!("normal-form:    {}", calc.eval(expr).to_string());
        println!("");
        println!("");
        println!("");
        println!("");
        println!("");
        // TODO: combine?
    }

    println!("vardefs:");
    for (name, expr) in calc.vardefs.clone() {
        println!("  {} ← {}", name, expr.to_string());
    }

    println!("");
    println!("typedefs:");
    for (name, ty) in calc.typedefs.clone() {
        println!("  {} ∈ {}", name, ty.to_string());
    }

    Ok(())
}
