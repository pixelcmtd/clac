use parse_int::parse;
use pest::iterators::*;
use pest::*;
use rand::seq::SliceRandom;
use std::collections::HashMap;

#[derive(Parser)]
#[grammar = "λ.pest"]
struct ΛParser;

#[derive(Clone, Debug)]
pub enum ΛNode {
    Σ(String, ΤNode),
    Λ(String, Box<ΛNode>, ΤNode),
    Α(Box<ΛNode>, Box<ΛNode>),
    Χ(String, Box<ΛNode>),
    Τ(String, ΤNode),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ΤNode {
    Χ,
    Σ(String),
    Λ(Box<ΤNode>, Box<ΤNode>),
    Α(Box<ΤNode>, Box<ΤNode>),
    Υ(Box<ΤNode>, Box<ΤNode>),
    Δ(String, Box<ΤNode>),
}

impl ΤNode {
    pub fn λ(a: ΤNode, b: ΤNode) -> Self {
        ΤNode::Λ(Box::from(a), Box::from(b))
    }

    pub fn α(f: ΤNode, a: ΤNode) -> Self {
        ΤNode::Α(Box::from(f), Box::from(a))
    }

    pub fn υ(a: ΤNode, b: ΤNode) -> Self {
        ΤNode::Υ(Box::from(a), Box::from(b))
    }

    pub fn δ(a: String, b: ΤNode) -> Self {
        ΤNode::Δ(a, Box::from(b))
    }

    // TODO: this is CLEARLY VERY bad and broken, once types are properly implemented, we can test
    pub fn to_string(&self) -> String {
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

// TODO: partialeq for ΤNode
impl PartialEq for ΛNode {
    fn eq(&self, other: &Self) -> bool {
        let s = self.reduce();
        let o = other.reduce();
        // TODO: type comparison once the type system is "done"
        match s.clone() {
            ΛNode::Σ(sv, st) => match o {
                ΛNode::Σ(ov, ot) => sv == ov,
                _ => false,
            },
            ΛNode::Λ(sa, sb, st) => match o.clone() {
                ΛNode::Λ(oa, ob, ot) => {
                    if sa == oa {
                        sb == ob
                    } else {
                        // TODO: check what this does if `oa` is free in `s`
                        s.α_rename(&sa, &oa) == o
                    }
                }
                _ => false,
            },
            ΛNode::Α(sf, sa) => match o {
                ΛNode::Α(of, oa) => sf == of && sa == oa,
                _ => false,
            },
            ΛNode::Χ(sn, sv) => match o {
                ΛNode::Χ(on, ov) => sn == on && sv == ov,
                _ => false,
            },
            ΛNode::Τ(sn, sv) => match o {
                ΛNode::Τ(on, ov) => sn == on && sv == ov,
                _ => false,
            },
        }
    }
}

//impl PartialEq for ΤNode {
//    fn eq(&self, other: &Self) -> bool {
//        let s = self.reduce();
//        let o = other.reduce();
//        match s.clone() {
//            ΤNode::Σ(s) => match o {
//                ΤNode::Σ(o) => s == o,
//                _ => false,
//            },
//        }
//    }
//}

impl ΛNode {
    pub fn π_expand(params: Vec<String>, body: ΛNode) -> ΛNode {
        let mut params = params.into_iter().rev();
        let mut func = ΛNode::λ(
            params.next().unwrap(),
            body.clone(),
            ΤNode::λ(ΤNode::Χ, body.ty().clone()),
        );
        for param in params {
            func = ΛNode::λ(param, func.clone(), ΤNode::λ(ΤNode::Χ, func.ty().clone()));
        }
        func
    }

    pub fn λ(param: String, body: ΛNode, ty: ΤNode) -> ΛNode {
        ΛNode::Λ(param, Box::from(body), ty)
    }

    pub fn α(func: ΛNode, arg: ΛNode) -> ΛNode {
        ΛNode::Α(Box::from(func), Box::from(arg))
    }

    pub fn χ(name: String, body: ΛNode) -> ΛNode {
        ΛNode::Χ(name, Box::from(body))
    }

    pub fn ι(mut n: u32) -> ΛNode {
        let mut num = ΛNode::Σ(String::from("x"), ΤNode::Σ(String::from("α")));
        while n > 0 {
            num = ΛNode::α(
                ΛNode::Σ(
                    String::from("f"),
                    ΤNode::λ(ΤNode::Σ(String::from("α")), ΤNode::Σ(String::from("α"))),
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
                ΤNode::λ(ΤNode::Σ(String::from("α")), ΤNode::Σ(String::from("α"))),
            ),
            ΤNode::δ(
                String::from("α"),
                ΤNode::λ(
                    ΤNode::λ(ΤNode::Σ(String::from("α")), ΤNode::Σ(String::from("α"))),
                    ΤNode::λ(ΤNode::Σ(String::from("α")), ΤNode::Σ(String::from("α"))),
                ),
            ),
        )
    }

    pub fn ty(&self) -> ΤNode {
        match self.clone() {
            ΛNode::Σ(_, t) | ΛNode::Λ(_, _, t) | ΛNode::Τ(_, t) => t,
            // TODO: no!
            ΛNode::Α(f, a) => ΤNode::α(f.ty(), a.ty()),
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
            Rule::variable | Rule::mvariable => {
                Some(ΛNode::Σ(String::from(tree.as_str()), ΤNode::Χ))
            }
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

    pub fn contains(&self, free_variable: &String) -> bool {
        match self {
            ΛNode::Σ(s, _) => *free_variable == *s,
            ΛNode::Λ(arg, body, _) => *arg != *free_variable && body.contains(free_variable),
            ΛNode::Α(func, arg) => func.contains(free_variable) || arg.contains(free_variable),
            ΛNode::Χ(name, body) => *name == *free_variable || body.contains(free_variable),
            ΛNode::Τ(name, ty) => *name == *free_variable, // TODO: || ty.contains
        }
    }

    pub fn α_rename(self, from: &String, to: &String) -> ΛNode {
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

    pub fn to_string(&self) -> String {
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

    pub fn reduce(&self) -> ΛNode {
        reduce(self, &String::new(), &ΛNode::Σ(String::new(), ΤNode::Χ))
    }
}

fn reduce(node: &ΛNode, name: &String, arg: &ΛNode) -> ΛNode {
    match node {
        ΛNode::Σ(ref s, _) => {
            if *s == *name {
                arg.clone()
            } else {
                // TODO: put ι-expansion in the β-reduction
                match parse::<u32>(s) {
                    Ok(n) => ΛNode::ι(n),
                    Err(_) => node.clone(),
                }
            }
        }
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
                // η-reduction
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
                ΛNode::Λ(fparam, body, _) => {
                    // β-reduction
                    // TODO: builtin `=`
                    let x = reduce(&*body, &fparam, &param);
                    // NOTE: this can be μ-optimized
                    if fparam != *name && !x.contains(&name) {
                        reduce(&x, &name, arg)
                    } else {
                        x
                    }
                }
                _ => ΛNode::α(func, param),
            }
        }
    }
}

pub struct ΛCalculus {
    pub vardefs: HashMap<String, ΛNode>,
    pub typedefs: HashMap<String, ΤNode>,
}

impl ΛCalculus {
    pub fn new() -> Self {
        ΛCalculus {
            vardefs: HashMap::new(),
            typedefs: HashMap::new(),
        }
    }

    pub fn parse(statements: &str) -> Vec<ΛNode> {
        ΛParser::parse(Rule::statements, statements)
            .unwrap_or_else(|e| panic!("{}", e))
            .map(ΛNode::from_parse_tree)
            .filter(|x| x.is_some())
            .map(|x| x.unwrap())
            .collect()
    }

    pub fn eval(&mut self, tree: ΛNode) -> ΛNode {
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
            // NOTE: for optimization you might want to flip the order of the checks around
            //       as Vec::contains should be faster than HashMap::get
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
