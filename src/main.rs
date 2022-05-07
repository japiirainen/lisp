use std::collections::HashMap;
use std::fmt;
use std::io;
use std::rc::Rc;

#[derive(Clone)]
enum Exp {
    Symbol(String),
    Number(f64),
    List(Vec<Exp>),
    Func(fn(&[Exp]) -> Result<Exp, LispErr>),
}

impl fmt::Display for Exp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            Exp::Symbol(s) => s.clone(),
            Exp::Number(n) => n.to_string(),
            Exp::List(xs) => {
                let xs: Vec<String> = xs.iter().map(|x| x.to_string()).collect();
                format!("({})", xs.join(","))
            }
            Exp::Func(_) => "Function {}".to_string(),
        };
        write!(f, "{}", str)
    }
}

#[derive(Clone)]
enum LispErr {
    Reason(String),
}

#[derive(Clone)]
struct Env {
    data: HashMap<String, Exp>,
}

fn tokenize(expr: String) -> Vec<String> {
    expr.replace("(", " ( ")
        .replace(")", " ) ")
        .split_whitespace()
        .map(|x| x.to_string())
        .collect()
}

fn parse<'a>(tokens: &'a [String]) -> Result<(Exp, &'a [String]), LispErr> {
    let (token, rest) = tokens
        .split_first()
        .ok_or(LispErr::Reason("could not get token".to_string()))?;
    match &token[..] {
        "(" => read_seq(rest),
        ")" => Err(LispErr::Reason("unexpected )".to_string())),
        _ => Ok((parse_atom(token), rest)),
    }
}

fn read_seq<'a>(tokens: &'a [String]) -> Result<(Exp, &'a [String]), LispErr> {
    let mut res: Vec<Exp> = vec![];
    let mut xs = tokens;
    loop {
        let (next_token, rest) = xs
            .split_first()
            .ok_or(LispErr::Reason("could not find closing `)`".to_string()))?;
        if next_token == ")" {
            return Ok((Exp::List(res), rest));
        }
        let (exp, new_xs) = parse(&xs)?;
        res.push(exp);
        xs = new_xs;
    }
}

fn parse_atom(token: &str) -> Exp {
    let potential_float = token.parse();
    match potential_float {
        Ok(v) => Exp::Number(v),
        Err(_) => Exp::Symbol(token.to_string().clone()),
    }
}

fn default_env() -> Env {
    let mut data: HashMap<String, Exp> = HashMap::new();
    data.insert(
        "+".to_string(),
        Exp::Func(|args: &[Exp]| -> Result<Exp, LispErr> {
            let sum = parse_list_of_floats(args)?
                .iter()
                .fold(0.0, |acc, x| acc + x);
            Ok(Exp::Number(sum))
        }),
    );
    data.insert(
        "-".to_string(),
        Exp::Func(|args: &[Exp]| -> Result<Exp, LispErr> {
            let floats = parse_list_of_floats(args)?;
            let first = floats.first().ok_or(LispErr::Reason(
                "expected at least one argument".to_string(),
            ))?;
            let sum_of_rest = floats[1..].iter().fold(0.0, |acc, x| acc + x);
            Ok(Exp::Number(first - sum_of_rest))
        }),
    );

    Env { data }
}

fn parse_list_of_floats(args: &[Exp]) -> Result<Vec<f64>, LispErr> {
    args.iter().map(|x| parse_single_float(x)).collect()
}

fn parse_single_float(exp: &Exp) -> Result<f64, LispErr> {
    match exp {
        Exp::Number(v) => Ok(*v),
        _ => Err(LispErr::Reason("expected a number".to_string())),
    }
}

fn eval(exp: &Exp, env: &mut Env) -> Result<Exp, LispErr> {
    match exp {
        Exp::Symbol(k) => env
            .data
            .get(k)
            .ok_or(LispErr::Reason(format!("unexpected symbol k=' {} '", k)))
            .map(|x| x.clone()),
        Exp::Number(_) => Ok(exp.clone()),
        Exp::List(xs) => {
            let first_form = xs
                .first()
                .ok_or(LispErr::Reason("expected a non-empty list".to_string()))?;
            let arg_form = &xs[1..];
            let first_eval = eval(first_form, env)?;
            match first_eval {
                Exp::Func(f) => {
                    let args_eval = arg_form
                        .iter()
                        .map(|x| eval(x, env))
                        .collect::<Result<Vec<Exp>, LispErr>>();
                    f(&args_eval?)
                }
                _ => Err(LispErr::Reason("first form must be a function".to_string())),
            }
        }
        Exp::Func(_) => Err(LispErr::Reason("unexpected form".to_string())),
    }
}

fn parse_eval(expr: String, env: &mut Env) -> Result<Exp, LispErr> {
    let (parsed_exp, _) = parse(&tokenize(expr))?;
    let evaled_exp = eval(&parsed_exp, env)?;
    Ok(evaled_exp)
}

fn slurp_expr() -> String {
    let mut expr = String::new();
    io::stdin()
        .read_line(&mut expr)
        .expect("Failed to read line from stdin");
    expr
}

fn main() {
    let env = &mut default_env();
    loop {
        println!("list >");
        let expr = slurp_expr();
        match parse_eval(expr, env) {
            Ok(res) => println!("// => {}", res),
            Err(e) => match e {
                LispErr::Reason(msg) => println!("// => {}", msg),
            },
        }
    }
}
