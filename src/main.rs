use std::collections::HashMap;
use std::fmt;
use std::io;
use std::num::ParseFloatError;
use std::rc::Rc;

/*
  Types
*/

#[derive(Clone)]
enum Exp {
    Bool(bool),
    Symbol(String),
    Number(f64),
    List(Vec<Exp>),
    Func(fn(&[Exp]) -> Result<Exp, LispErr>),
    Lambda(LispLambda),
}

#[derive(Clone)]
struct LispLambda {
    params_exp: Rc<Exp>,
    body_exp: Rc<Exp>,
}

impl fmt::Display for Exp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            Exp::Bool(a) => a.to_string(),
            Exp::Symbol(s) => s.clone(),
            Exp::Number(n) => n.to_string(),
            Exp::List(list) => {
                let xs: Vec<String> = list.iter().map(|x| x.to_string()).collect();
                format!("({})", xs.join(","))
            }
            Exp::Func(_) => "Function {}".to_string(),
            Exp::Lambda(_) => "Lambda {}".to_string(),
        };

        write!(f, "{}", str)
    }
}

#[derive(Debug)]
enum LispErr {
    Reason(String),
}

#[derive(Clone)]
struct Env<'a> {
    data: HashMap<String, Exp>,
    outer: Option<&'a Env<'a>>,
}

/*
  Parse
*/

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
        ")" => Err(LispErr::Reason("unexpected `)`".to_string())),
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
            return Ok((Exp::List(res), rest)); // skip `)`, head to the token after
        }
        let (exp, new_xs) = parse(&xs)?;
        res.push(exp);
        xs = new_xs;
    }
}

fn parse_atom(token: &str) -> Exp {
    match token.as_ref() {
        "true" => Exp::Bool(true),
        "false" => Exp::Bool(false),
        _ => {
            let potential_float: Result<f64, ParseFloatError> = token.parse();
            match potential_float {
                Ok(v) => Exp::Number(v),
                Err(_) => Exp::Symbol(token.to_string().clone()),
            }
        }
    }
}

/*
  Env
*/

macro_rules! ensure_tonicity {
    ($check_fn:expr) => {{
        |args: &[Exp]| -> Result<Exp, LispErr> {
            let floats = parse_list_of_floats(args)?;
            let first = floats
                .first()
                .ok_or(LispErr::Reason("expected at least one number".to_string()))?;
            let rest = &floats[1..];
            fn f(prev: &f64, xs: &[f64]) -> bool {
                match xs.first() {
                    Some(x) => $check_fn(prev, x) && f(x, &xs[1..]),
                    None => true,
                }
            }
            Ok(Exp::Bool(f(first, rest)))
        }
    }};
}

fn default_env<'a>() -> Env<'a> {
    let mut data: HashMap<String, Exp> = HashMap::new();
    data.insert(
        "+".to_string(),
        Exp::Func(|args: &[Exp]| -> Result<Exp, LispErr> {
            let sum = parse_list_of_floats(args)?
                .iter()
                .fold(0.0, |sum, a| sum + a);

            Ok(Exp::Number(sum))
        }),
    );
    data.insert(
        "-".to_string(),
        Exp::Func(|args: &[Exp]| -> Result<Exp, LispErr> {
            let floats = parse_list_of_floats(args)?;
            let first = *floats
                .first()
                .ok_or(LispErr::Reason("expected at least one number".to_string()))?;
            let sum_of_rest = floats[1..].iter().fold(0.0, |sum, a| sum + a);

            Ok(Exp::Number(first - sum_of_rest))
        }),
    );
    data.insert("=".to_string(), Exp::Func(ensure_tonicity!(|a, b| a == b)));
    data.insert(">".to_string(), Exp::Func(ensure_tonicity!(|a, b| a > b)));
    data.insert(">=".to_string(), Exp::Func(ensure_tonicity!(|a, b| a >= b)));
    data.insert("<".to_string(), Exp::Func(ensure_tonicity!(|a, b| a < b)));
    data.insert("<=".to_string(), Exp::Func(ensure_tonicity!(|a, b| a <= b)));

    Env { data, outer: None }
}

fn parse_list_of_floats(args: &[Exp]) -> Result<Vec<f64>, LispErr> {
    args.iter().map(|x| parse_single_float(x)).collect()
}

fn parse_single_float(exp: &Exp) -> Result<f64, LispErr> {
    match exp {
        Exp::Number(num) => Ok(*num),
        _ => Err(LispErr::Reason("expected a number".to_string())),
    }
}

/*
  Eval
*/

fn eval_if_args(arg_forms: &[Exp], env: &mut Env) -> Result<Exp, LispErr> {
    let test_form = arg_forms
        .first()
        .ok_or(LispErr::Reason("expected test form".to_string()))?;
    let test_eval = eval(test_form, env)?;
    match test_eval {
        Exp::Bool(b) => {
            let form_idx = if b { 1 } else { 2 };
            let res_form = arg_forms
                .get(form_idx)
                .ok_or(LispErr::Reason(format!("expected form idx={}", form_idx)))?;
            let res_eval = eval(res_form, env);

            res_eval
        }
        _ => Err(LispErr::Reason(format!(
            "unexpected test form='{}'",
            test_form.to_string()
        ))),
    }
}

fn eval_def_args(arg_forms: &[Exp], env: &mut Env) -> Result<Exp, LispErr> {
    let first_form = arg_forms
        .first()
        .ok_or(LispErr::Reason("expected first form".to_string()))?;
    let first_str = match first_form {
        Exp::Symbol(s) => Ok(s.clone()),
        _ => Err(LispErr::Reason(
            "expected first form to be a symbol".to_string(),
        )),
    }?;
    let second_form = arg_forms
        .get(1)
        .ok_or(LispErr::Reason("expected second form".to_string()))?;
    if arg_forms.len() > 2 {
        return Err(LispErr::Reason("def can only have two forms ".to_string()));
    }
    let second_eval = eval(second_form, env)?;
    env.data.insert(first_str, second_eval);

    Ok(first_form.clone())
}

fn eval_lambda_args(arg_forms: &[Exp]) -> Result<Exp, LispErr> {
    let params_exp = arg_forms
        .first()
        .ok_or(LispErr::Reason("expected args form".to_string()))?;
    let body_exp = arg_forms
        .get(1)
        .ok_or(LispErr::Reason("expected second form".to_string()))?;
    if arg_forms.len() > 2 {
        return Err(LispErr::Reason(
            "fn definition can only have two forms ".to_string(),
        ));
    }

    Ok(Exp::Lambda(LispLambda {
        body_exp: Rc::new(body_exp.clone()),
        params_exp: Rc::new(params_exp.clone()),
    }))
}

fn eval_built_in_form(exp: &Exp, arg_forms: &[Exp], env: &mut Env) -> Option<Result<Exp, LispErr>> {
    match exp {
        Exp::Symbol(s) => match s.as_ref() {
            "if" => Some(eval_if_args(arg_forms, env)),
            "def" => Some(eval_def_args(arg_forms, env)),
            "fn" => Some(eval_lambda_args(arg_forms)),
            _ => None,
        },
        _ => None,
    }
}

fn env_get(k: &str, env: &Env) -> Option<Exp> {
    match env.data.get(k) {
        Some(exp) => Some(exp.clone()),
        None => match &env.outer {
            Some(outer_env) => env_get(k, &outer_env),
            None => None,
        },
    }
}

fn parse_list_of_symbol_strings(form: Rc<Exp>) -> Result<Vec<String>, LispErr> {
    let list = match form.as_ref() {
        Exp::List(s) => Ok(s.clone()),
        _ => Err(LispErr::Reason(
            "expected args form to be a list".to_string(),
        )),
    }?;
    list.iter()
        .map(|x| match x {
            Exp::Symbol(s) => Ok(s.clone()),
            _ => Err(LispErr::Reason(
                "expected symbols in the argument list".to_string(),
            )),
        })
        .collect()
}

fn env_for_lambda<'a>(
    params: Rc<Exp>,
    arg_forms: &[Exp],
    outer_env: &'a mut Env,
) -> Result<Env<'a>, LispErr> {
    let ks = parse_list_of_symbol_strings(params)?;
    if ks.len() != arg_forms.len() {
        return Err(LispErr::Reason(format!(
            "expected {} arguments, got {}",
            ks.len(),
            arg_forms.len()
        )));
    }
    let vs = eval_forms(arg_forms, outer_env)?;
    let mut data: HashMap<String, Exp> = HashMap::new();
    for (k, v) in ks.iter().zip(vs.iter()) {
        data.insert(k.clone(), v.clone());
    }
    Ok(Env {
        data,
        outer: Some(outer_env),
    })
}

fn eval_forms(arg_forms: &[Exp], env: &mut Env) -> Result<Vec<Exp>, LispErr> {
    arg_forms.iter().map(|x| eval(x, env)).collect()
}

fn eval(exp: &Exp, env: &mut Env) -> Result<Exp, LispErr> {
    match exp {
        Exp::Symbol(k) => {
            env_get(k, env).ok_or(LispErr::Reason(format!("unexpected symbol k='{}'", k)))
        }
        Exp::Bool(_a) => Ok(exp.clone()),
        Exp::Number(_a) => Ok(exp.clone()),

        Exp::List(list) => {
            let first_form = list
                .first()
                .ok_or(LispErr::Reason("expected a non-empty list".to_string()))?;
            let arg_forms = &list[1..];
            match eval_built_in_form(first_form, arg_forms, env) {
                Some(res) => res,
                None => {
                    let first_eval = eval(first_form, env)?;
                    match first_eval {
                        Exp::Func(f) => f(&eval_forms(arg_forms, env)?),
                        Exp::Lambda(lambda) => {
                            let new_env = &mut env_for_lambda(lambda.params_exp, arg_forms, env)?;
                            eval(&lambda.body_exp, new_env)
                        }
                        _ => Err(LispErr::Reason("first form must be a function".to_string())),
                    }
                }
            }
        }
        Exp::Func(_) => Err(LispErr::Reason("unexpected form".to_string())),
        Exp::Lambda(_) => Err(LispErr::Reason("unexpected form".to_string())),
    }
}

/*
  Repl
*/

fn parse_eval(expr: String, env: &mut Env) -> Result<Exp, LispErr> {
    let (parsed_exp, _) = parse(&tokenize(expr))?;
    let evaled_exp = eval(&parsed_exp, env)?;

    Ok(evaled_exp)
}

fn slurp_expr() -> String {
    let mut expr = String::new();

    io::stdin()
        .read_line(&mut expr)
        .expect("Failed to read line");

    expr
}

fn main() {
    let env = &mut default_env();
    loop {
        println!("lisp >");
        let expr = slurp_expr();
        match parse_eval(expr, env) {
            Ok(res) => println!("// => {}", res),
            Err(e) => match e {
                LispErr::Reason(msg) => println!("// => {}", msg),
            },
        }
    }
}
