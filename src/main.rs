#![recursion_limit = "256"]

use std::fmt;
use std::rc::Rc;

enum Empty{}

#[derive(Clone)]
enum SExp {
    Undef,
    Nil,
    Cons(Rc<SExp>, Rc<SExp>),
    Bool(bool),
    Number(i64),
    String(String),
    Lambda(Rc<dyn Fn(Rc<SExp>) -> Empty>),
}

impl fmt::Display for SExp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match SExp::clone(self) {
            SExp::Undef => write!(f, "<undefined>"),
            SExp::Nil => write!(f, "()"),
            SExp::Bool(true) => write!(f, "#t"),
            SExp::Bool(false) => write!(f, "#f"),
            SExp::Number(i) => write!(f, "{}", i),
            SExp::String(s) => write!(f, "{}", s),
            SExp::Cons(ref car, ref cdr) => {
                write!(f, "({}", car)?;
                let mut cur = cdr.clone();
                loop {
                    match *cur {
                        SExp::Cons(ref car, ref cdr) => {
                            write!(f, " {}", car)?;
                            cur = cdr.clone();
                        },
                        SExp::Nil => {
                            write!(f, ")")?;
                            break;
                        },
                        _ => {
                            write!(f, " . {})", cdr)?;
                            break;
                        }
                    }
                }
                Ok(())
            },
            SExp::Lambda(_) => write!(f, "<lambda>"),
        }
    }
}

impl std::convert::From<bool> for SExp {
    fn from(x: bool) -> SExp {
        SExp::Bool(x)
    }
}

impl std::convert::From<i64> for SExp {
    fn from(x: i64) -> SExp {
        SExp::Number(x)
    }
}

impl std::convert::From<&str> for SExp {
    fn from(x: &str) -> SExp {
        SExp::String(x.to_string())
    }
}

macro_rules! define {
    ((define $name:ident $expr:tt)) => {
        let mut $name = Rc::new(SExp::Undef);
        let mut cont = |a: Rc<SExp>| {
            $name = a;
        };
        eval!{cont, $expr};
    };
    ((define ($name:ident $( $args:tt )*) $( $body:tt )+)) => {
        let mut $name = Rc::new(SExp::Undef);
        let mut cont = |a: Rc<SExp>| {
            $name = a;
        };
        eval!{cont, (lambda ( $( $args )* ) $( $body )+ )};
    };
}

macro_rules! eval {
    ($cc:ident, ()) => { $cc(Rc::new(SExp::Nil)) };
    ($cc:ident, $lit:literal) => { $cc(Rc::new(SExp::from($lit))) };
    ($cc:ident, (cons $a:tt $b:tt)) => {{
        let cont = |a: Rc<SExp>| -> Empty {
            let cont = |b: Rc<SExp>| -> Empty {
                $cc(Rc::new(SExp::Cons(a, b)))
            };
            eval!{cont, $b}
        };
        eval!{cont, $a}
    }};
    ($cc:ident, (+ $a:tt $b:tt)) => {{
        let cont = |a: Rc<SExp>| -> Empty {
            let SExp::Number(a) = *a else { panic!("not a number"); };
            let cont = |b: Rc<SExp>| -> Empty {
                let SExp::Number(b) = *b else { panic!("not a number"); };
                $cc(Rc::new(SExp::Number(a + b)))
            };
            eval!{cont, $b}
        };
        eval!{cont, $a}
    }};
    ($cc:ident, (if $cond:tt $then:tt $else:tt)) => {{
        let cont = |cond: Rc<SExp>| -> Empty {
            if let SExp::Bool(false) = *cond {
                eval!{$cc, $else}
            } else {
                eval!{$cc, $then}
            }
        };
        eval!{cont, $cond}
    }};
    ($cc:ident, (lambda ($( $arg:ident )*) $( $stmt:tt )+ )) => {{
        $cc(Rc::new(SExp::Lambda(
            Rc::new(|args: Rc<SExp>| {
                let SExp::Cons(ref cont, ref args) = *args else { panic!("foo") };
                $( let SExp::Cons(ref $arg, ref args) = *args.clone() else { panic!("foo") }; let $arg = $arg.clone(); )*
                let SExp::Nil = *args.clone() else { panic!("bar"); };
                if let SExp::Lambda(ref cont) = **cont {
                    let cont = |v| cont(Rc::new(SExp::Cons(v, Rc::new(SExp::Nil))));
                    eval!{cont, (begin $( $stmt )+)}
                } else {
                    panic!("continuation is not a callable");
                }
            })
        )))
    }};
    ($cc:ident, (begin $stmt:tt)) => {{
        eval!{$cc, $stmt}
    }};
    ($cc:ident, (begin $stmt:tt $( $stmts:tt ) + )) => {{
        let cont = |v: Rc<SExp>| -> Empty {
            eval!{$cc, (begin $stmts)};
        };
        eval!{cont, $stmt}
    }};
    ($cc:ident, ($f:tt $( $args:tt )*)) => {{
        let mut args = Vec::<Rc<SExp>>::new();
        args.push(Rc::new(SExp::Lambda(Rc::new($cc))));
        eval!{"expand fcall", ($f $( $args )*), args}
    }};
    ("expand fcall", ($f:tt), $packed:ident) => {{
        let cont = |f: Rc<SExp>| -> Empty {
            if let SExp::Lambda(ref f) = *f {
                let mut args: Rc<SExp> = Rc::new(SExp::Nil);
                for arg in $packed.into_iter().rev() {
                    args = Rc::new(SExp::Cons(arg, args));
                }
                f(args)
            } else {
                panic!("the function position is not a function")
            }
        };
        eval!{cont, $f}
    }};
    ("expand fcall", ($f:tt $arg:tt $( $rest:tt )*), $packed:ident) => {{
        let cont = |arg: Rc<SExp>| -> Empty {
            $packed.push(arg);
             eval!{"expand fcall", ($f $( $rest )*), $packed}
        };
        eval!{cont, $arg}
    }};
    ($cc:ident, $name:ident) => { $cc($name) };
}

fn main() {
    let cont = |v: Rc<SExp>| { println!("{}", v); std::process::exit(0) };
    define!{
        (define (foo a) (if true (cons (+ a 1) (cons "foo" ())) false))
    }

    eval!{
        cont,
        (foo 10)
    };
}
