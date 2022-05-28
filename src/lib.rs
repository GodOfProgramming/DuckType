mod code;
pub mod types;

#[cfg(test)]
mod test;

pub use code::Context;
use code::{Compiler, OpCode};
use ptr::SmartPtr;
use types::{Error, Interpreter};
pub use types::{NativeFn, Value};

pub trait New<T> {
  fn new(item: T) -> Self;
}

#[derive(Default)]
pub struct Vpu {
  show_disassembly: bool,
  runtime_disassembly: bool,
}

impl Vpu {
  pub fn new(show_disassembly: bool, runtime_disassembly: bool) -> Self {
    Self {
      show_disassembly,
      runtime_disassembly,
    }
  }

  fn unary_op<F: FnOnce(&mut Context, Value) -> Option<Error>>(
    ctx: &mut Context,
    opcode: &OpCode,
    f: F,
  ) -> Option<Error> {
    if let Some(v) = ctx.stack_pop() {
      f(ctx, v)
    } else {
      Some(ctx.reflect_instruction(|opcode_ref| {
        Error::from_ref(
          String::from("cannot operate on empty stack"),
          opcode,
          opcode_ref,
        )
      }))
    }
  }

  fn binary_op<F: FnOnce(&mut Context, Value, Value) -> Option<Error>>(
    ctx: &mut Context,
    opcode: &OpCode,
    f: F,
  ) -> Option<Error> {
    if let Some(bv) = ctx.stack_pop() {
      if let Some(av) = ctx.stack_pop() {
        f(ctx, av, bv)
      } else {
        Some(ctx.reflect_instruction(|opcode_ref| {
          Error::from_ref(
            String::from("cannot operate on empty stack"),
            opcode,
            opcode_ref,
          )
        }))
      }
    } else {
      Some(ctx.reflect_instruction(|opcode_ref| {
        Error::from_ref(
          String::from("cannot operate on empty stack"),
          opcode,
          opcode_ref,
        )
      }))
    }
  }

  fn global_op<F: FnOnce(&mut Context, String) -> Option<Error>>(
    ctx: &mut Context,
    opcode: &OpCode,
    index: usize,
    f: F,
  ) -> Option<Error> {
    if let Some(name) = ctx.global_const_at(index) {
      if let Value::Str(name) = name {
        f(ctx, name)
      } else {
        Some(ctx.reflect_instruction(|opcode_ref| {
          Error::from_ref(
            format!("global variable name is not an identifier: {}", name),
            opcode,
            opcode_ref,
          )
        }))
      }
    } else {
      Some(ctx.reflect_instruction(|opcode_ref| {
        Error::from_ref(
          String::from("global variable name does not exist"),
          opcode,
          opcode_ref,
        )
      }))
    }
  }
}

impl Interpreter for Vpu {
  fn interpret(&self, ctx: &mut Context) -> Result<Value, Error> {
    while !ctx.done() {
      let opcode = ctx.next();

      #[cfg(debug_assertions)]
      if self.runtime_disassembly {
        ctx.display_stack();
        ctx.display_instruction(&opcode, ctx.ip);
      }

      match opcode {
        OpCode::NoOp => break,
        OpCode::Const(index) => {
          if let Some(c) = ctx.const_at(index) {
            ctx.stack_push(c);
          } else {
            return Err(ctx.reflect_instruction(|opcode_ref| {
              Error::from_ref(
                String::from("could not lookup constant"),
                &opcode,
                opcode_ref,
              )
            }));
          }
        }
        OpCode::Nil => ctx.stack_push(Value::Nil),
        OpCode::True => ctx.stack_push(Value::new(true)),
        OpCode::False => ctx.stack_push(Value::new(false)),
        OpCode::Pop => {
          ctx.stack_pop();
        }
        OpCode::PopN(count) => ctx.stack_pop_n(count),
        OpCode::LookupLocal(index) => match ctx.stack_index(index) {
          Some(l) => ctx.stack_push(l),
          None => {
            return Err(ctx.reflect_instruction(|opcode_ref| {
              Error::from_ref(
                format!("could not index stack at pos {}", index),
                &opcode,
                opcode_ref,
              )
            }));
          }
        },
        OpCode::AssignLocal(index) => match ctx.stack_peek() {
          Some(v) => ctx.stack_assign(index, v),
          None => {
            return Err(ctx.reflect_instruction(|opcode_ref| {
              Error::from_ref(
                format!("could not replace stack value at pos {}", index),
                &opcode,
                opcode_ref,
              )
            }));
          }
        },
        OpCode::LookupGlobal(index) => {
          if let Some(e) = Vpu::global_op(ctx, &opcode, index, |ctx, name| {
            match ctx.lookup_global(&name) {
              Some(g) => {
                ctx.stack_push(g);
                None
              }
              None => Some(ctx.reflect_instruction(|opcode_ref| {
                Error::from_ref(
                  String::from("use of undefined variable"),
                  &opcode,
                  opcode_ref,
                )
              })),
            }
          }) {
            return Err(e);
          }
        }
        OpCode::DefineGlobal(index) => {
          if let Some(e) = Vpu::global_op(ctx, &opcode, index, |ctx, name| {
            if let Some(v) = ctx.stack_pop() {
              if !ctx.define_global(name, v) {
                return Some(ctx.reflect_instruction(|opcode_ref| {
                  Error::from_ref(String::from("tried redefining global variable, this error should be detected in the parsing phase"), &opcode, opcode_ref)
                }));
              }
              None
            } else {
              Some(ctx.reflect_instruction(|opcode_ref| {
                Error::from_ref(
                  String::from("can not define global using empty stack"),
                  &opcode,
                  opcode_ref,
                )
              }))
            }
          }) {
            return Err(e);
          }
        }
        OpCode::AssignGlobal(index) => {
          if let Some(e) = Vpu::global_op(ctx, &opcode, index, |ctx, name| {
            if let Some(v) = ctx.stack_pop() {
              if !ctx.assign_global(name, v) {
                return Some(ctx.reflect_instruction(|opcode_ref| {
                  Error::from_ref(
                    String::from("tried to assign to nonexistent global"),
                    &opcode,
                    opcode_ref,
                  )
                }));
              }
              None
            } else {
              Some(ctx.reflect_instruction(|opcode_ref| {
                Error::from_ref(
                  String::from("can not assign to global using empty stack"),
                  &opcode,
                  opcode_ref,
                )
              }))
            }
          }) {
            return Err(e);
          }
        }
        OpCode::Equal => {
          if let Some(e) = Vpu::binary_op(ctx, &opcode, |ctx, a, b| {
            ctx.stack_push(Value::new(a == b));
            None
          }) {
            return Err(e);
          }
        }
        OpCode::NotEqual => {
          if let Some(e) = Vpu::binary_op(ctx, &opcode, |ctx, a, b| {
            ctx.stack_push(Value::new(a != b));
            None
          }) {
            return Err(e);
          }
        }
        OpCode::Greater => {
          if let Some(e) = Vpu::binary_op(ctx, &opcode, |ctx, a, b| {
            ctx.stack_push(Value::new(a > b));
            None
          }) {
            return Err(e);
          }
        }
        OpCode::GreaterEqual => {
          if let Some(e) = Vpu::binary_op(ctx, &opcode, |ctx, a, b| {
            ctx.stack_push(Value::new(a >= b));
            None
          }) {
            return Err(e);
          }
        }
        OpCode::Less => {
          if let Some(e) = Vpu::binary_op(ctx, &opcode, |ctx, a, b| {
            ctx.stack_push(Value::new(a < b));
            None
          }) {
            return Err(e);
          }
        }
        OpCode::LessEqual => {
          if let Some(e) = Vpu::binary_op(ctx, &opcode, |ctx, a, b| {
            ctx.stack_push(Value::new(a <= b));
            None
          }) {
            return Err(e);
          }
        }
        OpCode::Check => match ctx.stack_pop() {
          Some(a) => match ctx.stack_peek() {
            Some(b) => ctx.stack_push(Value::new(a == b)),
            None => {
              return Err(ctx.reflect_instruction(|opcode_ref| {
                Error::from_ref(String::from("stack peek failed"), &opcode, opcode_ref)
              }))
            }
          },
          None => {
            return Err(ctx.reflect_instruction(|opcode_ref| {
              Error::from_ref(String::from("stack pop failed"), &opcode, opcode_ref)
            }))
          }
        },
        OpCode::Add => {
          if let Some(e) = Vpu::binary_op(ctx, &opcode, |ctx, a, b| match a + b {
            Ok(v) => {
              ctx.stack_push(v);
              None
            }
            Err(e) => {
              Some(ctx.reflect_instruction(|opcode_ref| Error::from_ref(e, &opcode, opcode_ref)))
            }
          }) {
            return Err(e);
          }
        }
        OpCode::Sub => {
          if let Some(e) = Vpu::binary_op(ctx, &opcode, |ctx, a, b| match a - b {
            Ok(v) => {
              ctx.stack_push(v);
              None
            }
            Err(e) => {
              Some(ctx.reflect_instruction(|opcode_ref| Error::from_ref(e, &opcode, opcode_ref)))
            }
          }) {
            return Err(e);
          }
        }
        OpCode::Mul => {
          if let Some(e) = Vpu::binary_op(ctx, &opcode, |ctx, a, b| match a * b {
            Ok(v) => {
              ctx.stack_push(v);
              None
            }
            Err(e) => {
              Some(ctx.reflect_instruction(|opcode_ref| Error::from_ref(e, &opcode, opcode_ref)))
            }
          }) {
            return Err(e);
          }
        }
        OpCode::Div => {
          if let Some(e) = Vpu::binary_op(ctx, &opcode, |ctx, a, b| match a / b {
            Ok(v) => {
              ctx.stack_push(v);
              None
            }
            Err(e) => {
              Some(ctx.reflect_instruction(|opcode_ref| Error::from_ref(e, &opcode, opcode_ref)))
            }
          }) {
            return Err(e);
          }
        }
        OpCode::Mod => {
          if let Some(e) = Vpu::binary_op(ctx, &opcode, |ctx, a, b| match a % b {
            Ok(v) => {
              ctx.stack_push(v);
              None
            }
            Err(e) => {
              Some(ctx.reflect_instruction(|opcode_ref| Error::from_ref(e, &opcode, opcode_ref)))
            }
          }) {
            return Err(e);
          }
        }
        OpCode::Or(count) => match ctx.stack_peek() {
          Some(v) => {
            if v.truthy() {
              ctx.jump(count);
              continue;
            } else {
              ctx.stack_pop();
            }
          }
          None => {
            return Err(ctx.reflect_instruction(|opcode_ref| {
              Error::from_ref(
                String::from("no item on the stack to peek"),
                &opcode,
                opcode_ref,
              )
            }))
          }
        },
        OpCode::And(count) => match ctx.stack_peek() {
          Some(v) => {
            if !v.truthy() {
              ctx.jump(count);
              continue;
            } else {
              ctx.stack_pop();
            }
          }
          None => {
            return Err(ctx.reflect_instruction(|opcode_ref| {
              Error::from_ref(
                String::from("no item on the stack to peek"),
                &opcode,
                opcode_ref,
              )
            }))
          }
        },
        OpCode::Not => {
          if let Some(e) = Vpu::unary_op(ctx, &opcode, |ctx, v| {
            ctx.stack_push(!v);
            None
          }) {
            return Err(e);
          }
        }
        OpCode::Negate => {
          if let Some(e) = Vpu::unary_op(ctx, &opcode, |ctx, v| match -v {
            Ok(n) => {
              ctx.stack_push(n);
              None
            }
            Err(e) => {
              Some(ctx.reflect_instruction(|opcode_ref| Error::from_ref(e, &opcode, opcode_ref)))
            }
          }) {
            return Err(e);
          }
        }
        OpCode::Print => {
          if let Some(e) = Vpu::unary_op(ctx, &opcode, |_, v| {
            println!("{}", v);
            None
          }) {
            return Err(e);
          }
        }
        OpCode::Swap => {
          if let Some(e) = Vpu::binary_op(ctx, &opcode, |ctx, a, b| {
            ctx.stack_push(a);
            ctx.stack_push(b);
            None
          }) {
            return Err(e);
          }
        }
        OpCode::Jump(count) => {
          ctx.jump(count);
          continue;
        }
        OpCode::JumpIfFalse(count) => match ctx.stack_pop() {
          Some(v) => {
            if !v.truthy() {
              ctx.jump(count);
              continue; // ip is now at correct place, so skip advance
            }
          }
          None => {
            return Err(ctx.reflect_instruction(|opcode_ref| {
              Error::from_ref(
                String::from("no item on the stack to pop"),
                &opcode,
                opcode_ref,
              )
            }))
          }
        },
        OpCode::Loop(count) => {
          ctx.loop_back(count);
          continue; // ip is at correct place
        }
        OpCode::End => match ctx.stack_pop() {
          Some(v) => return Ok(v),
          None => return Ok(Value::Nil),
        },
        OpCode::Call(airity) => {
          if let Some(v) = ctx.stack_index_rev(airity) {
            let mut args = Vec::new();
            for _ in 0..airity {
              if let Some(arg) = ctx.stack_pop() {
                args.push(arg);
              } else {
                return Err(ctx.reflect_instruction(|opcode_ref| {
                  Error::from_ref(
                    String::from("no available argument on stack for function call"),
                    &opcode,
                    opcode_ref,
                  )
                }));
              }
            }
            match v {
              Value::Function(mut f) => match f.call(self, args) {
                Ok(v) => {
                  ctx.stack_pop();
                  ctx.stack_push(v);
                }
                Err(e) => {
                  return Err(
                    ctx.reflect_instruction(|opcode_ref| Error::from_ref(e, &opcode, opcode_ref)),
                  )
                }
              },
              Value::NativeFunction(f) => match f(args.drain(0..).rev().collect()) {
                Ok(v) => {
                  ctx.stack_pop();
                  ctx.stack_push(v);
                }
                Err(e) => {
                  return Err(
                    ctx.reflect_instruction(|opcode_ref| Error::from_ref(e, &opcode, opcode_ref)),
                  )
                }
              },
              _ => {
                return Err(ctx.reflect_instruction(|opcode_ref| {
                  Error::from_ref(
                    format!("unable to call non function '{}'", v),
                    &opcode,
                    opcode_ref,
                  )
                }))
              }
            }
          } else {
            return Err(ctx.reflect_instruction(|opcode_ref| {
              Error::from_ref(
                String::from("cannot operate on empty stack"),
                &opcode,
                opcode_ref,
              )
            }));
          }
        }
        OpCode::Return => {
          if let Some(val) = ctx.stack_pop() {
            return Ok(val);
          } else {
            return Ok(Value::Nil);
          }
        }
        x => unimplemented!("Unimplemented: {:?}", x),
      }
      ctx.advance();
    }

    #[cfg(debug_assertions)]
    if self.runtime_disassembly {
      ctx.display_stack();
    }

    Ok(Value::Nil)
  }
}

pub struct Vm<I: Interpreter> {
  vpu: I,
}

impl<I: Interpreter> Vm<I> {
  pub fn new(vpu: I) -> Self {
    Vm { vpu }
  }

  pub fn load(&self, file: String, code: &str) -> Result<SmartPtr<Context>, Vec<Error>> {
    let compiler = Compiler::default();
    compiler.compile(&file, code)
  }

  pub fn run(&self, ctx: &mut Context) -> Result<Value, Error> {
    self.vpu.interpret(ctx)
  }
}
