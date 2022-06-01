mod code;
pub mod types;

#[cfg(test)]
mod test;

pub use code::Context;
use code::{Compiler, OpCode};
use ptr::SmartPtr;
use std::fs;
use types::{Error, Interpreter};
pub use types::{Value, ValueOpResult};

pub trait New<T> {
  fn new(item: T) -> Self;
}

#[derive(Default)]
pub struct Vpu {
  disassemble: bool,
}

impl Vpu {
  pub fn new(disassemble: bool) -> Self {
    Self { disassemble }
  }

  fn unary_op<F: FnOnce(&mut Context, Value) -> Option<Vec<Error>>>(
    ctx: &mut Context,
    opcode: &OpCode,
    f: F,
  ) -> Option<Vec<Error>> {
    if let Some(v) = ctx.stack_pop() {
      f(ctx, v)
    } else {
      Some(Self::error(
        ctx,
        opcode,
        String::from("cannot operate on empty stack"),
      ))
    }
  }

  fn binary_op<F: FnOnce(&mut Context, Value, Value) -> Option<Vec<Error>>>(
    ctx: &mut Context,
    opcode: &OpCode,
    f: F,
  ) -> Option<Vec<Error>> {
    if let Some(bv) = ctx.stack_pop() {
      if let Some(av) = ctx.stack_pop() {
        f(ctx, av, bv)
      } else {
        Some(Self::error(
          ctx,
          opcode,
          String::from("cannot operate on empty stack"),
        ))
      }
    } else {
      Some(Self::error(
        ctx,
        opcode,
        String::from("cannot operate on empty stack"),
      ))
    }
  }

  fn global_op<F: FnOnce(&mut Context, String) -> Option<Vec<Error>>>(
    ctx: &mut Context,
    opcode: &OpCode,
    index: usize,
    f: F,
  ) -> Option<Vec<Error>> {
    if let Some(name) = ctx.global_const_at(index) {
      if let Value::Str(name) = name {
        f(ctx, name)
      } else {
        Some(Self::error(
          ctx,
          opcode,
          format!("global variable name is not an identifier: {}", name),
        ))
      }
    } else {
      Some(Self::error(
        ctx,
        opcode,
        String::from("global variable name does not exist"),
      ))
    }
  }

  fn error(ctx: &mut Context, opcode: &OpCode, msg: String) -> Vec<Error> {
    vec![ctx.error_at(|opcode_ref| Error::from_ref(msg, opcode, opcode_ref))]
  }
}

impl Interpreter for Vpu {
  fn interpret(&self, mut ctx: SmartPtr<Context>) -> Result<Value, Vec<Error>> {
    while !ctx.done() {
      let opcode = ctx.next();

      #[cfg(debug_assertions)]
      if self.disassemble {
        ctx.display_stack();
        ctx.display_instruction(&opcode, ctx.ip);
      }

      match opcode {
        OpCode::NoOp => break,
        OpCode::Const(index) => {
          if let Some(c) = ctx.const_at(index) {
            ctx.stack_push(c);
          } else {
            return Err(Self::error(
              &mut ctx,
              &opcode,
              String::from("could not lookup constant"),
            ));
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
            return Err(Self::error(
              &mut ctx,
              &opcode,
              format!("could not index stack at pos {}", index),
            ));
          }
        },
        OpCode::AssignLocal(index) => match ctx.stack_peek() {
          Some(v) => ctx.stack_assign(index, v.clone()),
          None => {
            return Err(Self::error(
              &mut ctx,
              &opcode,
              format!("could not replace stack value at pos {}", index),
            ));
          }
        },
        OpCode::AssignMember => match ctx.stack_pop() {
          Some(value) => match ctx.stack_pop() {
            Some(name) => match ctx.stack_peek() {
              Some(obj) => match obj {
                Value::Struct(mut obj) => match name {
                  Value::Str(name) => obj.set(name, value),
                  _ => {
                    return Err(Self::error(
                      &mut ctx,
                      &opcode,
                      String::from("member name is not a string"),
                    ))
                  }
                },
                _ => {
                  return Err(Self::error(
                    &mut ctx,
                    &opcode,
                    String::from("tried to assigning to non object"),
                  ))
                }
              },
              None => {
                return Err(Self::error(
                  &mut ctx,
                  &opcode,
                  String::from("no object on stack to assign to"),
                ))
              }
            },
            None => {
              return Err(Self::error(
                &mut ctx,
                &opcode,
                String::from("no name on stack to assign to"),
              ))
            }
          },
          None => {
            return Err(Self::error(
              &mut ctx,
              &opcode,
              String::from("no value on stack to assign to"),
            ))
          }
        },
        OpCode::LookupGlobal(index) => {
          if let Some(e) = Vpu::global_op(&mut ctx, &opcode, index, |ctx, name| {
            match ctx.lookup_global(&name) {
              Some(g) => {
                ctx.stack_push(g);
                None
              }
              None => Some(Self::error(
                ctx,
                &opcode,
                String::from("use of undefined variable"),
              )),
            }
          }) {
            return Err(e);
          }
        }
        OpCode::DefineGlobal(index) => {
          if let Some(e) = Vpu::global_op(&mut ctx, &opcode, index, |ctx, name| {
            if let Some(v) = ctx.stack_peek() {
              if !ctx.define_global(name, v.clone()) {
                return Some(Self::error(
                  ctx,
                  &opcode,
                  String::from("tried redefining global variable"),
                ));
              }
              None
            } else {
              Some(Self::error(
                ctx,
                &opcode,
                String::from("can not define global using empty stack"),
              ))
            }
          }) {
            return Err(e);
          }
        }
        OpCode::AssignGlobal(index) => {
          if let Some(e) = Vpu::global_op(&mut ctx, &opcode, index, |ctx, name| {
            if let Some(v) = ctx.stack_peek() {
              if !ctx.assign_global(name, v) {
                return Some(Self::error(
                  ctx,
                  &opcode,
                  String::from("tried to assign to nonexistent global"),
                ));
              }
              None
            } else {
              Some(Self::error(
                ctx,
                &opcode,
                String::from("can not assign to global using empty stack"),
              ))
            }
          }) {
            return Err(e);
          }
        }
        OpCode::Equal => {
          if let Some(e) = Vpu::binary_op(&mut ctx, &opcode, |ctx, a, b| {
            ctx.stack_push(Value::new(a == b));
            None
          }) {
            return Err(e);
          }
        }
        OpCode::NotEqual => {
          if let Some(e) = Vpu::binary_op(&mut ctx, &opcode, |ctx, a, b| {
            ctx.stack_push(Value::new(a != b));
            None
          }) {
            return Err(e);
          }
        }
        OpCode::Greater => {
          if let Some(e) = Vpu::binary_op(&mut ctx, &opcode, |ctx, a, b| {
            ctx.stack_push(Value::new(a > b));
            None
          }) {
            return Err(e);
          }
        }
        OpCode::GreaterEqual => {
          if let Some(e) = Vpu::binary_op(&mut ctx, &opcode, |ctx, a, b| {
            ctx.stack_push(Value::new(a >= b));
            None
          }) {
            return Err(e);
          }
        }
        OpCode::Less => {
          if let Some(e) = Vpu::binary_op(&mut ctx, &opcode, |ctx, a, b| {
            ctx.stack_push(Value::new(a < b));
            None
          }) {
            return Err(e);
          }
        }
        OpCode::LessEqual => {
          if let Some(e) = Vpu::binary_op(&mut ctx, &opcode, |ctx, a, b| {
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
              return Err(Self::error(
                &mut ctx,
                &opcode,
                String::from("stack peek failed"),
              ));
            }
          },
          None => {
            return Err(Self::error(
              &mut ctx,
              &opcode,
              String::from("stack pop failed"),
            ));
          }
        },
        OpCode::Add => {
          if let Some(e) = Vpu::binary_op(&mut ctx, &opcode, |ctx, a, b| match a + b {
            Ok(v) => {
              ctx.stack_push(v);
              None
            }
            Err(e) => Some(Self::error(ctx, &opcode, e)),
          }) {
            return Err(e);
          }
        }
        OpCode::Sub => {
          if let Some(e) = Vpu::binary_op(&mut ctx, &opcode, |ctx, a, b| match a - b {
            Ok(v) => {
              ctx.stack_push(v);
              None
            }
            Err(e) => Some(Self::error(ctx, &opcode, e)),
          }) {
            return Err(e);
          }
        }
        OpCode::Mul => {
          if let Some(e) = Vpu::binary_op(&mut ctx, &opcode, |ctx, a, b| match a * b {
            Ok(v) => {
              ctx.stack_push(v);
              None
            }
            Err(e) => Some(Self::error(ctx, &opcode, e)),
          }) {
            return Err(e);
          }
        }
        OpCode::Div => {
          if let Some(e) = Vpu::binary_op(&mut ctx, &opcode, |ctx, a, b| match a / b {
            Ok(v) => {
              ctx.stack_push(v);
              None
            }
            Err(e) => Some(Self::error(ctx, &opcode, e)),
          }) {
            return Err(e);
          }
        }
        OpCode::Mod => {
          if let Some(e) = Vpu::binary_op(&mut ctx, &opcode, |ctx, a, b| match a % b {
            Ok(v) => {
              ctx.stack_push(v);
              None
            }
            Err(e) => Some(Self::error(ctx, &opcode, e)),
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
            return Err(Self::error(
              &mut ctx,
              &opcode,
              String::from("no item on the stack to peek"),
            ))
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
            return Err(Self::error(
              &mut ctx,
              &opcode,
              String::from("no item on the stack to peek"),
            ))
          }
        },
        OpCode::Not => {
          if let Some(e) = Vpu::unary_op(&mut ctx, &opcode, |ctx, v| {
            ctx.stack_push(!v);
            None
          }) {
            return Err(e);
          }
        }
        OpCode::Negate => {
          if let Some(e) = Vpu::unary_op(&mut ctx, &opcode, |ctx, v| match -v {
            Ok(n) => {
              ctx.stack_push(n);
              None
            }
            Err(e) => Some(Self::error(ctx, &opcode, e)),
          }) {
            return Err(e);
          }
        }
        OpCode::Print => {
          if let Some(e) = Vpu::unary_op(&mut ctx, &opcode, |_, v| {
            println!("{}", v);
            None
          }) {
            return Err(e);
          }
        }
        OpCode::Swap => {
          if let Some(e) = Vpu::binary_op(&mut ctx, &opcode, |ctx, a, b| {
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
            return Err(Self::error(
              &mut ctx,
              &opcode,
              String::from("no item on the stack to pop"),
            ))
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
          if let Some(mut value) = ctx.stack_pop() {
            let args = ctx.stack_drain_from(airity);
            match value.call(self, args) {
              Ok(ret) => ctx.stack_push(ret),
              Err(errors) => {
                return Err(
                  errors
                    .into_iter()
                    .map(|e| ctx.error_at(|opcode_ref| Error::from_ref(e, &opcode, opcode_ref)))
                    .collect(),
                )
              }
            }
          } else {
            return Err(Self::error(
              &mut ctx,
              &opcode,
              String::from("cannot operate on empty stack"),
            ));
          }
        }
        OpCode::Return => {
          if let Some(val) = ctx.stack_pop() {
            return Ok(val);
          } else {
            return Ok(Value::Nil);
          }
        }
        OpCode::Req => {
          if let Some(file) = ctx.stack_pop() {
            if let Value::Str(file) = file {
              match fs::read_to_string(&file) {
                Ok(data) => {
                  let new_ctx = Compiler::compile(&file, &data)?;
                  match self.interpret(new_ctx) {
                    Ok(v) => ctx.stack_push(v),
                    Err(mut errors) => {
                      let req_err = Self::error(
                        &mut ctx,
                        &opcode,
                        format!("errors detected while loading file {}", file),
                      );
                      errors.extend(req_err);
                    }
                  }
                }
                Err(e) => {
                  return Err(Self::error(
                    &mut ctx,
                    &opcode,
                    format!("unable to read file {}: {}", file, e),
                  ))
                }
              }
            } else {
              return Err(Self::error(&mut ctx, &opcode, format!("can only load files specified by strings or objects convertible to strings, got {}", file)));
            }
          } else {
            return Err(Self::error(
              &mut ctx,
              &opcode,
              String::from("cannot operate with an empty stack"),
            ));
          }
        }
        OpCode::Index => {
          if let Some(index) = ctx.stack_pop() {
            if let Some(indexable) = ctx.stack_pop() {
              match indexable.index(index) {
                Ok(value) => ctx.stack_push(value),
                Err(err) => {
                  return Err(Self::error(
                    &mut ctx,
                    &opcode,
                    format!("unable to index item {}: {}", indexable, err),
                  ))
                }
              }
            }
          }
        }
        OpCode::CreateList(num_items) => {
          let list = ctx.stack_drain_from(num_items);
          ctx.stack_push(Value::new(list));
        }
        x => unimplemented!("Unimplemented: {:?}", x),
      }
      ctx.advance();
    }

    #[cfg(debug_assertions)]
    if self.disassemble {
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
    Compiler::compile(&file, code)
  }

  pub fn run(&self, ctx: SmartPtr<Context>) -> Result<Value, Vec<Error>> {
    self.vpu.interpret(ctx)
  }
}
