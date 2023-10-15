pub(crate) mod string_value {
    use crate::prelude::*;
    use std::{
        fmt::{Display, Formatter, Result as FmtResult},
        ops::{Deref, DerefMut},
    };
    #[uuid("71d35fbb-2091-40c3-ae3c-5b62b259e8a4")]
    pub struct StringValue {
        str: String,
    }
    #[automatically_derived]
    impl ::core::default::Default for StringValue {
        #[inline]
        fn default() -> StringValue {
            StringValue {
                str: ::core::default::Default::default(),
            }
        }
    }
    impl Usertype for StringValue {
        const ID: uuid::Uuid = {
            ::uuid::Uuid::from_bytes([
                113u8,
                211u8,
                95u8,
                187u8,
                32u8,
                145u8,
                64u8,
                195u8,
                174u8,
                60u8,
                91u8,
                98u8,
                178u8,
                89u8,
                232u8,
                164u8,
            ])
        };
    }
    #[automatically_derived]
    impl UsertypeFields for StringValue {
        fn get_field(&self, field: &str) -> ValueResult<Option<Value>> {
            match field {
                _ => Ok(None),
            }
        }
        fn set_field(&mut self, field: &str, value: Value) -> ValueResult<()> {
            match field {
                _ => Err(ValueError::InvalidAssignment(field.to_string()))?,
            }
            Ok(())
        }
    }
    impl StringValue {
        fn len(&self) -> ValueResult<i32> {
            Ok(self.str.len() as i32)
        }
        fn clone(&self) -> ValueResult<Self> {
            Ok(Self { str: self.str.clone() })
        }
        fn reverse(&self) -> ValueResult<Self> {
            Ok(Self {
                str: self.str.chars().rev().collect::<String>(),
            })
        }
        fn __add__(&self, other: Value) -> ValueResult<Self> {
            Ok(Self {
                str: {
                    let res = ::alloc::fmt::format(format_args!("{0}{1}", self, other));
                    res
                },
            })
        }
        fn __eq__(&self, other: &Self) -> ValueResult<bool> {
            Ok(self.str == other.str)
        }
        fn __index__(&self, index: i32) -> ValueResult {
            Ok(self.chars().nth(index as usize).map(|c| c.into()).unwrap_or_default())
        }
        fn __str__(&self) -> String {
            self.deref().clone()
        }
        fn __dbg__(&self) -> String {
            {
                let res = ::alloc::fmt::format(format_args!("\"{0}\"", self.__str__()));
                res
            }
        }
    }
    #[automatically_derived]
    impl UsertypeMethods for StringValue {
        fn __new__(_args: Args) -> ValueResult {
            Ok(Value::from(Self::default()))
        }
        fn get_method(&self, this: &Value, field: &str) -> ValueResult<Option<Value>> {
            fn try_arg_cast<T>(
                this: Value,
                fn_name: &'static str,
                pos: usize,
            ) -> ValueResult<T>
            where
                T: MaybeFrom<Value>,
            {
                T::maybe_from(this).ok_or(ValueError::InvalidArgument(fn_name, pos))
            }
            match field {
                "len" => {
                    Ok(
                        Some(
                            Value::new_native_fn_method(
                                this.clone(),
                                |_, _, mut args| {
                                    if args.list.len() == 0usize + 1 {
                                        if let Some(this) = args.list.pop() {
                                            if let Some(this) = this.cast_to::<StringValue>() {
                                                let mut args = args.list.into_iter();
                                                Ok(Value::from(StringValue::len(this)?))
                                            } else {
                                                Err(ValueError::BadCast("len", "StringValue", this))
                                            }
                                        } else {
                                            Err(ValueError::MissingSelf("len"))
                                        }
                                    } else {
                                        Err(ValueError::ArgumentError(args.list.len(), 0usize + 1))
                                    }
                                },
                            ),
                        ),
                    )
                }
                "clone" => {
                    Ok(
                        Some(
                            Value::new_native_fn_method(
                                this.clone(),
                                |_, _, mut args| {
                                    if args.list.len() == 0usize + 1 {
                                        if let Some(this) = args.list.pop() {
                                            if let Some(this) = this.cast_to::<StringValue>() {
                                                let mut args = args.list.into_iter();
                                                Ok(Value::from(StringValue::clone(this)?))
                                            } else {
                                                Err(ValueError::BadCast("clone", "StringValue", this))
                                            }
                                        } else {
                                            Err(ValueError::MissingSelf("clone"))
                                        }
                                    } else {
                                        Err(ValueError::ArgumentError(args.list.len(), 0usize + 1))
                                    }
                                },
                            ),
                        ),
                    )
                }
                "reverse" => {
                    Ok(
                        Some(
                            Value::new_native_fn_method(
                                this.clone(),
                                |_, _, mut args| {
                                    if args.list.len() == 0usize + 1 {
                                        if let Some(this) = args.list.pop() {
                                            if let Some(this) = this.cast_to::<StringValue>() {
                                                let mut args = args.list.into_iter();
                                                Ok(Value::from(StringValue::reverse(this)?))
                                            } else {
                                                Err(ValueError::BadCast("reverse", "StringValue", this))
                                            }
                                        } else {
                                            Err(ValueError::MissingSelf("reverse"))
                                        }
                                    } else {
                                        Err(ValueError::ArgumentError(args.list.len(), 0usize + 1))
                                    }
                                },
                            ),
                        ),
                    )
                }
                "__add__" => {
                    Ok(
                        Some(
                            Value::new_native_fn_method(
                                this.clone(),
                                |_, _, mut args| {
                                    if args.list.len() == 1usize + 1 {
                                        if let Some(this) = args.list.pop() {
                                            if let Some(this) = this.cast_to::<StringValue>() {
                                                let mut args = args.list.into_iter();
                                                Ok(
                                                    Value::from(
                                                        StringValue::__add__(
                                                            this,
                                                            try_arg_cast(args.next().unwrap(), "__add__", 0usize)?,
                                                        )?,
                                                    ),
                                                )
                                            } else {
                                                Err(ValueError::BadCast("__add__", "StringValue", this))
                                            }
                                        } else {
                                            Err(ValueError::MissingSelf("__add__"))
                                        }
                                    } else {
                                        Err(ValueError::ArgumentError(args.list.len(), 1usize + 1))
                                    }
                                },
                            ),
                        ),
                    )
                }
                "__eq__" => {
                    Ok(
                        Some(
                            Value::new_native_fn_method(
                                this.clone(),
                                |_, _, mut args| {
                                    if args.list.len() == 1usize + 1 {
                                        if let Some(this) = args.list.pop() {
                                            if let Some(this) = this.cast_to::<StringValue>() {
                                                let mut args = args.list.into_iter();
                                                Ok(
                                                    Value::from(
                                                        StringValue::__eq__(
                                                            this,
                                                            try_arg_cast(args.next().unwrap(), "__eq__", 0usize)?,
                                                        )?,
                                                    ),
                                                )
                                            } else {
                                                Err(ValueError::BadCast("__eq__", "StringValue", this))
                                            }
                                        } else {
                                            Err(ValueError::MissingSelf("__eq__"))
                                        }
                                    } else {
                                        Err(ValueError::ArgumentError(args.list.len(), 1usize + 1))
                                    }
                                },
                            ),
                        ),
                    )
                }
                "__index__" => {
                    Ok(
                        Some(
                            Value::new_native_fn_method(
                                this.clone(),
                                |_, _, mut args| {
                                    if args.list.len() == 1usize + 1 {
                                        if let Some(this) = args.list.pop() {
                                            if let Some(this) = this.cast_to::<StringValue>() {
                                                let mut args = args.list.into_iter();
                                                Ok(
                                                    Value::from(
                                                        StringValue::__index__(
                                                            this,
                                                            try_arg_cast(args.next().unwrap(), "__index__", 0usize)?,
                                                        )?,
                                                    ),
                                                )
                                            } else {
                                                Err(ValueError::BadCast("__index__", "StringValue", this))
                                            }
                                        } else {
                                            Err(ValueError::MissingSelf("__index__"))
                                        }
                                    } else {
                                        Err(ValueError::ArgumentError(args.list.len(), 1usize + 1))
                                    }
                                },
                            ),
                        ),
                    )
                }
                _ => Ok(None),
            }
        }
    }
    impl DisplayValue for StringValue {
        fn __str__(&self) -> String {
            self.deref().clone()
        }
    }
    impl DebugValue for StringValue {
        fn __dbg__(&self) -> String {
            {
                let res = ::alloc::fmt::format(format_args!("\"{0}\"", self.__str__()));
                res
            }
        }
    }
    impl LockableValue for StringValue {}
    impl Display for StringValue {
        fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
            f.write_fmt(format_args!("{0}", self.str))
        }
    }
    impl From<String> for StringValue {
        fn from(str: String) -> Self {
            Self { str, ..Default::default() }
        }
    }
    impl From<&str> for StringValue {
        fn from(str: &str) -> Self {
            Self {
                str: str.to_string(),
                ..Default::default()
            }
        }
    }
    impl Deref for StringValue {
        type Target = String;
        fn deref(&self) -> &Self::Target {
            &self.str
        }
    }
    impl DerefMut for StringValue {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.str
        }
    }
}
