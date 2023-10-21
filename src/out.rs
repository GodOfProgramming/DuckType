pub(crate) mod instance_value {
    use crate::prelude::*;
    #[uuid("988a6bd1-4a54-416f-aad5-0d1cc8ce652e")]
    pub struct InstanceValue {
        #[trace]
        pub data: StructValue,
        #[trace]
        pub class: Value,
    }
    impl Usertype for InstanceValue {
        const ID: uuid::Uuid = {
            ::uuid::Uuid::from_bytes([
                152u8,
                138u8,
                107u8,
                209u8,
                74u8,
                84u8,
                65u8,
                111u8,
                170u8,
                213u8,
                13u8,
                28u8,
                200u8,
                206u8,
                101u8,
                46u8,
            ])
        };
    }
    impl TraceableValue for InstanceValue {
        fn trace(&self, marks: &mut Marker) {
            self.data.trace(marks);
            self.class.trace(marks);
        }
    }
    #[automatically_derived]
    impl ::core::default::Default for InstanceValue {
        #[inline]
        fn default() -> InstanceValue {
            InstanceValue {
                data: ::core::default::Default::default(),
                class: ::core::default::Default::default(),
            }
        }
    }
    impl InstanceValue {
        pub fn new(data: StructValue, class: Value) -> Self {
            Self { data, class }
        }
    }
    impl UsertypeFields for InstanceValue {
        fn get_field(&self, gc: &mut Gc, field: &str) -> ValueResult<Option<Value>> {
            if field == "__class__" {
                Ok(Some(self.class.clone()))
            } else {
                self.data.get_field(gc, field)
            }
        }
        fn set_field(
            &mut self,
            _gc: &mut Gc,
            field: &str,
            value: Value,
        ) -> ValueResult<()> {
            if field == "__class__" {
                Err(ValueError::Immutable(field.to_string()))
            } else {
                Ok(self.data.set(field, value))
            }
        }
    }
    impl UsertypeMethods for InstanceValue {
        fn get_method(
            &self,
            gc: &mut Gc,
            this: &Value,
            name: &str,
        ) -> ValueResult<Option<Value>> {
            if let Some(class) = self.class.as_class() {
                Ok(class.get_method(gc, this, name))
            } else {
                Ok(None)
            }
        }
    }
    impl DisplayValue for InstanceValue {
        fn __str__(&self) -> String {
            {
                let res = ::alloc::fmt::format(
                    format_args!("<instance of {0}>", self.class),
                );
                res
            }
        }
    }
    impl DebugValue for InstanceValue {
        fn __dbg__(&self) -> String {
            self.__str__()
        }
    }
    impl LockableValue for InstanceValue {}
}
