use serde::Serialize;

use SerializeError;
use data::Data;

pub struct Serializer;

impl ::serde::Serializer for Serializer {
    type Ok = Data;
    type Error = SerializeError;

    type SerializeSeq = SerializeSeq;
    type SerializeTuple = SerializeStruct;
    type SerializeTupleStruct = SerializeStruct;
    type SerializeTupleVariant = SerializeVariant;
    type SerializeMap = SerializeMap;
    type SerializeStruct = SerializeStruct;
    type SerializeStructVariant = SerializeVariant;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        Ok(Data::Prim("bool", v.to_string()))
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        Ok(Data::Prim("i8", v.to_string()))
    }
    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        Ok(Data::Prim("i16", v.to_string()))
    }
    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        Ok(Data::Prim("i32", v.to_string()))
    }
    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
        Ok(Data::Prim("i64", v.to_string()))
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        Ok(Data::Prim("u8", v.to_string()))
    }
    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        Ok(Data::Prim("u16", v.to_string()))
    }
    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        Ok(Data::Prim("u32", v.to_string()))
    }
    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        Ok(Data::Prim("u64", v.to_string()))
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
        Ok(Data::Prim("f32", v.to_string()))
    }
    fn serialize_f64(self, v: f64) -> Result<Self::Ok, Self::Error> {
        Ok(Data::Prim("f64", v.to_string()))
    }

    fn serialize_char(self, v: char) -> Result<Self::Ok, Self::Error> {
        Ok(Data::Prim("char", v.to_string()))
    }
    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        Ok(Data::Prim("str", v.to_string()))
    }
    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok, Self::Error> {
        use serde::ser::SerializeSeq;

        let mut seq = self.serialize_seq(Some(v.len()))?;
        for b in v {
            seq.serialize_element(&b)?;
        }
        seq.end()
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        use serde::ser::SerializeTupleStruct;

        let mut variant = self.serialize_tuple_struct("Option", 1)?;
        variant.serialize_field("None")?;
        variant.end()
    }
    fn serialize_some<T: ?Sized + Serialize>(
        self,
        v: &T,
    ) -> Result<Self::Ok, Self::Error> {
        use serde::ser::SerializeTupleStruct;

        let mut variant = self.serialize_tuple_struct("Option", 2)?;
        variant.serialize_field("Some")?;
        variant.serialize_field(v)?;
        variant.end()
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        self.serialize_unit_struct("unit")
    }
    fn serialize_unit_struct(
        self,
        name: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        use serde::ser::SerializeTupleStruct;

        let s = self.serialize_tuple_struct(name, 0)?;
        s.end()
    }
    fn serialize_unit_variant(
        self,
        name: &'static str,
        variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        use serde::ser::SerializeTupleVariant;

        let s = self.serialize_tuple_variant(name, variant_index, variant, 0)?;
        s.end()
    }

    fn serialize_newtype_struct<T: ?Sized + Serialize>(
        self,
        name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error> {
        use serde::ser::SerializeTupleStruct;

        let mut s = self.serialize_tuple_struct(name, 1)?;
        s.serialize_field(value)?;
        s.end()
    }
    fn serialize_newtype_variant<T: ?Sized + Serialize>(
        self,
        name: &'static str,
        variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error> {
        use serde::ser::SerializeTupleVariant;

        let mut s =
            self.serialize_tuple_variant(name, variant_index, variant, 1)?;
        s.serialize_field(value)?;
        s.end()
    }

    fn serialize_seq(
        self,
        _len: Option<usize>,
    ) -> Result<Self::SerializeSeq, Self::Error> {
        Ok(SerializeSeq { data: Vec::new() })
    }

    fn serialize_tuple(
        self,
        len: usize,
    ) -> Result<Self::SerializeTuple, Self::Error> {
        self.serialize_tuple_struct("tuple", len)
    }
    fn serialize_tuple_struct(
        self,
        name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        Ok(SerializeStruct {
            data: Vec::new(),
            name,
        })
    }
    fn serialize_tuple_variant(
        self,
        name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Ok(SerializeVariant {
            data: Vec::new(),
            name,
            variant,
        })
    }
    fn serialize_map(
        self,
        _len: Option<usize>,
    ) -> Result<Self::SerializeMap, Self::Error> {
        Ok(SerializeMap {
            data: Vec::new(),
            last_key: None,
        })
    }
    fn serialize_struct(
        self,
        name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        self.serialize_tuple_struct(name, len)
    }
    fn serialize_struct_variant(
        self,
        name: &'static str,
        variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        self.serialize_tuple_variant(name, variant_index, variant, len)
    }
}

pub struct SerializeMap {
    data: Vec<(Data, Data)>,
    last_key: Option<Data>,
}

impl ::serde::ser::SerializeMap for SerializeMap {
    type Ok = Data;
    type Error = SerializeError;

    fn serialize_key<T: Serialize + ?Sized>(
        &mut self,
        key: &T,
    ) -> Result<(), Self::Error> {
        assert!(self.last_key.is_none());
        let key = key.serialize(Serializer)?;
        self.last_key = Some(key);
        Ok(())
    }

    fn serialize_value<T: Serialize + ?Sized>(
        &mut self,
        value: &T,
    ) -> Result<(), Self::Error> {
        let key = self.last_key.take().unwrap();
        let value = value.serialize(Serializer)?;
        self.data.push((key, value));
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Data::Map(self.data))
    }
}

pub struct SerializeSeq {
    data: Vec<Data>,
}

impl ::serde::ser::SerializeSeq for SerializeSeq {
    type Ok = Data;
    type Error = SerializeError;

    fn serialize_element<T: Serialize + ?Sized>(
        &mut self,
        value: &T,
    ) -> Result<(), Self::Error> {
        let value = value.serialize(Serializer)?;
        self.data.push(value);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Data::Seq(self.data))
    }
}

pub struct SerializeStruct {
    data: Vec<Data>,
    name: &'static str,
}

impl ::serde::ser::SerializeStruct for SerializeStruct {
    type Ok = Data;
    type Error = SerializeError;

    fn serialize_field<T: Serialize + ?Sized>(
        &mut self,
        _key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error> {
        let value = value.serialize(Serializer)?;
        self.data.push(value);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Data::Struct(self.name.to_string(), self.data))
    }
}

impl ::serde::ser::SerializeTuple for SerializeStruct {
    type Ok = Data;
    type Error = SerializeError;

    fn serialize_element<T: Serialize + ?Sized>(
        &mut self,
        value: &T,
    ) -> Result<(), Self::Error> {
        let value = value.serialize(Serializer)?;
        self.data.push(value);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Data::Struct(self.name.to_string(), self.data))
    }
}

impl ::serde::ser::SerializeTupleStruct for SerializeStruct {
    type Ok = Data;
    type Error = SerializeError;

    fn serialize_field<T: Serialize + ?Sized>(
        &mut self,
        value: &T,
    ) -> Result<(), Self::Error> {
        let value = value.serialize(Serializer)?;
        self.data.push(value);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Data::Struct(self.name.to_string(), self.data))
    }
}

pub struct SerializeVariant {
    data: Vec<Data>,
    name: &'static str,
    variant: &'static str,
}

impl ::serde::ser::SerializeStructVariant for SerializeVariant {
    type Ok = Data;
    type Error = SerializeError;

    fn serialize_field<T: Serialize + ?Sized>(
        &mut self,
        _key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error> {
        let value = value.serialize(Serializer)?;
        self.data.push(value);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Data::Struct(
            format!("{}_{}", self.name, self.variant),
            self.data,
        ))
    }
}

impl ::serde::ser::SerializeTupleVariant for SerializeVariant {
    type Ok = Data;
    type Error = SerializeError;

    fn serialize_field<T: Serialize + ?Sized>(
        &mut self,
        value: &T,
    ) -> Result<(), Self::Error> {
        let value = value.serialize(Serializer)?;
        self.data.push(value);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Data::Struct(
            format!("{}_{}", self.name, self.variant),
            self.data,
        ))
    }
}
