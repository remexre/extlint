use ast_core::{CoreType, Expression, Pattern};
use ast_locations::Loc;
use ast_module::{Signature, Structure};

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Attribute(Loc<String>, Payload);

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Extension(Loc<String>, Payload);

pub type Attributes = Vec<Attribute>;

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum Payload {
    Structure(Structure),
    Signature(Signature),
    Type(CoreType),
    Pattern(Box<Pattern>, Option<Box<Expression>>),
}
