#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Attribute;

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Extension;

pub type Attributes = Vec<Attribute>;

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Payload;
