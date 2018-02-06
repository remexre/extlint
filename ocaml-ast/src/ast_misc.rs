#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum ArgLabel {
    NoLabel,
    Labelled(String),
    Optional(String),
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum LongIdent {
    Ident(String),
    Dot(Box<LongIdent>, String),
    Apply(Box<LongIdent>, Box<LongIdent>),
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum Variance {
    Covariant,
    Contravariant,
    Invariant,
}
