use ast_core::{CoreType, Expression, Pattern};
use ast_extension::{Attribute, Attributes, Extension};
use ast_locations::{Loc, Location};
use ast_module::ValueBinding;
use ast_misc::{ArgLabel, LongIdent, Variance};

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ClassType {
    pub desc: ClassTypeDesc,
    pub location: Location,
    pub attributes: Attributes,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum ClassTypeDesc {
    Constr(Loc<LongIdent>, Vec<CoreType>),
    Signature(ClassSignature),
    Arrow(ArgLabel, CoreType, Box<ClassType>),
    Extension(Extension),
    Open(bool, Loc<LongIdent>, Box<ClassType>),
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ClassSignature {
    #[serde(rename = "self")] pub self_: CoreType,
    pub fields: Vec<ClassTypeField>,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ClassTypeField {
    pub desc: ClassTypeFieldDesc,
    pub location: Location,
    pub attributes: Attributes,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum ClassTypeFieldDesc {
    Inherit(ClassType),
    Val(Loc<String>, bool, bool, CoreType),
    Method(Loc<String>, bool, bool, CoreType),
    Constraint(CoreType, CoreType),
    Attribute(Attribute),
    Extension(Extension),
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ClassInfos<T> {
    pub virt: bool,
    pub params: Vec<(CoreType, Variance)>,
    pub name: Loc<String>,
    pub expr: Box<T>,
    pub location: Location,
    pub attributes: Attributes,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ClassDescription(ClassInfos<ClassType>);

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ClassTypeDeclaration(ClassInfos<ClassType>);

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ClassExpr {
    pub desc: ClassExprDesc,
    pub location: Location,
    pub attributes: Attributes,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum ClassExprDesc {
    Constr(Loc<LongIdent>, Vec<CoreType>),
    Structure(ClassStructure),
    Fun(ArgLabel, Option<Expression>, Pattern, Box<ClassExpr>),
    Apply(Box<ClassExpr>, Vec<(ArgLabel, Expression)>),
    Let(bool, Vec<ValueBinding>, Box<ClassExpr>),
    Constraint(Box<ClassExpr>, ClassType),
    Extension(Extension),
    Open(bool, Loc<LongIdent>, Box<ClassExpr>),
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ClassStructure {
    #[serde(rename = "self")] pub self_: Pattern,
    pub fields: Vec<ClassField>,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ClassField {
    pub desc: ClassFieldDesc,
    pub location: Location,
    pub attributes: Attributes,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum ClassFieldDesc {
    Inherit(bool, ClassExpr, Option<Loc<String>>),
    Val(Loc<String>, bool, ClassFieldKind),
    Method(Loc<String>, bool, ClassFieldKind),
    Constraint(CoreType, CoreType),
    Initializer(Expression),
    Attribute(Attribute),
    Extension(Extension),
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum ClassFieldKind {
    Virtual(CoreType),
    Concrete(bool, Expression),
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ClassDeclaration(ClassInfos<ClassExpr>);
