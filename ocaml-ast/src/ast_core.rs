use {Constant, Location};
use ast_extension::Attributes;
use ast_locations::Loc;
use ast_misc::{ArgLabel, LongIdent};

pub struct CoreType;
pub struct CoreTypeDesc;
pub struct PackageType;
pub struct RowField;
pub struct ObjectField;

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Pattern {
    pub desc: PatternDesc,
    pub location: Location,
    pub attributes: Attributes,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum PatternDesc {
    Any,
    // var
    // alias
    // constant
    // interval
    // tuple
    Construct(Loc<LongIdent>, Option<Box<Pattern>>),
    // variant
    // record
    // array
    // or
    // constraint
    // type
    // lazy
    // unpack
    // exception
    // extension
    // open
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Expression {
    pub desc: ExpressionDesc,
    pub location: Location,
    pub attributes: Attributes,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum ExpressionDesc {
    Ident(Loc<LongIdent>),
    Constant(Constant),
    // let
    // function
    // fun
    Apply(Box<Expression>, Vec<(ArgLabel, Expression)>),
    // match
    // try
    // tuple
    Construct(Loc<LongIdent>, Option<Box<Expression>>),
    // variant
    // record
    // field
    // setfield
    // array
    // ifthenelse
    // sequence
    // while
    // for
    // constraint
    // coerce
    // send
    // new
    // setinstvar
    // override
    // letmodule
    // letexception
    // assert
    // lazy
    // poly
    // object
    // newtype
    // pack
    // open
    // extension
    Unreachable,
}

pub struct Case;
pub struct ValueDescription;
pub struct TypeDeclaration;
pub struct TypeKind;
pub struct LabelDeclaration;
pub struct ConstructorDeclaration;
pub struct ConstructorArguments;
pub struct TypeExtension;
pub struct ExtensionConstructor;
pub struct ExtensionConstructorKind;
