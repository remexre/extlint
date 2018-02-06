use Constant;
use ast_extension::{Attributes, Extension};
use ast_locations::{Loc, Location};
use ast_misc::{ArgLabel, LongIdent, Variance};

/// A primitive type.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct CoreType {
    pub desc: CoreTypeDesc,
    pub location: Location,
    pub attributes: Attributes,
}

/// The contents of a CoreType.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum CoreTypeDesc {
    Any,
    Var(String),
    Arrow(ArgLabel, Box<CoreType>, Box<CoreType>),
    Tuple(Vec<CoreType>),
    Constr(Loc<LongIdent>, Vec<CoreType>),
    Object(Vec<ObjectField>, bool),
    Class(Loc<LongIdent>, Vec<CoreType>),
    Alias(Box<CoreType>, String),
    Variant(Vec<RowField>, bool, Option<Vec<String>>),
    Poly(Vec<Loc<String>>, Box<CoreType>),
    Package(Loc<LongIdent>, Vec<(Loc<LongIdent>, CoreType)>),
    Extension(Extension),
}

/// A single possiblity in a enumerated type.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum RowField {
    Tag(Loc<String>, Attributes, bool, Vec<CoreType>),
    Inherit(CoreType),
}

/// A variable in a record.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ObjectField {}

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

/// The contents of a single type declaration.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct TypeDeclaration {
    /// The name of the type.
    pub name: Loc<String>,

    /// The type's parameters.
    pub params: Vec<(CoreType, Variance)>,

    // cstrs
    // kind
    // private
    // manifest
    // attributes
    /// The location of the type declaration.
    pub location: Location,
}

pub struct TypeKind;
pub struct LabelDeclaration;
pub struct ConstructorDeclaration;
pub struct ConstructorArguments;
pub struct TypeExtension;
pub struct ExtensionConstructor;
pub struct ExtensionConstructorKind;
