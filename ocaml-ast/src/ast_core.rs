use Constant;
use ast_extension::{Attributes, Extension};
use ast_locations::{Loc, Location};
use ast_misc::{ArgLabel, LongIdent, Variance};
use ast_module::ValueBinding;

/// A primitive type.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct CoreType {
    pub desc: CoreTypeDesc,
    pub location: Location,
    pub attributes: Attributes,
}

/// The contents of a CoreType.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
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
    Var(Loc<String>),
    Alias(Box<Pattern>, Loc<String>),
    Constant(Constant),
    // interval
    Tuple(Vec<Pattern>),
    Construct(Loc<LongIdent>, Option<Box<Pattern>>),
    // variant
    // record
    // array
    Or(Box<Pattern>, Box<Pattern>),
    Constraint(Box<Pattern>, CoreType),
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
    Let(bool, Vec<ValueBinding>, Box<Expression>),
    Function(Vec<Case>),
    Fun(ArgLabel, Option<Box<Expression>>, Pattern, Box<Expression>),
    Apply(Box<Expression>, Vec<(ArgLabel, Expression)>),
    Match(Box<Expression>, Vec<Case>),
    Try(Box<Expression>, Vec<Case>),
    Tuple(Vec<Expression>),
    Construct(Loc<LongIdent>, Option<Box<Expression>>),
    // variant
    Record(Vec<(Loc<LongIdent>, Expression)>, Option<Box<Expression>>),
    Field(Box<Expression>, Loc<LongIdent>),
    SetField(Box<Expression>, Loc<LongIdent>, Box<Expression>),
    // array
    IfThenElse(Box<Expression>, Box<Expression>, Option<Box<Expression>>),
    Sequence(Box<Expression>, Box<Expression>),
    // while
    // for
    Constraint(Box<Expression>, CoreType),
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

/// A single case in a `function` or `match` expression.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Case {
    /// The pattern the value must match.
    pub pat: Pattern,

    /// The guard expression, if any.
    pub guard: Option<Expression>,

    /// The expression evaluated if the pattern matches and the guard
    /// expression evaluates to true.
    pub expr: Expression,
}

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

/// A labelled value in a record.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct LabelDeclaration {
    /// The name of the label.
    pub name: Loc<String>,

    /// Whether the value is mutable.
    pub mutable: bool,

    /// The type of the value.
    #[serde(rename = "type")]
    type_: CoreType,

    /// The location of the labelled value.
    pub location: Location,

    /// The attributes assosciated with the value.
    pub attributes: Attributes,
}

pub struct ConstructorDeclaration;

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum ConstructorArguments {
    /// Arguments forming a tuple.
    Tuple(Vec<CoreType>),

    /// Arguments forming a record.
    Record(Vec<LabelDeclaration>),
}

pub struct TypeExtension;

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ExtensionConstructor {
    /// The name of the constructor.
    pub name: Loc<String>,

    /// The kind of the extension constructor.
    pub kind: ExtensionConstructorKind,

    /// The location of the extension contructor.
    pub location: Location,

    /// Attributes associated with the extension.
    pub attributes: Attributes,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum ExtensionConstructorKind {
    /// A new constructor.
    Decl(ConstructorArguments, Option<CoreType>),

    /// A rebinding of an existing constructor.
    Rebind(Loc<LongIdent>),
}
