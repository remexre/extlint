use Constant;
use ast_class::ClassStructure;
use ast_extension::{Attributes, Extension};
use ast_locations::{Loc, Location};
use ast_misc::{ArgLabel, LongIdent, Variance};
use ast_module::{ModuleExpr, ValueBinding};

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
    Extension(Box<Extension>),
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PackageType(Loc<LongIdent>, Vec<(Loc<LongIdent>, CoreType)>);

/// A single possiblity in a enumerated type.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum RowField {
    Tag(Loc<String>, Attributes, bool, Vec<CoreType>),
    Inherit(CoreType),
}

/// A variable in a record.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum ObjectField {
    Tag(Loc<String>, Attributes, CoreType),
    Inherit(CoreType),
}

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
    Interval(Constant, Constant),
    Tuple(Vec<Pattern>),
    Construct(Loc<LongIdent>, Option<Box<Pattern>>),
    Variant(String, Option<Box<Pattern>>),
    Record(Vec<(Loc<LongIdent>, Pattern)>, bool),
    Array(Vec<Pattern>),
    Or(Box<Pattern>, Box<Pattern>),
    Constraint(Box<Pattern>, CoreType),
    Type(Loc<LongIdent>),
    Lazy(Box<Pattern>),
    Unpack(Loc<String>),
    Exception(Box<Pattern>),
    Extension(Extension),
    Open(Loc<LongIdent>, Box<Pattern>),
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
    Variant(String, Option<Box<Expression>>),
    Record(Vec<(Loc<LongIdent>, Expression)>, Option<Box<Expression>>),
    Field(Box<Expression>, Loc<LongIdent>),
    SetField(Box<Expression>, Loc<LongIdent>, Box<Expression>),
    Array(Vec<Expression>),
    IfThenElse(Box<Expression>, Box<Expression>, Option<Box<Expression>>),
    Sequence(Box<Expression>, Box<Expression>),
    While(Box<Expression>, Box<Expression>),
    For(
        Box<Pattern>,
        Box<Expression>,
        Box<Expression>,
        bool,
        Box<Expression>,
    ),
    Constraint(Box<Expression>, CoreType),
    Coerce(Box<Expression>, Option<CoreType>, CoreType),
    Send(Box<Expression>, Loc<String>),
    New(Loc<LongIdent>),
    SetInstVar(Loc<String>, Box<Expression>),
    Override(Vec<(Loc<String>, Box<Expression>)>),
    LetModule(Loc<String>, Box<ModuleExpr>, Box<Expression>),
    LetException(ExtensionConstructor, Box<Expression>),
    Assert(Box<Expression>),
    Lazy(Box<Expression>),
    Poly(Box<Expression>, Option<CoreType>),
    Object(ClassStructure),
    NewType(Loc<String>, Box<Expression>),
    Pack(Box<ModuleExpr>),
    Open(bool, Loc<LongIdent>, Box<Expression>),
    Extension(Extension),
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

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ValueDescription {
    pub name: Loc<String>,

    #[serde(rename = "type")] pub type_: CoreType,

    pub prim: Vec<String>,

    pub attributes: Attributes,

    pub loc: Location,
}

/// The contents of a single type declaration.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct TypeDeclaration {
    /// The name of the type.
    pub name: Loc<String>,

    /// The type's parameters.
    pub params: Vec<(CoreType, Variance)>,

    /// The constraints on the type.
    pub cstrs: Vec<(CoreType, CoreType, Location)>,

    /// The kind of a type.
    pub kind: TypeKind,

    /// Whether the type is private or not.
    pub private: bool,

    pub manifest: Option<CoreType>,

    /// The attributes attached to the type declaration.
    pub attributes: Attributes,

    /// The location of the type declaration.
    pub location: Location,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum TypeKind {
    Abstract,
    Variant(Vec<ConstructorDeclaration>),
    Record(Vec<LabelDeclaration>),
    Open,
}

/// A labelled value in a record.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct LabelDeclaration {
    /// The name of the label.
    pub name: Loc<String>,

    /// Whether the value is mutable.
    pub mutable: bool,

    /// The type of the value.
    #[serde(rename = "type")]
    pub type_: CoreType,

    /// The location of the labelled value.
    pub location: Location,

    /// The attributes assosciated with the value.
    pub attributes: Attributes,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ConstructorDeclaration {
    pub name: Loc<String>,
    pub args: ConstructorArguments,
    pub res: Option<CoreType>,
    pub location: Location,
    pub attributes: Attributes,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum ConstructorArguments {
    /// Arguments forming a tuple.
    Tuple(Vec<CoreType>),

    /// Arguments forming a record.
    Record(Vec<LabelDeclaration>),
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct TypeExtension {
    pub path: Loc<LongIdent>,
    pub params: Vec<(CoreType, Variance)>,
    pub constructors: Vec<ExtensionConstructor>,
    pub private: bool,
    pub attributes: Attributes,
}

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
