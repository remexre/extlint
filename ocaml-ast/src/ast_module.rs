use ast_class::{ClassDescription, ClassTypeDeclaration};
use ast_core::{Expression, ExtensionConstructor, Pattern, TypeDeclaration,
               TypeExtension, ValueDescription};
use ast_extension::{Attribute, Attributes, Extension};
use ast_locations::{Loc, Location};
use ast_misc::LongIdent;

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ModuleType {
    pub desc: ModuleTypeDesc,
    pub location: Location,
    pub attributes: Attributes,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum ModuleTypeDesc {
    Ident(Loc<LongIdent>),
    Signature(Signature),
    Functor(Loc<String>, Option<Box<ModuleType>>, Box<ModuleType>),
    With(Box<ModuleType>, Vec<WithConstraint>),
    TypeOf(ModuleExpr),
    Extension(Extension),
    Alias(Loc<LongIdent>),
}

/// A signature.
pub type Signature = Vec<SignatureItem>;

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct SignatureItem {
    pub desc: SignatureItemDesc,
    pub location: Location,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum SignatureItemDesc {
    Value(ValueDescription),
    Type(bool, Vec<TypeDeclaration>),
    TypeExt(TypeExtension),
    Exception(ExtensionConstructor),
    Module(ModuleDeclaration),
    RecModule(Vec<ModuleDeclaration>),
    ModType(ModuleTypeDeclaration),
    Open(OpenDescription),
    Include(IncludeDescription),
    Class(Vec<ClassDescription>),
    ClassType(Vec<ClassTypeDeclaration>),
    Attribute(Attribute),
    Extension(Extension, Attributes),
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ModuleDeclaration {
    pub name: Loc<String>,
    #[serde(rename = "type")] pub type_: ModuleType,
    pub attributes: Attributes,
    pub location: Location,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ModuleTypeDeclaration {
    pub name: Loc<String>,
    #[serde(rename = "type")] pub type_: Option<ModuleType>,
    pub attributes: Attributes,
    pub location: Location,
}

/// The data associated with an open statement.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct OpenDescription {
    pub id: Loc<LongIdent>,
    #[serde(rename = "override")] pub override_: bool,
    pub location: Location,
    pub attributes: Attributes,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct IncludeInfos<T> {
    #[serde(rename = "mod")] pub mod_: T,
    pub location: Location,
    pub attributes: Attributes,
}

pub type IncludeDescription = IncludeInfos<ModuleType>;
pub type IncludeDeclaration = IncludeInfos<ModuleExpr>;

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum WithConstraint {
    Type(Loc<LongIdent>, TypeDeclaration),
    Module(Loc<LongIdent>, Loc<LongIdent>),
    TypeSubst(Loc<LongIdent>, TypeDeclaration),
    ModSubst(Loc<LongIdent>, Loc<LongIdent>),
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ModuleExpr {
    pub desc: ModuleExprDesc,
    pub location: Location,
    pub attributes: Attributes,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum ModuleExprDesc {
    Ident(Loc<LongIdent>),
    Structure(Structure),
    Functor(Loc<String>, Option<Box<ModuleType>>, Box<ModuleExpr>),
    Apply(Box<ModuleExpr>, Box<ModuleExpr>),
    Constraint(Box<ModuleExpr>, Box<ModuleType>),
    Unpack(Expression),
    Extension(Extension),
}

/// A structure.
pub type Structure = Vec<StructureItem>;

/// A top-level item.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct StructureItem {
    /// The location of the StructureItem.
    pub location: Location,

    /// The actual StructureItem.
    pub desc: StructureItemDesc,
}

/// A top-level item. This type is the actual value.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum StructureItemDesc {
    /// The evaluation of an expression.
    Eval(Expression, Attributes),

    /// A definition, like `let x = 5`.
    ///
    /// The flag is true for a `let rec`.
    Value(bool, Vec<ValueBinding>),

    // Primitive,
    /// The definition of a type.
    Type(bool, Vec<TypeDeclaration>),

    // TypeExt,
    /// The declaration of an exception.
    Exception(ExtensionConstructor),

    // Module,
    // RecModule,
    // ModType,
    /// An open statement.
    Open(OpenDescription),
    // Class,
    // ClassType,
    // Include,
    /// An attribute, usually a doc comment.
    Attribute(Attribute),
    // Extension,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ValueBinding {
    pub pat: Pattern,
    pub expr: Expression,
    pub attributes: Attributes,
    pub location: Location,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ModuleBinding {
    pub name: Loc<String>,
    pub expr: ModuleExpr,
    pub attributes: Attributes,
    pub location: Location,
}
