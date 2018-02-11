use ast_core::{Expression, Pattern, TypeDeclaration};
use ast_extension::Attributes;
use ast_locations::{Loc, Location};
use ast_misc::LongIdent;

pub struct ModuleType;
pub struct ModuleTypeDesc;

/// A signature.
pub type Signature = Vec<SignatureItem>;

pub struct SignatureItem;
pub struct SignatureItemDesc;
pub struct ModuleDeclaration;
pub struct ModuleTypeDeclaration;

/// The data associated with an open statement.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct OpenDescription {
    pub id: Loc<LongIdent>,
    #[serde(rename = "override")] pub override_: bool,
    pub location: Location,
    pub attributes: Attributes,
}

pub struct IncludeInfos;
pub struct IncludeDescription;
pub struct IncludeDeclaration;
pub struct WithConstraint;
pub struct ModuleExpr;
pub struct ModuleExprDesc;

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
    // Exception,
    // Module,
    // RecModule,
    // ModType,
    /// An open statement.
    Open(OpenDescription),
    // Class,
    // ClassType,
    // Include,
    // Attribute,
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
pub struct ModuleBinding;
