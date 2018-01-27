use Location;
use ast_core::{Expression, Pattern};
use ast_extension::Attributes;

pub type ModuleType = ();
pub type ModuleTypeDesc = ();

/// A signature.
pub type Signature = Vec<SignatureItem>;

pub type SignatureItem = ();
pub type SignatureItemDesc = ();
pub type ModuleDeclaration = ();
pub type ModuleTypeDeclaration = ();
pub type OpenDescription = ();
pub type IncludeInfos = ();
pub type IncludeDescription = ();
pub type IncludeDeclaration = ();
pub type WithConstraint = ();
pub type ModuleExpr = ();
pub type ModuleExprDesc = ();

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
    Eval,

    /// A definition, like `let x = 5`.
    ///
    /// The flag is true for a `let rec`.
    Value(bool, Vec<ValueBinding>),

    Primitive,
    Type,
    TypeExt,
    Exception,
    Module,
    RecModule,
    ModType,
    Open,
    Class,
    ClassType,
    Include,
    Attribute,
    Extension,
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
