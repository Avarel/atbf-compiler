/// Pass to despan the AST.
mod despan;

/// Pass to desugar conjunctions and disjunctions into if expressions.
mod shrink;

/// Pass to generate unique names for shadowed variables.
mod uniquify;

/// Pass to flatten lexical scopes.
mod flatten;

/// Pass to change variable expressions to mutable variables into get expressions.
mod uncover_get;

/// Pass to flatten the structure of binary operations, unary operations and calls.
mod remove_complex;

mod explicate_control;

pub use despan::despan;

pub use shrink::shrink;

pub use uniquify::uniquify;

pub use flatten::flatten;

pub use uncover_get::uncover_get;

pub use remove_complex::remove_complex;

pub use explicate_control::explicate_control;