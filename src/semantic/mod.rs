//! The semantic module is responsible for the semantic analysis of the source
//! code. It checks for any kind of semantic errors that the lexer and parser
//! might have missed and can make the code generation phase easier by
//! converting the AST into a more structured form that can be easily more
//! easily converted into the target language or bytecode.
//!
//! The semantic analysis is done in three passes:
//! - The first pass checks for any kind of semantic errors that can be
//!   detected without knowing the types of the variables or without a
//!   detailed knowledge of the code context. This includes things like
//!   checking for redeclarations of variables, checking if a variable is
//!   used before it is declared, etc. During this pass, the semantic
//!   analyzer also builds structures that will be used in the second pass
//!   to check for more complex errors.
//!
//! - The second pass checks for more complex errors that require a detailed
//!   knowledge of the code context. This includes things like checking if a
//!   variable is used in a way that is not allowed by its type, checking if
//!   a function is called with the wrong number of arguments, etc.
//!
//! - Finally, the semantic analyzer converts the AST into a more structured
//!   form that can be easily converted into the target language or bytecode.
//!   During this phase, the semantic analyzer can also perform optimizations
//!   that are easier to do at this stage than at the code generation stage
//!   when some informations about the code semantics are lost.
