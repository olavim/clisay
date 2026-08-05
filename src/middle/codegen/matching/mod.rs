//! Match statement, `~` match operator, and `has` codegen.
//!
//! A `match` compiles to a decision tree (`tree`) that shares tests across arms. A single `~` or
//! `has` test compiles to a linear short-circuit sequence (`linear`). This module holds what both
//! share: the scalar vocabulary, slot lookup, and constant pooling.

mod linear;
pub mod tree;

use crate::compiler_error;
use crate::core::value::Value;
use crate::middle::hir::{HirId, HirLiteral, HirMatchElem, HirMatcher, Symbol, TypeId};

use super::Compiler;

/// One member of a tested surface, with what it admits: whether null is allowed and which
/// witnesses. A method, or a trait's member, declares no shape and answers `None`, so only its
/// presence can be asked about.
pub type SurfaceMember = (Symbol, Option<(bool, Vec<TypeId>)>);

/// A matcher literal or shape key.
#[derive(Clone, PartialEq)]
pub enum Scalar {
    Null,
    Bool(bool),
    Num(u64),
    Str(String),
}

impl From<&HirLiteral> for Scalar {
    fn from(lit: &HirLiteral) -> Self {
        match lit {
            HirLiteral::Null => Scalar::Null,
            HirLiteral::Boolean(b) => Scalar::Bool(*b),
            // f64 is not Eq, but the decision tree compares scalars to share and exclude tests, so
            // a number is keyed by its bits. The grammar admits no signed zero or NaN literal, the
            // only values where bit equality parts from the runtime `==`, so the key stays faithful.
            HirLiteral::Number(n) => Scalar::Num(n.to_bits()),
            HirLiteral::String(s) => Scalar::Str(s.clone()),
            _ => unreachable!("a matcher literal or key is a scalar"),
        }
    }
}

impl<'a> Compiler<'a> {
    /// What a structural type test asks of a value: one entry per surface member. An empty answer
    /// asks for nothing beyond a shaped value, exactly as the `{ }` matcher does.
    pub(super) fn surface_members<T: 'static>(&self, matcher: &HirId<HirMatcher>, name: Symbol, node: &HirId<T>) -> Result<Vec<SurfaceMember>, anyhow::Error> {
        let decl = self.bindings.type_ref(matcher);
        let Some(members) = decl.and_then(|d| self.bindings.surface(&d)) else {
            compiler_error!(self, node, "'{}' is not a type or trait", self.hir.text(name));
        };
        // A trait has no layout, so its surface is tested by name.
        let layout = decl.and_then(|d| self.bindings.layout_of_decl(&d));
        Ok(members.to_vec().into_iter()
            .map(|member| (member, layout.and_then(|l| self.member_admits(l, member))))
            .collect())
    }

    /// Interns a member name (a string key) into the constant pool, returning its index.
    pub(in crate::middle::codegen) fn member_constant(&mut self, name: Symbol) -> Result<u8, anyhow::Error> {
        let name_ref = self.gc.intern(self.hir.text(name));
        self.ir.add_constant(Value::from(name_ref))
    }

    /// Pools a scalar-literal `has` key as its runtime value, returning the constant index.
    fn key_constant(&mut self, key: &HirLiteral) -> Result<u8, anyhow::Error> {
        self.scalar_constant(&key.into())
    }

    /// Pools a scalar matcher literal as its runtime value, returning the constant index.
    fn scalar_constant(&mut self, lit: &Scalar) -> Result<u8, anyhow::Error> {
        let value = match lit {
            Scalar::Null => Value::NULL,
            Scalar::Bool(b) => Value::from(*b),
            Scalar::Num(bits) => Value::from(f64::from_bits(*bits)),
            Scalar::Str(s) => Value::from(self.gc.intern(s)),
        };
        self.ir.add_constant(value)
    }
}

/// The reserved local slot a binder name stores into.
fn slot_of(binders: &[(Symbol, u8)], name: Symbol) -> u8 {
    binders.iter().find(|(n, _)| *n == name).map(|(_, slot)| *slot).expect("a binder's slot was recorded")
}

/// Splits array elements at the `..` rest. Returns the fixed prefix before it, the rest element
/// itself, and the fixed suffix after it. With no rest, the prefix holds every element.
fn split_at_rest(elements: &[HirMatchElem]) -> (&[HirMatchElem], Option<&HirMatchElem>, &[HirMatchElem]) {
    match elements.iter().position(|e| matches!(e, HirMatchElem::Rest(_))) {
        Some(p) => (&elements[..p], Some(&elements[p]), &elements[p + 1..]),
        None => (elements, None, &[]),
    }
}
