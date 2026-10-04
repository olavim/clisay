//! Slot-clause parsing: the `:` clause on a variable, parameter, field, or return.

use super::*;

impl<'parser, 'vm> Parser<'parser, 'vm> {
    /// Parses an optional `:` slot clause. A missing `:` yields an empty clause, but a
    /// present `:` must name at least one atom.
    pub(super) fn parse_slot_clause(&mut self, slot: SlotKind) -> Result<SlotClause, anyhow::Error> {
        let mut clause = SlotClause::default();
        if self.tokens.next_if(TokenType::Colon).is_none() {
            return Ok(clause);
        }

        if !self.at_clause_atom() {
            return Err(self.error_help(
                "Expected an obligation name after ':'",
                &self.tokens.peek(0).pos,
                "a ':' clause names one or more obligations, like 'opt' or 'opt fails'",
            ));
        }
        let start = self.tokens.peek(0).pos.clone();
        while self.at_clause_atom() {
            self.parse_clause_atom(&mut clause, slot)?;
        }
        clause.pos = Some(start.to(&self.tokens.previous().pos));
        if clause.void && !clause.names.is_empty() {
            return Err(self.error_help(
                "A void return cannot also owe an obligation",
                clause.pos.as_ref().unwrap(),
                "a function must return a value on every path or on none",
            ));
        }

        Ok(clause)
    }

    fn parse_clause_atom(&mut self, clause: &mut SlotClause, slot: SlotKind) -> Result<(), anyhow::Error> {
        if self.at_void_marker() {
            return self.parse_void_marker(clause, slot);
        }
        self.push_obligation_name(clause)
    }

    fn at_clause_atom(&self) -> bool {
        self.at_void_marker() || self.at_obligation_name()
    }

    fn at_void_marker(&self) -> bool {
        self.tokens.peek(0).contextual() == Some(ContextualKeyword::Void)
    }

    fn at_obligation_name(&self) -> bool {
        let tok = self.tokens.peek(0);
        tok.kind == TokenType::Identifier && tok.contextual().is_none()
    }

    fn parse_void_marker(&mut self, clause: &mut SlotClause, slot: SlotKind) -> Result<(), anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        self.tokens.next();
        if !slot.allows_void_clause() {
            return Err(self.error_help(format!("A {} cannot be void", slot.label()), &pos, "'void' is a return-only presence fact"));
        }
        if clause.void {
            parse_error!(self, &pos, "Repeated 'void'");
        }
        clause.void = true;
        Ok(())
    }

    fn push_obligation_name(&mut self, clause: &mut SlotClause) -> Result<(), anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        let name = self.parse_identifier()?;
        let sym = self.ast.intern(&name);
        if clause.names.contains(&sym) {
            parse_error!(self, &pos, "Repeated obligation '{name}'");
        }
        clause.names.push(sym);
        Ok(())
    }
}
