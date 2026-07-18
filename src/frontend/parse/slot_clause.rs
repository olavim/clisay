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
                "A ':' clause cannot be empty",
                &self.tokens.peek(0).pos,
                "name at least one obligation after the ':'",
            ));
        }
        while self.at_clause_atom() {
            self.parse_clause_atom(&mut clause, slot)?;
        }

        Ok(clause)
    }

    fn parse_clause_atom(&mut self, clause: &mut SlotClause, slot: SlotKind) -> Result<(), anyhow::Error> {
        if self.tokens.matches(TokenType::LeftBracket) {
            self.parse_obligation_container(clause)
        } else if self.at_void_marker() {
            self.parse_void_marker(clause, slot)
        } else if self.at_mut_marker() {
            self.parse_mut_marker(clause, slot)
        } else {
            self.push_obligation_name(clause)
        }
    }

    fn at_clause_atom(&self) -> bool {
        self.tokens.matches(TokenType::LeftBracket) || self.at_void_marker() || self.at_mut_marker() || self.at_obligation_name()
    }

    fn at_void_marker(&self) -> bool {
        self.tokens.peek(0).contextual() == Some(ContextualKeyword::Void)
    }

    fn at_mut_marker(&self) -> bool {
        matches!(self.tokens.peek(0).contextual(), Some(ContextualKeyword::Mut | ContextualKeyword::Move))
    }

    fn parse_mut_marker(&mut self, clause: &mut SlotClause, slot: SlotKind) -> Result<(), anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();

        if !slot.allows_capability() {
            return Err(self.error_help(format!("A {} cannot carry a mutability capability", slot.label()), &pos, "'mut' / 'move mut' are parameter or return facts"));
        }
        if clause.capability != Capability::None {
            parse_error!(self, &pos, "Repeated mutability capability");
        }

        clause.capability = match self.tokens.next().contextual() {
            Some(ContextualKeyword::Mut) => Capability::Mut,
            _ => {
                if self.tokens.next().contextual() != Some(ContextualKeyword::Mut) {
                    return Err(self.error_help("'move' must be followed by 'mut'", &pos, "ownership transfer is the moving form of 'mut', spelled 'move mut'"));
                }
                Capability::MoveMut
            },
        };
        Ok(())
    }

    fn at_obligation_name(&self) -> bool {
        let tok = self.tokens.peek(0);
        tok.kind == TokenType::Identifier && tok.contextual().is_none()
    }

    /// Whether more of the container body follows before its `]`: another name, or a
    /// malformed token the loop still diagnoses.
    fn at_container_content(&self) -> bool {
        self.at_obligation_name()
            || self.at_void_marker()
            || self.tokens.matches(TokenType::LeftBracket)
            || self.tokens.matches(TokenType::Comma)
    }

    fn parse_obligation_container(&mut self, clause: &mut SlotClause) -> Result<(), anyhow::Error> {
        let open = self.tokens.expect(TokenType::LeftBracket)?.pos.clone();
        clause.container = true;
        while self.at_container_content() {
            let pos = self.tokens.peek(0).pos.clone();
            if self.tokens.matches(TokenType::LeftBracket) {
                parse_error!(self, &pos, "A container obligation cannot nest");
            }
            if self.tokens.matches(TokenType::Comma) {
                parse_error!(self, &pos, "Container obligations are separated by spaces, not commas");
            }
            if self.at_void_marker() {
                parse_error!(self, &pos, "'void' is not a valid container obligation");
            }
            self.push_obligation_name(clause)?;
        }
        self.tokens.expect_close(TokenType::RightBracket, &open)?;
        Ok(())
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
