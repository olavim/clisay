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
        let start = self.tokens.peek(0).pos.clone();
        while self.at_clause_atom() {
            self.parse_clause_atom(&mut clause, slot)?;
        }
        clause.pos = Some(start.to(&self.tokens.previous().pos));

        Ok(clause)
    }

    fn parse_clause_atom(&mut self, clause: &mut SlotClause, slot: SlotKind) -> Result<(), anyhow::Error> {
        if self.tokens.matches(TokenType::LeftBracket) {
            self.parse_obligation_container(clause, slot)
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
        self.tokens.peek(0).contextual() == Some(ContextualKeyword::Mut)
    }

    fn parse_mut_marker(&mut self, clause: &mut SlotClause, slot: SlotKind) -> Result<(), anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();

        match slot {
            SlotKind::Return => {},
            // A named slot puts the marker ahead of the name, which is the one spelling it has.
            SlotKind::Param | SlotKind::Receiver => return Err(self.error_help(
                "A capability leads the name, not the ':' clause", &pos,
                format!("write it ahead of the {}, as in `mut x`", slot.label()))),
            SlotKind::Local | SlotKind::Field | SlotKind::Member => return Err(self.error_help(
                format!("A {} cannot carry a mutability capability", slot.label()), &pos,
                "'mut' leads a parameter's name, or rides a return's clause")),
        }
        if clause.capability != Capability::None {
            parse_error!(self, &pos, "Repeated mutability capability");
        }

        // The capability leads the clause, so each clause has one canonical spelling.
        if !clause.names.is_empty() || clause.container {
            return Err(self.error_help(
                "Mutability must lead the ':' clause",
                &pos,
                "move 'mut' ahead of the obligations",
            ));
        }

        self.tokens.next();
        clause.capability = Capability::Mut;
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

    fn parse_obligation_container(&mut self, clause: &mut SlotClause, slot: SlotKind) -> Result<(), anyhow::Error> {
        let open = self.tokens.expect(TokenType::LeftBracket)?.pos.clone();
        if !slot.allows_container() {
            return Err(self.error_help(format!("A {} cannot be a container", slot.label()), &open,
                "'[obl]' names an array or dict whose elements owe the obligation"));
        }
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
