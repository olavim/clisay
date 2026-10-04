//! Call-graph ordering.

use std::collections::HashMap;

use super::{CallableId, Collector};

pub(super) struct CallableGroup {
    pub members: Vec<CallableId>,
    /// Whether the group calls back into itself.
    pub self_recursive: bool,
}

impl<'a> Collector<'a> {
    /// Runs `step` over every callable, callees before callers, until nothing grows. Only a group
    /// that calls back into itself can grow after its first pass, so only it repeats.
    pub(super) fn converge_groups(&mut self, step: impl Fn(&mut Self, CallableId) -> bool) {
        let groups = std::mem::take(&mut self.callable_groups);
        for group in &groups {
            loop {
                let mut changed = false;
                for member in &group.members {
                    self.current = Some(*member);
                    changed |= step(self, *member);
                }
                if !changed || !group.self_recursive {
                    break;
                }
            }
        }
        self.callable_groups = groups;
    }

    /// Every callable, grouped so a group comes after everything its members call.
    pub(super) fn callee_first_groups(&self) -> Vec<CallableGroup> {
        let nodes: Vec<CallableId> = self.sigs.fns.keys().copied().collect();
        let edges = nodes.iter()
            .map(|node| (*node, self.bodies.get(node).map(|b| b.callees.clone()).unwrap_or_default()))
            .collect();
        Components::of(&nodes, &edges)
    }
}

/// Tarjan's strongly connected components. Groups come out in reverse topological order.
struct Components {
    next: usize,
    marks: HashMap<CallableId, Mark>,
    stack: Vec<CallableId>,
    out: Vec<CallableGroup>,
}

/// Where the walk first reached a node, and whether it's still waiting for its group to close.
#[derive(Clone, Copy)]
struct Mark {
    index: usize,
    on_stack: bool,
}

impl Components {
    fn of(nodes: &[CallableId], edges: &HashMap<CallableId, Vec<CallableId>>) -> Vec<CallableGroup> {
        let mut state = Components {
            next: 0,
            marks: HashMap::with_capacity(nodes.len()),
            stack: Vec::new(),
            out: Vec::new(),
        };
        for &node in nodes {
            if !state.marks.contains_key(&node) {
                state.visit(node, edges);
            }
        }
        state.out
    }

    /// Finds the earliest node reachable from `node`.
    fn visit(&mut self, node: CallableId, edges: &HashMap<CallableId, Vec<CallableId>>) -> usize {
        let index = self.next;
        self.next += 1;
        self.marks.insert(node, Mark { index, on_stack: true });
        self.stack.push(node);

        let callees = edges.get(&node).map(Vec::as_slice).unwrap_or_default();
        let mut low = index;
        for &callee in callees {
            let reach = match self.marks.get(&callee) {
                None => self.visit(callee, edges),
                Some(mark) if mark.on_stack => mark.index,
                Some(_) => continue,
            };
            low = low.min(reach);
        }

        if low == index {
            let mut members = Vec::new();
            while let Some(member) = self.stack.pop() {
                if let Some(mark) = self.marks.get_mut(&member) {
                    mark.on_stack = false;
                }
                members.push(member);
                if member == node {
                    break;
                }
            }
            let self_recursive = members.len() > 1 || callees.contains(&node);
            self.out.push(CallableGroup { members, self_recursive });
        }
        low
    }
}
