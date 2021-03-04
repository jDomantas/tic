#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Sticky {
    Both,
    Prev,
    Next,
    None,
}

impl Sticky {
    fn to_prev(self) -> bool {
        matches!(self, Sticky::Both | Sticky::Prev)
    }
    
    fn to_next(self) -> bool {
        matches!(self, Sticky::Both | Sticky::Next)
    }
}

pub struct Node {
    pub large: bool,
    pub indents: bool,
}

pub struct FormatBuilder {
    root: FormatNode,
}

impl FormatBuilder {
    pub fn new() -> FormatBuilder {
        FormatBuilder {
            root: FormatNode {
                node: Node {
                    large: false,
                    indents: false,
                },
                children: Vec::new(),
                finished: false,
            }
        }
    }

    fn current_node(&mut self) -> &mut FormatNode {
        fn go(node: &mut FormatNode) -> &mut FormatNode {
            match node.children.last_mut() {
                Some(Element::Node(n)) if !n.finished => {
                    return match node.children.last_mut() {
                        Some(Element::Node(n)) => go(n),
                        _ => unreachable!(),
                    };
                }
                _ => {}
            }
            node
        }
        go(&mut self.root)
    }

    pub fn add_token(&mut self, text: &str) {
        let current = self.current_node();
        current.children.push(Element::Token(text.to_owned(), Sticky::None));
    }

    pub fn add_sticky_token(&mut self, text: &str, sticky: Sticky) {
        let current = self.current_node();
        current.children.push(Element::Token(text.to_owned(), sticky));
    }

    pub fn add_newline(&mut self) {
        self.current_node().children.push(Element::Newline)
    }

    pub fn add_large_newline(&mut self) {
        self.current_node().children.push(Element::LargeNewline)
    }

    pub fn add_whitespace(&mut self, ws: &str) {
        for c in ws.chars() {
            if c == '\n' {
                self.add_newline();
                return;
            }
        }
    }

    pub fn start_node(&mut self, node: Node) {
        self.current_node().children.push(Element::Node(FormatNode {
            node,
            children: Vec::new(),
            finished: false,
        }));
    }

    pub fn finish_node(&mut self) {
        self.current_node().finished = true;
    }

    pub fn finish(mut self) -> String {
        let mut raw = RawBuilder::new();
        self.root.mark_large();
        self.root.format(&mut raw);
        raw.result
    }
}

struct FormatNode {
    node: Node,
    children: Vec<Element>,
    finished: bool,
}

impl FormatNode {
    fn mark_large(&mut self) {
        for c in &mut self.children {
            if let Element::Node(n) = c {
                n.mark_large();
                self.node.large |= n.node.large;
            }
        }
    }

    fn format(&self, raw: &mut RawBuilder) {
        for c in &self.children {
            match c {
                Element::Token(t, s) => {
                    raw.add_token(t, *s);
                }
                Element::Node(n) => {
                    if n.node.indents {
                        raw.node(|raw| {
                            n.format(raw);
                        });
                    } else {
                        n.format(raw);
                    }
                }
                Element::Newline => {
                    raw.add_newline();
                }
                Element::LargeNewline => {
                    if self.node.large {
                        raw.add_newline();
                    }
                }
            }
        }
    }
}

enum Element {
    Token(String, Sticky),
    Node(FormatNode),
    Newline,
    LargeNewline,
}

#[derive(Debug)]
struct IndentNode {
    indent: u32,
    has_tokens: bool,
    has_newline: bool,
}

struct RawBuilder {
    result: String,
    node_stack: Vec<IndentNode>,
    has_newline: bool,
    needs_indent: bool,
    needs_space: bool,
}

impl RawBuilder {
    fn new() -> RawBuilder {
        RawBuilder {
            result: String::new(),
            node_stack: vec![IndentNode { indent: 0, has_tokens: true, has_newline: true }],
            has_newline: true,
            needs_indent: false,
            needs_space: false,
        }
    }

    fn add_token(&mut self, text: &str, sticky: Sticky) {
        assert!(!self.needs_space || !self.needs_indent);
        if self.needs_space && !sticky.to_prev() {
            self.result.push(' ');
        }
        if self.needs_indent {
            // println!("indenting, stack: {:?}", self.indent_stack);
            // while self.indent_stack.last().unwrap().depth > self.depth {
            //     self.indent_stack.pop();
            // }
            // if self.indent_stack.last().unwrap().depth != self.depth {
            //     self.indent_stack.push(IndentElem { depth: self.depth, has_tokens: false });
            // }
            // println!("adjusted stack: {:?}", self.indent_stack);
            let indent = self.node_stack
                .iter()
                .rev()
                .find_map(|n| if n.has_tokens {
                    Some(n.indent)
                } else {
                    None
                })
                .unwrap();
            for _ in 0..indent {
                self.result.push_str("    ");
            }
            self.needs_indent = false;
        }
        self.result.push_str(text);
        self.node_stack.last_mut().unwrap().has_tokens = true;
        self.needs_space = !sticky.to_next();
        self.has_newline = false;
    }

    fn add_newline(&mut self) {
        if !self.has_newline {
            self.result.push('\n');
            self.has_newline = true;
            self.needs_space = false;
            self.needs_indent = true;
            self.node_stack.last_mut().unwrap().has_newline = true;
        }
    }

    fn node(&mut self, contents: impl FnOnce(&mut RawBuilder)) {
        let prev_node = self.node_stack.last().unwrap();
        let indent = prev_node.indent + u32::from(prev_node.has_newline);
        self.node_stack.push(IndentNode {
            indent,
            has_tokens: false,
            has_newline: false,
        });
        // self.result.push('{');
        contents(self);
        // self.result.push('}');
        self.node_stack.pop();
    }
}
