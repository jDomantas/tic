use ticc::{Pos, Span};

pub fn extract_spans(source: &str) -> (String, Vec<Span>) {
    let mut fixed_source = String::new();
    let mut spans = Vec::new();
    let mut starts = Vec::new();
    let mut pos = Pos::new(0);
    for c in source.chars() {
        if c == '{' {
            starts.push(pos);
        } else if c == '}' {
            let start = starts.pop().expect("unbalanced span markers");
            spans.push(Span::new(start, pos));
        } else {
            fixed_source.push(c);
            pos = pos.from_origin(Pos::new(1));
        }
    }
    (fixed_source, spans)
}

pub fn extract_single_span(source: &str) -> (String, Span) {
    let (source, spans) = extract_spans(source);
    if spans.len() != 1 {
        panic!("expected 1 span marker, got {}", spans.len());
    }
    (source, spans[0])
}

pub fn extract_single_pos(source: &str) -> (String, Pos) {
    let (source, spans) = extract_spans(source);
    if spans.len() != 1 {
        panic!("expected 1 span marker, got {}", spans.len());
    }
    if spans[0].start() != spans[0].end() {
        panic!("expected pos marker, got non-empty span");
    }
    (source, spans[0].start())
}
