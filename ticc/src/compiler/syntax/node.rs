use super::{
    AstNode,
    SyntaxKind,
    SyntaxNode,
    SyntaxToken,
    child,
    child2,
    child3,
    children,
    child_token,
};

macro_rules! nodes {
    () => {};
    ($v:vis enum $name:ident { $($member:ident($t:ty),)* } $($rest:tt)*) => {
        $v enum $name {
            $($member($t),)*
        }

        impl AstNode for $name {
            fn cast(syntax: SyntaxNode) -> Option<Self> {
                $(
                    if let Some(x) = <$t>::cast(syntax.clone()) {
                        return Some(Self::$member(x));
                    }
                )*
                None
            }

            fn syntax(&self) -> &SyntaxNode {
                match self {
                    $(Self::$member(x) => x.syntax(),)*
                }
            }
        }

        nodes!($($rest)*);
    };
    
    ($v:vis struct $name:ident { $kind:expr } $($rest:tt)*) => {
        $v struct $name {
            $v syntax : SyntaxNode,
        }

        impl AstNode for $name {
            fn cast(syntax: SyntaxNode) -> Option<Self> {
                if syntax.kind() == $kind {
                    Some(Self { syntax })
                } else {
                    None
                }
            }

            fn syntax(&self) -> &SyntaxNode {
                &self.syntax
            }
        }

        nodes!($($rest)*);
    };
}

nodes! {
    pub(crate) enum Item {
        Value(ValueItem),
        Type(TypeItem),
    }

    pub(crate) struct TypeItem { SyntaxKind::TypeItem }

    pub(crate) struct TypeParams { SyntaxKind::TypeParams }
    pub(crate) struct TypeCase { SyntaxKind::TypeCase }

    pub(crate) enum Type {
        Int(IntType),
        Bool(BoolType),
        Fn(FnType),
        Named(NamedType),
        Rec(RecType),
        Paren(ParenType),
    }

    pub(crate) struct IntType { SyntaxKind::IntType }
    pub(crate) struct BoolType { SyntaxKind::BoolType }
    pub(crate) struct FnType { SyntaxKind::FnType }
    pub(crate) struct NamedType { SyntaxKind::NamedType }
    pub(crate) struct RecType { SyntaxKind::RecType }
    pub(crate) struct ParenType { SyntaxKind::ParenType }

    pub(crate) struct ValueItem { SyntaxKind::ValueItem }

    pub(crate) enum Expr {
        Name(NameExpr),
        Apply(ApplyExpr),
        Binary(BinaryExpr),
        Let(LetExpr),
        Match(MatchExpr),
        If(IfExpr),
        Bool(BoolExpr),
        Number(NumberExpr),
        Lambda(LambdaExpr),
        Paren(ParenExpr),
    }

    pub(crate) struct NameExpr { SyntaxKind::NameExpr }
    pub(crate) struct ApplyExpr { SyntaxKind::ApplyExpr }
    pub(crate) struct BinaryExpr { SyntaxKind::BinaryExpr }
    pub(crate) struct LetExpr { SyntaxKind::LetExpr }
    pub(crate) struct MatchExpr { SyntaxKind::MatchExpr }
    pub(crate) struct IfExpr { SyntaxKind::IfExpr }
    pub(crate) struct BoolExpr { SyntaxKind::BoolExpr }
    pub(crate) struct NumberExpr { SyntaxKind::NumberExpr }
    pub(crate) struct LambdaExpr { SyntaxKind::LambdaExpr }
    pub(crate) struct ParenExpr { SyntaxKind::ParenExpr }

    pub(crate) struct MatchCase { SyntaxKind::MatchCase }
    pub(crate) struct MatchVars { SyntaxKind::MatchVars }
    
    pub(crate) struct BinaryOp { SyntaxKind::BinaryOp }
}

impl TypeItem {
    pub(crate) fn type_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::TypeToken) }
    pub(crate) fn name_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::IdentToken) }
    pub(crate) fn params(&self) -> Option<TypeParams> { child(&self.syntax) }
    pub(crate) fn equals_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::EqualsToken) }
    pub(crate) fn cases(&self) -> impl Iterator<Item = TypeCase> { children(&self.syntax) }
    pub(crate) fn semi_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::SemicolonToken) }
}

impl TypeParams {
    pub(crate) fn params(&self) -> impl Iterator<Item = SyntaxToken> {
        self.syntax
            .children_with_tokens()
            .filter_map(|c| c.into_token())
            .filter(|c| c.kind() == SyntaxKind::IdentToken)
    }
}

impl TypeCase {
    pub(crate) fn pipe_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::PipeToken) }
    pub(crate) fn name_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::IdentToken) }
    pub(crate) fn types(&self) -> impl Iterator<Item = Type> { children(&self.syntax) }
}

impl IntType {
    pub(crate) fn token(&self) -> SyntaxToken { child_token(&self.syntax, SyntaxKind::IntToken).unwrap() }
}

impl BoolType {
    pub(crate) fn token(&self) -> SyntaxToken { child_token(&self.syntax, SyntaxKind::BoolToken).unwrap() }
}

impl FnType {
    pub(crate) fn from(&self) -> Option<Type> { child(&self.syntax) }
    pub(crate) fn arrow_token(&self) -> SyntaxToken { child_token(&self.syntax, SyntaxKind::ArrowToken).unwrap() }
    pub(crate) fn to(&self) -> Option<Type> { child2(&self.syntax) }
}

impl NamedType {
    pub(crate) fn name_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::IdentToken) }
    pub(crate) fn type_args(&self) -> impl Iterator<Item = Type> { children(&self.syntax) }
}

impl RecType {
    pub(crate) fn token(&self) -> SyntaxToken { child_token(&self.syntax, SyntaxKind::RecToken).unwrap() }
}

impl ParenType {
    pub(crate) fn left_paren_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::LeftParenToken) }
    pub(crate) fn inner(&self) -> Option<Type> { child(&self.syntax) }
    pub(crate) fn right_paren_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::RightParenToken) }
}

impl ValueItem {
    pub(crate) fn export_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::ExportToken) }
    pub(crate) fn let_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::LetToken) }
    pub(crate) fn name_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::IdentToken) }
    pub(crate) fn colon_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::ColonToken) }
    pub(crate) fn type_(&self) -> Option<Type> { child(&self.syntax) }
    pub(crate) fn eq_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::EqualsToken) }
    pub(crate) fn expr(&self) -> Option<Expr> { child(&self.syntax) }
    pub(crate) fn semi_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::SemicolonToken) }
}

impl NameExpr {
    pub(crate) fn name_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::IdentToken) }
}

impl ApplyExpr {
    pub(crate) fn function(&self) -> Option<Expr> { child(&self.syntax) }
    pub(crate) fn arg(&self) -> Option<Expr> { child2(&self.syntax) }
}

impl BinaryExpr {
    pub(crate) fn lhs(&self) -> Option<Expr> { child(&self.syntax) }
    pub(crate) fn op(&self) -> Option<BinaryOp> { child(&self.syntax) }
    pub(crate) fn rhs(&self) -> Option<Expr> { child2(&self.syntax) }
}

impl LetExpr {
    pub(crate) fn let_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::LetToken) }
    pub(crate) fn name_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::IdentToken) }
    pub(crate) fn colon_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::ColonToken) }
    pub(crate) fn type_(&self) -> Option<Type> { child(&self.syntax) }
    pub(crate) fn eq_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::EqualsToken) }
    pub(crate) fn value(&self) -> Option<Expr> { child(&self.syntax) }
    pub(crate) fn semi_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::SemicolonToken) }
    pub(crate) fn rest(&self) -> Option<Expr> { child2(&self.syntax) }
}

impl MatchExpr {
    pub(crate) fn match_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::MatchToken) }
    pub(crate) fn discr(&self) -> Option<Expr> { child(&self.syntax) }
    pub(crate) fn with_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::WithToken) }
    pub(crate) fn cases(&self) -> impl Iterator<Item = MatchCase> { children(&self.syntax) }
    pub(crate) fn end_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::EndToken) }
}

impl IfExpr {
    pub(crate) fn if_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::IfToken) }
    pub(crate) fn cond(&self) -> Option<Expr> { child(&self.syntax) }
    pub(crate) fn then_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::ThenToken) }
    pub(crate) fn then_value(&self) -> Option<Expr> { child2(&self.syntax) }
    pub(crate) fn else_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::ElseToken) }
    pub(crate) fn else_value(&self) -> Option<Expr> { child3(&self.syntax) }
}

impl BoolExpr {
    pub(crate) fn token(&self) -> SyntaxToken {
        self.syntax.children_with_tokens().find_map(|c| c.into_token()).unwrap()
    }
}

impl NumberExpr {
    pub(crate) fn token(&self) -> SyntaxToken {
        self.syntax.children_with_tokens().find_map(|c| c.into_token()).unwrap()
    }
}

impl LambdaExpr {
    pub(crate) fn lambda_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::BackslashToken) }
    pub(crate) fn fold_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::FoldToken) }
    pub(crate) fn is_fold(&self) -> bool { self.fold_token().is_some() }
    pub(crate) fn param_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::IdentToken) }
    pub(crate) fn arrow_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::ArrowToken) }
    pub(crate) fn body(&self) -> Option<Expr> { child(&self.syntax) }
}

impl ParenExpr {
    pub(crate) fn left_paren_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::LeftParenToken) }
    pub(crate) fn inner(&self) -> Option<Expr> { child(&self.syntax) }
    pub(crate) fn right_paren_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::RightParenToken) }
}

impl MatchCase {
    pub(crate) fn pipe_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::PipeToken) }
    pub(crate) fn ctor_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::IdentToken) }
    pub(crate) fn vars(&self) -> Option<MatchVars> { child(&self.syntax) }
    pub(crate) fn arrow_token(&self) -> Option<SyntaxToken> { child_token(&self.syntax, SyntaxKind::ArrowToken) }
    pub(crate) fn body(&self) -> Option<Expr> { child(&self.syntax) }
}

impl MatchVars {
    pub(crate) fn vars(&self) -> impl Iterator<Item = SyntaxToken> {
        self.syntax
            .children_with_tokens()
            .filter_map(|c| c.into_token())
            .filter(|c| c.kind() == SyntaxKind::IdentToken)
    }
}

impl BinaryOp {
    pub(crate) fn token(&self) -> SyntaxToken {
        self.syntax.children_with_tokens().find_map(|c| c.into_token()).unwrap()
    }
}
