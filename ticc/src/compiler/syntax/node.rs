#![allow(unused)]

use super::{
    AstNode,
    SyntaxKind,
    SyntaxNode,
    SyntaxToken,
    TokenKind,
    child,
    child2,
    child3,
    children,
    child_token,
};

macro_rules! nodes {
    () => {};
    ($v:vis enum $name:ident { $($member:ident($t:ident),)* } $($rest:tt)*) => {
        #[derive(Clone, Copy)]
        $v enum $name<'a> {
            $($member($t<'a>),)*
        }

        impl<'a> AstNode<'a> for $name<'a> {
            fn cast(syntax: SyntaxNode<'a>) -> Option<Self> {
                $(
                    if let Some(x) = <$t>::cast(syntax.clone()) {
                        return Some(Self::$member(x));
                    }
                )*
                None
            }

            fn syntax(&self) -> &SyntaxNode<'a> {
                match self {
                    $(Self::$member(x) => x.syntax(),)*
                }
            }
        }

        nodes!($($rest)*);
    };
    
    ($v:vis struct $name:ident { $kind:expr } $($rest:tt)*) => {
        #[derive(Clone, Copy)]
        $v struct $name<'a> {
            $v syntax : SyntaxNode<'a>,
        }

        impl<'a> AstNode<'a> for $name<'a> {
            fn cast(syntax: SyntaxNode<'a>) -> Option<Self> {
                if syntax.kind() == $kind {
                    Some(Self { syntax })
                } else {
                    None
                }
            }

            fn syntax(&self) -> &SyntaxNode<'a> {
                &self.syntax
            }
        }

        nodes!($($rest)*);
    };
}

nodes! {
    pub(crate) enum Item {
        Import(ImportItem),
        Value(ValueItem),
        Type(TypeItem),
    }

    pub(crate) struct ImportItem { SyntaxKind::ImportItem }
    pub(crate) struct ExposedList { SyntaxKind::ExposedList }

    pub(crate) struct TypeItem { SyntaxKind::TypeItem }

    pub(crate) struct TypeParams { SyntaxKind::TypeParams }
    pub(crate) struct TypeCase { SyntaxKind::TypeCase }

    pub(crate) struct RecField { SyntaxKind::RecField }
    pub(crate) struct TypeField { SyntaxKind::TypeField }

    pub(crate) enum Field {
        Rec(RecField),
        Type(TypeField),
    }

    pub(crate) enum Type {
        Int(IntType),
        Bool(BoolType),
        String(StringType),
        Fn(FnType),
        Named(NamedType),
        Paren(ParenType),
        Err(ErrType),
    }

    pub(crate) struct IntType { SyntaxKind::IntType }
    pub(crate) struct BoolType { SyntaxKind::BoolType }
    pub(crate) struct StringType { SyntaxKind::StringType }
    pub(crate) struct FnType { SyntaxKind::FnType }
    pub(crate) struct NamedType { SyntaxKind::NamedType }
    pub(crate) struct ParenType { SyntaxKind::ParenType }
    pub(crate) struct ErrType { SyntaxKind::ErrType }

    pub(crate) struct ValueItem { SyntaxKind::ValueItem }

    pub(crate) enum Expr {
        Name(NameExpr),
        NamespacedName(NamespacedNameExpr),
        Apply(ApplyExpr),
        Binary(BinaryExpr),
        Let(LetExpr),
        Match(MatchExpr),
        If(IfExpr),
        Bool(BoolExpr),
        Number(NumberExpr),
        String(StringExpr),
        Lambda(LambdaExpr),
        Paren(ParenExpr),
        Hole(HoleExpr),
    }

    pub(crate) struct NameExpr { SyntaxKind::NameExpr }
    pub(crate) struct NamespacedNameExpr { SyntaxKind::NamespacedNameExpr }
    pub(crate) struct ApplyExpr { SyntaxKind::ApplyExpr }
    pub(crate) struct BinaryExpr { SyntaxKind::BinaryExpr }
    pub(crate) struct LetExpr { SyntaxKind::LetExpr }
    pub(crate) struct MatchExpr { SyntaxKind::MatchExpr }
    pub(crate) struct IfExpr { SyntaxKind::IfExpr }
    pub(crate) struct BoolExpr { SyntaxKind::BoolExpr }
    pub(crate) struct NumberExpr { SyntaxKind::NumberExpr }
    pub(crate) struct StringExpr { SyntaxKind::StringExpr }
    pub(crate) struct LambdaExpr { SyntaxKind::LambdaExpr }
    pub(crate) struct ParenExpr { SyntaxKind::ParenExpr }
    pub(crate) struct HoleExpr { SyntaxKind::HoleExpr }

    pub(crate) struct MatchCase { SyntaxKind::MatchCase }
    pub(crate) struct MatchVars { SyntaxKind::MatchVars }
    
    pub(crate) struct BinaryOp { SyntaxKind::BinaryOp }

    pub(crate) struct Name { SyntaxKind::Name }
}

impl std::fmt::Debug for Name<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Name").field("text", &self.syntax().text()).finish()
    }
}

impl<'a> ImportItem<'a> {
    pub(crate) fn import_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::Import) }
    pub(crate) fn name(&self) -> Option<Name<'a>> { child(&self.syntax) }
    pub(crate) fn exposed_list(&self) -> Option<ExposedList<'a>> { child(&self.syntax) }
    pub(crate) fn from_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::From) }
    pub(crate) fn path(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::String) }
    pub(crate) fn semi_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::Semicolon) }
}

impl<'a> ExposedList<'a> {
    pub(crate) fn left_paren_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::LeftParen) }
    pub(crate) fn imported_names(&self) -> impl Iterator<Item = Name<'a>> + 'a { children(&self.syntax) }
    pub(crate) fn right_paren_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::RightParen) }
}

impl<'a> TypeItem<'a> {
    pub(crate) fn export_token(&self) -> Option<SyntaxToken<'a>> { type_export_token(&self.syntax, true) }
    pub(crate) fn type_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::Type) }
    pub(crate) fn name(&self) -> Option<Name<'a>> { child(&self.syntax) }
    pub(crate) fn params(&self) -> Option<TypeParams<'a>> { child(&self.syntax) }
    pub(crate) fn equals_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::Equals) }
    pub(crate) fn export_cases_token(&self) -> Option<SyntaxToken<'a>> { type_export_token(&self.syntax, false) }
    pub(crate) fn cases(&self) -> impl Iterator<Item = TypeCase<'a>> + 'a { children(&self.syntax) }
    pub(crate) fn semi_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::Semicolon) }
}

fn type_export_token<'a>(node: &SyntaxNode<'a>, before_eq: bool) -> Option<SyntaxToken<'a>> {
    if before_eq {
        node.all_children()
            .filter_map(|c| c.into_token())
            .take_while(|t| t.kind() != TokenKind::Equals)
            .find(|t| t.kind() == TokenKind::Export)
    } else {
        node.all_children()
            .filter_map(|c| c.into_token())
            .skip_while(|t| t.kind() != TokenKind::Equals)
            .find(|t| t.kind() == TokenKind::Export)
    }
}

impl<'a> TypeParams<'a> {
    pub(crate) fn params(&self) -> impl Iterator<Item = Name<'a>> + 'a {
        children(&self.syntax)
    }
}

impl<'a> TypeCase<'a> {
    pub(crate) fn pipe_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::Pipe) }
    pub(crate) fn name(&self) -> Option<Name<'a>> { child(&self.syntax) }
    pub(crate) fn fields(&self) -> impl Iterator<Item = Field<'a>> + 'a { children(&self.syntax) }
}

impl<'a> RecField<'a> {
    pub(crate) fn rec_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::Rec) }
}

impl<'a> TypeField<'a> {
    pub(crate) fn ty(&self) -> Option<Type<'a>> { child(&self.syntax) }
}

impl<'a> IntType<'a> {
    pub(crate) fn token(&self) -> SyntaxToken { child_token(&self.syntax, TokenKind::Int).unwrap() }
}

impl<'a> BoolType<'a> {
    pub(crate) fn token(&self) -> SyntaxToken { child_token(&self.syntax, TokenKind::Bool).unwrap() }
}

impl<'a> StringType<'a> {
    pub(crate) fn token(&self) -> SyntaxToken { child_token(&self.syntax, TokenKind::StringTy).unwrap() }
}

impl<'a> FnType<'a> {
    pub(crate) fn from(&self) -> Option<Type<'a>> { child(&self.syntax) }
    pub(crate) fn arrow_token(&self) -> SyntaxToken { child_token(&self.syntax, TokenKind::Arrow).unwrap() }
    pub(crate) fn to(&self) -> Option<Type<'a>> { child2(&self.syntax) }
}

impl<'a> NamedType<'a> {
    pub(crate) fn namespace(&self) -> Option<Name<'a>> { if self.dot().is_some() { child(&self.syntax) } else { None } }
    pub(crate) fn dot(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::Dot) }
    pub(crate) fn name(&self) -> Option<Name<'a>> { if self.dot().is_some() { child2(&self.syntax) } else { child(&self.syntax) } }
    pub(crate) fn type_args(&self) -> impl Iterator<Item = Type<'a>> + 'a { children(&self.syntax) }
}

impl<'a> ParenType<'a> {
    pub(crate) fn left_paren_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::LeftParen) }
    pub(crate) fn inner(&self) -> Option<Type<'a>> { child(&self.syntax) }
    pub(crate) fn right_paren_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::RightParen) }
}

impl<'a> ValueItem<'a> {
    pub(crate) fn export_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::Export) }
    pub(crate) fn let_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::Let) }
    pub(crate) fn name(&self) -> Option<Name<'a>> { child(&self.syntax) }
    pub(crate) fn colon_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::Colon) }
    pub(crate) fn type_(&self) -> Option<Type<'a>> { child(&self.syntax) }
    pub(crate) fn eq_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::Equals) }
    pub(crate) fn expr(&self) -> Option<Expr<'a>> { child(&self.syntax) }
    pub(crate) fn semi_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::Semicolon) }
}

impl<'a> NameExpr<'a> {
    pub(crate) fn name(&self) -> Option<Name<'a>> { child(&self.syntax) }
}

impl<'a> NamespacedNameExpr<'a> {
    pub(crate) fn namespace(&self) -> Option<Name<'a>> { child(&self.syntax) }
    pub(crate) fn dot(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::Dot) }
    pub(crate) fn name(&self) -> Option<Name<'a>> { child2(&self.syntax) }
}

impl<'a> ApplyExpr<'a> {
    pub(crate) fn function(&self) -> Option<Expr<'a>> { child(&self.syntax) }
    pub(crate) fn arg(&self) -> Option<Expr<'a>> { child2(&self.syntax) }
}

impl<'a> BinaryExpr<'a> {
    pub(crate) fn lhs(&self) -> Option<Expr<'a>> { child(&self.syntax) }
    pub(crate) fn op(&self) -> Option<BinaryOp<'a>> { child(&self.syntax) }
    pub(crate) fn rhs(&self) -> Option<Expr<'a>> { child2(&self.syntax) }
}

impl<'a> LetExpr<'a> {
    pub(crate) fn let_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::Let) }
    pub(crate) fn name(&self) -> Option<Name<'a>> { child(&self.syntax) }
    pub(crate) fn colon_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::Colon) }
    pub(crate) fn type_(&self) -> Option<Type<'a>> { child(&self.syntax) }
    pub(crate) fn eq_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::Equals) }
    pub(crate) fn value(&self) -> Option<Expr<'a>> { child(&self.syntax) }
    pub(crate) fn semi_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::Semicolon) }
    pub(crate) fn rest(&self) -> Option<Expr<'a>> { child2(&self.syntax) }
}

impl<'a> MatchExpr<'a> {
    pub(crate) fn match_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::Match) }
    pub(crate) fn discr(&self) -> Option<Expr<'a>> { child(&self.syntax) }
    pub(crate) fn with_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::With) }
    pub(crate) fn cases(&self) -> impl Iterator<Item = MatchCase<'a>> + 'a { children(&self.syntax) }
    pub(crate) fn end_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::End) }
}

impl<'a> IfExpr<'a> {
    pub(crate) fn if_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::If) }
    pub(crate) fn cond(&self) -> Option<Expr<'a>> { child(&self.syntax) }
    pub(crate) fn then_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::Then) }
    pub(crate) fn then_value(&self) -> Option<Expr<'a>> { child2(&self.syntax) }
    pub(crate) fn else_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::Else) }
    pub(crate) fn else_value(&self) -> Option<Expr<'a>> { child3(&self.syntax) }
}

impl<'a> BoolExpr<'a> {
    pub(crate) fn token(&self) -> SyntaxToken {
        self.syntax.all_children().find_map(|c| c.into_token()).unwrap()
    }
}

impl<'a> NumberExpr<'a> {
    pub(crate) fn token(&self) -> SyntaxToken {
        self.syntax.all_children().find_map(|c| c.into_token()).unwrap()
    }
}

impl<'a> StringExpr<'a> {
    pub(crate) fn token(&self) -> SyntaxToken {
        self.syntax.all_children().find_map(|c| c.into_token()).unwrap()
    }
}

impl<'a> LambdaExpr<'a> {
    pub(crate) fn lambda_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::Backslash) }
    pub(crate) fn fold_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::Fold) }
    pub(crate) fn is_fold(&self) -> bool { self.fold_token().is_some() }
    pub(crate) fn param(&self) -> Option<Name<'a>> { child(&self.syntax) }
    pub(crate) fn arrow_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::Arrow) }
    pub(crate) fn body(&self) -> Option<Expr<'a>> { child(&self.syntax) }
}

impl<'a> ParenExpr<'a> {
    pub(crate) fn left_paren_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::LeftParen) }
    pub(crate) fn inner(&self) -> Option<Expr<'a>> { child(&self.syntax) }
    pub(crate) fn right_paren_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::RightParen) }
}

impl<'a> HoleExpr<'a> {
    pub(crate) fn token(&self) -> SyntaxToken<'a> { child_token(&self.syntax, TokenKind::Hole).unwrap() }
}

impl<'a> MatchCase<'a> {
    pub(crate) fn pipe_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::Pipe) }
    pub(crate) fn namespace(&self) -> Option<Name<'a>> { if self.dot().is_some() { child(&self.syntax) } else { None } }
    pub(crate) fn dot(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::Dot) }
    pub(crate) fn ctor(&self) -> Option<Name<'a>> { if self.dot().is_some() { child2(&self.syntax) } else { child(&self.syntax) } }
    pub(crate) fn vars(&self) -> Option<MatchVars<'a>> { child(&self.syntax) }
    pub(crate) fn arrow_token(&self) -> Option<SyntaxToken<'a>> { child_token(&self.syntax, TokenKind::Arrow) }
    pub(crate) fn body(&self) -> Option<Expr<'a>> { child(&self.syntax) }
}

impl<'a> MatchVars<'a> {
    pub(crate) fn vars(&self) -> impl Iterator<Item = Name<'a>> + 'a {
        children(&self.syntax)
    }
}

impl<'a> BinaryOp<'a> {
    pub(crate) fn token(&self) -> SyntaxToken<'a> {
        self.syntax
            .all_children()
            .find_map(|c| c.into_token())
            .unwrap()
    }
}

impl<'a> Name<'a> {
    pub(crate) fn token(&self) -> SyntaxToken<'a> {
        self.syntax
            .all_children()
            .find_map(|c| c.into_token())
            .unwrap()
    }
}
