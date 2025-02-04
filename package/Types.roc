module [
    RangeQuantifier,
    QuantifierType,
    Quantifier,
    LazyModifier,
    Negation,
    Character,
    CharacterRange,
    CharacterGroupItem,
    CharacterGroup,
    CharacterClass,
    StartOfStringAnchor,
    Anchor,
    MatchCharacterClass,
    MatchItem,
    Match,
    GroupModifier,
    Group,
    Expression,
    # Subexpression,
]

RangeQuantifier : [
    ExactRange U64,
    LowerBounded U64,
    LowerAndUpperBounded (U64, U64),
]

QuantifierType : [
    ZeroOrMoreQuantifier,
    OneOrMoreQuantifier,
    ZeroOrOneQuantifier,
    ExactRange U64,
    LowerBounded U64,
    LowerAndUpperBounded (U64, U64),
]

Quantifier : { q : QuantifierType, modifier : LazyModifier }

LazyModifier : [Lazy, NotLazy]

Negation : [Negated, NotNegated]

Character : [Char U8]

CharacterRange : [CharRange (U8, U8)]

CharacterGroupItem : [
    CharacterClassFromUnicodeCategory,
    CharRange (U8, U8),
    Char U8,
    CharacterClassAnyWord,
    CharacterClassAnyWordInverted,
    CharacterClassAnyDecimalDigit,
    CharacterClassAnyDecimalDigitInverted,
    CharacterClassAnyWhitepace,
    CharacterClassAnyWhitespaceInverted,
]

CharacterGroup : { items : List CharacterGroupItem, negation : Negation }

StartOfStringAnchor : [StartOfStringAnchor, NotAnchored]

Anchor : [AnchorWordBoundary, AnchorNonWordBoundary]

CharacterClass : [
    CharacterClassAnyWord,
    CharacterClassAnyWordInverted,
    CharacterClassAnyDecimalDigit,
    CharacterClassAnyDecimalDigitInverted,
    CharacterClassAnyWhitepace,
    CharacterClassAnyWhitespaceInverted,
]

MatchCharacterClass : [
    CharacterGroup CharacterGroup,
    CharacterClassAnyWord,
    CharacterClassAnyWordInverted,
    CharacterClassAnyDecimalDigit,
    CharacterClassAnyDecimalDigitInverted,
    CharacterClassAnyWhitepace,
    CharacterClassAnyWhitespaceInverted,
]

MatchItem : [
    MatchAnyCharacter,
    CharacterGroup CharacterGroup,
    CharacterClassAnyWord,
    CharacterClassAnyWordInverted,
    CharacterClassAnyDecimalDigit,
    CharacterClassAnyDecimalDigitInverted,
    CharacterClassAnyWhitepace,
    CharacterClassAnyWhitespaceInverted,
    Char U8,
]

Match : { item : MatchItem, quantifier : [Quantifier Quantifier, NotQuantified] }

GroupModifier : [Capturing, NonCapturing]

Group : { 
    modifier : GroupModifier,
    expression : Expression, 
    quantifier : [Quantifier Quantifier, NotQuantified], 
}

Expression : [NotImplemented]

# Expression : {
#     subexpression : List [
#         Match Match,
#         Group Group,
#         AnchorWordBoundary,
#         AnchorNonWordBoundary,
#     ],
#     expression : [Expression Expression, NoExpression],
# }
