module [
    RangeQuantifier,
    QuantifierType,
    Quantifier,
    LazyModifier,
    Negation,
    Character,
    CharacterRange,
    CharacterGroupItem,
    CharacterClass,
    StartOfStringAnchor,
    Anchor,
    MatchCharacterClass,
    MatchItem,
    Match,
]

RangeQuantifier : [ExactRange U64, LowerBounded U64, LowerAndUpperBounded (U64, U64)]

QuantifierType : [ZeroOrMoreQuantifier, OneOrMoreQuantifier, ZeroOrOneQuantifier, ExactRange U64, LowerBounded U64, LowerAndUpperBounded (U64, U64)]

Quantifier : (QuantifierType, LazyModifier)

LazyModifier : [Lazy, NotLazy]

Negation : [Negated, NotNegated]

Character : [Char (U8)]

CharacterRange : [CharRange (U8, U8)]

CharacterGroupItem : [CharacterClassFromUnicodeCategory, CharRange(U8, U8), Char(U8), CharacterClassAnyWord, CharacterClassAnyWordInverted, CharacterClassAnyDecimalDigit, CharacterClassAnyDecimalDigitInverted]

CharacterClass : [CharacterClassAnyWord, CharacterClassAnyWordInverted, CharacterClassAnyDecimalDigit, CharacterClassAnyDecimalDigitInverted]

StartOfStringAnchor : [StartOfStringAnchor, NotAnchored]

Anchor : [AnchorWordBoundary, AnchorNonWordBoundary, AnchorStartOfStringOnly, AnchorEndOfStringOnlyNotNewline, AnchorEndOfStringOnly, AnchorPreviousMatchEnd, AnchorEndOfString, NotAnchored]

MatchCharacterClass : [CharacterClass(CharacterClass), CharacterGroup(CharacterGroupItem)]

MatchItem : [MatchAnyCharacter, MatchCharacterClass(MatchCharacterClass), MatchCharacter(Character)]

Match : (MatchItem, [Quantifier(Quantifier), NotQuantified])