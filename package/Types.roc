module [
    RangeQuantifier,
    Negation,
    Character,
    CharacterRange,
    CharacterGroupItem,
    CharacterClass,
]

RangeQuantifier : [ExactRange U64, LowerBounded U64, LowerAndUpperBounded (U64, U64)]

Negation : [Negated, NotNegated]

Character : [Char (U8)]

CharacterRange : [CharRange (U8, U8)]

CharacterGroupItem : [CharacterClassFromUnicodeCategory, CharRange(U8, U8), Char(U8), CharacterClassAnyWord, CharacterClassAnyWordInverted, CharacterClassAnyDecimalDigit, CharacterClassAnyDecimalDigitInverted]

CharacterClass : [CharacterClassAnyWord, CharacterClassAnyWordInverted, CharacterClassAnyDecimalDigit, CharacterClassAnyDecimalDigitInverted]
