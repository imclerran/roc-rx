module [
    RangeQuantifier,
    Negation,
    Character,
    CharacterRange,
    CharacterGroupItem,
]

RangeQuantifier : [ExactRange U64, LowerBounded U64, LowerAndUpperBounded (U64, U64)]

Negation : [Negated, NotNegated]

Character : [Char (U8)]

CharacterRange : [CharRange (U8, U8)]

CharacterGroupItem : [CharacterClass, CharacterClassFromUnicodeCategory, CharRange(U8, U8), Char(U8)]
