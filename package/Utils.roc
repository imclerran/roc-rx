module [is_digit, quantify, modify, negate, lazy]

import Types exposing [Quantifier, GroupModifier, Negation, LazyModifier]

Maybe a : [Some a, None]

is_digit : U8 -> Bool
is_digit = |c| c >= '0' and c <= '9'

quantify : Maybe Quantifier -> [Quantifier Quantifier, NotQuantified]
quantify = |maybe_quantifier|
    when maybe_quantifier is
        Some(q) -> Quantifier(q)
        None -> NotQuantified

modify : Maybe GroupModifier -> GroupModifier
modify = |maybe_modifier|
    when maybe_modifier is
        Some(m) -> m
        None -> NonCapturing

negate : Maybe Negation -> Negation
negate = |maybe_negation|
    when maybe_negation is
        Some(n) -> n
        None -> NotNegated

lazy : Maybe LazyModifier -> LazyModifier
lazy = |maybe_lazy|
    when maybe_lazy is
        Some(l) -> l
        None -> NotLazy
