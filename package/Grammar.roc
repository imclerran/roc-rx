module []

import Parser exposing [Parser, map, maybe, lhs, rhs, both, string, number, char, one_of, excluding, one_or_more]
import Types exposing [RangeQuantifier, QuantifierType, Quantifier, LazyModifier, Negation, CharacterGroupItem, CharacterRange, Character, CharacterClass, StartOfStringAnchor, Anchor]

## RangeQuantifier ::= "{" RangeQuantifierLowerBound ( "," RangeQuantifierUpperBound? )? "}"
## RangeQuantifierLowerBound ::= Integer
## RangeQuantifierUpperBound ::= Integer
range_quantifier : Parser RangeQuantifier [InvalidRangeQuantifier]
range_quantifier = |str|
    pattern = string("{") |> rhs(number) |> both(maybe(string(",") |> rhs(maybe(number)))) |> lhs(string("}"))
    parser =
        pattern
        |> map(
            |(min, maybe_comma_max)|
                when maybe_comma_max is
                    None -> Ok(ExactRange(min))
                    Some(None) -> Ok(LowerBounded(min))
                    Some(Some(max)) -> Ok(LowerAndUpperBounded((min, max))),
        )
    parser(str) |> Result.map_err(|_| InvalidRangeQuantifier)

expect range_quantifier("{1}") == Ok((ExactRange(1), ""))
expect range_quantifier("{1,}") == Ok((LowerBounded(1), ""))
expect range_quantifier("{1,2}") == Ok((LowerAndUpperBounded((1, 2)), ""))
expect range_quantifier("{123,456}") == Ok((LowerAndUpperBounded((123, 456)), ""))
expect range_quantifier("{1,2}abc") == Ok((LowerAndUpperBounded((1, 2)), "abc"))
expect range_quantifier("{") == Err(InvalidRangeQuantifier)
expect range_quantifier("{1,2") == Err(InvalidRangeQuantifier)
expect range_quantifier("{1,2,}") == Err(InvalidRangeQuantifier)

## CharacterGroupNegativeModifier ::= "^"
character_group_negative_modifier : Parser Negation [InvalidCharacterGroupNegativeModifier]
character_group_negative_modifier = |str|
    parser = string("^") |> map(|_carrot| Ok(Negated))
    parser(str) |> Result.map_err(|_| InvalidCharacterGroupNegativeModifier)

expect character_group_negative_modifier("^") == Ok((Negated, ""))

character_excluding_escaped : Parser Character [CharNotFound]
character_excluding_escaped = |str|
    excluded_characters = ['.', '^', '$', '*', '+', '?', '(', ')', '[', ']', '{', '}', '|', '\\', '/', '-', ' ']
    parser = char |> excluding(|c| List.contains(excluded_characters, c)) |> map(|c| Ok(Char(c)))
    parser(str) |> Result.map_err(|_| CharNotFound)

expect character_excluding_escaped("a") == Ok((Char('a'), ""))
expect character_excluding_escaped(" ") == Err(CharNotFound)

escaped_reserved_character : Parser Character [EscapedCharNotFound]
escaped_reserved_character = |str|
    reserved_character_strs =
        ["\\.", "\\^", "\\$", "\\*", "\\+", "\\?", "\\(", "\\)", "\\[", "\\]", "\\{", "\\}", "\\|", "\\\\", "\\/", "\\-", "\\ "]
    pattern = one_of(List.map(reserved_character_strs, string))
    parser = pattern |> map(|s| s |> Str.to_utf8 |> List.get(1) |> Result.map_ok(|c| Char(c)))
    parser(str) |> Result.map_err(|_| EscapedCharNotFound)

expect escaped_reserved_character("\\\\") == Ok((Char('\\'), ""))
expect escaped_reserved_character("\\+") == Ok((Char('+'), ""))

## Character ::= Char | EscapedReservedCharacter
character : Parser Character [CharNotFound]
character = |str|
    parser = one_of([character_excluding_escaped, escaped_reserved_character])
    parser(str) |> Result.map_err(|_| CharNotFound)

expect character("a") == Ok((Char('a'), ""))
expect character("\\+") == Ok((Char('+'), ""))

## CharacterRange ::= Char "-" Char
character_range : Parser CharacterRange [InvalidCharacterRange]
character_range = |str|
    pattern = character |> lhs(string("-")) |> both(character) 
    parser = 
        pattern 
        |> map(|(Char(start), Char(end))| if start > end then Err(InvalidCharacterRange) else Ok(CharRange((start, end))))
    parser(str) |> Result.map_err(|_| InvalidCharacterRange)

expect character_range("a-b") == Ok((CharRange((97, 98)), ""))
expect character_range("a-") == Err(InvalidCharacterRange)
expect character_range("\\+-\\-") == Ok((CharRange(('+', '-')), ""))
expect character_range("b-a") == Err(InvalidCharacterRange)

## CharacterClassAnyWord ::= "\w"
character_class_any_word : Parser CharacterClass [InvalidCharacterClass]
character_class_any_word = |str|
    parser = string("\\w") |> map(|_| Ok(CharacterClassAnyWord))
    parser(str) |> Result.map_err(|_| InvalidCharacterClass)

expect character_class_any_word("\\w") == Ok((CharacterClassAnyWord, ""))

## CharacterClassAnyWordInverted ::= "\W"
character_class_any_word_inverted : Parser CharacterClass [InvalidCharacterClass]
character_class_any_word_inverted = |str|
    parser = string("\\W") |> map(|_| Ok(CharacterClassAnyWordInverted))
    parser(str) |> Result.map_err(|_| InvalidCharacterClass)

expect character_class_any_word_inverted("\\W") == Ok((CharacterClassAnyWordInverted, ""))

## CharacterClassAnyDecimalDigit ::= "\d"
character_class_any_decimal_digit : Parser CharacterClass [InvalidCharacterClass]
character_class_any_decimal_digit = |str|
    parser = string("\\d") |> map(|_| Ok(CharacterClassAnyDecimalDigit))
    parser(str) |> Result.map_err(|_| InvalidCharacterClass)

expect character_class_any_decimal_digit("\\d") == Ok((CharacterClassAnyDecimalDigit, ""))

## CharacterClassAnyDecimalDigitInverted ::= "\D"
character_class_any_decimal_digit_inverted : Parser CharacterClass [InvalidCharacterClass]
character_class_any_decimal_digit_inverted = |str|
    parser = string("\\D") |> map(|_| Ok(CharacterClassAnyDecimalDigitInverted))
    parser(str) |> Result.map_err(|_| InvalidCharacterClass)

expect character_class_any_decimal_digit_inverted("\\D") == Ok((CharacterClassAnyDecimalDigitInverted, ""))

## CharacterClass ::= CharacterClassAnyWord | CharacterClassAnyWordInverted | CharacterClassAnyDecimalDigit | CharacterClassAnyDecimalDigitInverted
character_class : Parser CharacterClass [InvalidCharacterClass]
character_class = |str|
    parser = one_of([character_class_any_word, character_class_any_word_inverted, character_class_any_decimal_digit, character_class_any_decimal_digit_inverted])
    parser(str) |> Result.map_err(|_| InvalidCharacterClass)

## CharacterGroupItem ::= CharacterRange | CharacterClass | Character
character_group_item : Parser CharacterGroupItem [InvalidCharacterGroupItem]
character_group_item = |str|
    parser = one_of([character_range, character_class, character, ])
    parser(str) |> Result.map_err(|_| InvalidCharacterGroupItem)

expect character_group_item("a") == Ok((Char('a'), ""))
expect character_group_item("a-b") == Ok((CharRange(('a', 'b')), ""))
expect character_group_item("\\w") == Ok((CharacterClassAnyWord, ""))

## CharacterGroup ::= "[" CharacterGroupNegativeModifier? CharacterGroupItem+ "]"
character_group : Parser (Negation, List CharacterGroupItem) [InvalidCharacterGroup]
character_group = |str|
    pattern =
        string("[")
        |> rhs(maybe(character_group_negative_modifier))
        |> both(one_or_more(one_of([character_group_item])))
        |> lhs(string("]"))
    parser =
        pattern 
        |> map(|(maybe_negation, items)|
            when maybe_negation is 
                Some(_) -> Ok((Negated, items))
                None -> Ok((NotNegated, items))
        )
    parser(str) |> Result.map_err(|_| InvalidCharacterGroup)

expect character_group("[a]") == Ok ((NotNegated, [Char 'a']), "")
expect character_group("[a-b]") == Ok ((NotNegated, [CharRange ('a', 'b')]), "")
expect character_group("[^a]") == Ok ((Negated, [Char 'a']), "")
expect character_group("[\\]]") == Ok ((NotNegated, [Char ']']), "")
expect character_group("[\\w\\W\\d\\D]") == Ok ((NotNegated, [CharacterClassAnyWord, CharacterClassAnyWordInverted, CharacterClassAnyDecimalDigit, CharacterClassAnyDecimalDigitInverted]), "")
expect character_group("[^a\\+-\\-b]") == Ok ((Negated, [Char 'a', CharRange(('+', '-')), Char 'b']), "")
expect character_group("[]b]") == Err(InvalidCharacterGroup)

## ZeroOrMoreQuantifier ::= "*"
zero_or_more_quantifier : Parser QuantifierType [InvalidQuantifierType]
zero_or_more_quantifier = |str|
    parser = string("*") |> map(|_| Ok(ZeroOrMoreQuantifier))
    parser(str) |> Result.map_err(|_| InvalidQuantifierType)

expect zero_or_more_quantifier("*") == Ok((ZeroOrMoreQuantifier, ""))

## OneOrMoreQuantifier ::= "+"
one_or_more_quantifier : Parser QuantifierType [InvalidQuantifierType]
one_or_more_quantifier = |str|
    parser = string("+") |> map(|_| Ok(OneOrMoreQuantifier))
    parser(str) |> Result.map_err(|_| InvalidQuantifierType)

expect one_or_more_quantifier("+") == Ok((OneOrMoreQuantifier, ""))

## ZeroOrOneQuantifier ::= "?"
zero_or_one_quantifier : Parser QuantifierType [InvalidQuantifierType]
zero_or_one_quantifier = |str|
    parser = string("?") |> map(|_| Ok(ZeroOrOneQuantifier))
    parser(str) |> Result.map_err(|_| InvalidQuantifierType)

expect zero_or_one_quantifier("?") == Ok((ZeroOrOneQuantifier, ""))

## LazyModifier ::= "?"
lazy_modifier : Parser LazyModifier [InvalidLazyModifier]
lazy_modifier = |str|
    parser = string("?") |> map(|_| Ok(Lazy))
    parser(str) |> Result.map_err(|_| InvalidLazyModifier)

quantifier_type : Parser QuantifierType [InvalidQuantifierType]
quantifier_type = |str|
    parser = one_of([zero_or_more_quantifier, one_or_more_quantifier, zero_or_one_quantifier, range_quantifier])
    parser(str) |> Result.map_err(|_| InvalidQuantifierType)

## Quantifier ::= QuantifierType LazyModifier?
quantifier : Parser Quantifier [InvalidQuantifier]
quantifier = |str|
    pattern = quantifier_type |> both(maybe(lazy_modifier))
    parser =
        pattern
        |> map(|(q, maybe_lazy)|
            when maybe_lazy is
                Some(_) -> Ok((q, Lazy))
                None -> Ok((q, NotLazy))
        )
    parser(str) |> Result.map_err(|_| InvalidQuantifier)

expect quantifier("*") == Ok(((ZeroOrMoreQuantifier, NotLazy), ""))
expect quantifier("+") == Ok(((OneOrMoreQuantifier, NotLazy), ""))
expect quantifier("?") == Ok(((ZeroOrOneQuantifier, NotLazy), ""))
expect quantifier("{1}") == Ok(((ExactRange(1), NotLazy), ""))
expect quantifier("{1,}") == Ok(((LowerBounded(1), NotLazy), ""))
expect quantifier("{1,2}") == Ok(((LowerAndUpperBounded((1, 2)), NotLazy), ""))
expect quantifier("??") == Ok(((ZeroOrOneQuantifier, Lazy), ""))

## StartOfStringAnchor ::= "^"
start_of_string_anchor : Parser StartOfStringAnchor [InvalidStartOfStringAnchor]
start_of_string_anchor = |str|
    parser = string("^") |> map(|_| Ok(StartOfStringAnchor))
    parser(str) |> Result.map_err(|_| InvalidStartOfStringAnchor)

expect start_of_string_anchor("^") == Ok((StartOfStringAnchor, ""))

## Anchor ::= "\b"
anchor_word_boundary : Parser Anchor [InvalidAnchor]
anchor_word_boundary = |str|
    parser = string("\\b") |> map(|_| Ok(AnchorWordBoundary))
    parser(str) |> Result.map_err(|_| InvalidAnchor)

## Anchor ::= "\B"
anchor_non_word_boundary : Parser Anchor [InvalidAnchor]
anchor_non_word_boundary = |str|
    parser = string("\\B") |> map(|_| Ok(AnchorNonWordBoundary))
    parser(str) |> Result.map_err(|_| InvalidAnchor)

## Anchor ::= "\A"
anchor_start_of_string_only : Parser Anchor [InvalidAnchor]
anchor_start_of_string_only = |str|
    parser = string("\\A") |> map(|_| Ok(AnchorStartOfStringOnly))
    parser(str) |> Result.map_err(|_| InvalidAnchor)

## Anchor ::= "\Z"
anchor_end_of_string_only_not_newline : Parser Anchor [InvalidAnchor]
anchor_end_of_string_only_not_newline = |str|
    parser = string("\\z") |> map(|_| Ok(AnchorEndOfStringOnlyNotNewline))
    parser(str) |> Result.map_err(|_| InvalidAnchor)

## Anchor ::= "\Z"
anchor_end_of_string_only : Parser Anchor [InvalidAnchor]
anchor_end_of_string_only = |str|
    parser = string("\\Z") |> map(|_| Ok(AnchorEndOfStringOnly))
    parser(str) |> Result.map_err(|_| InvalidAnchor)

## Anchor ::= "\G"
anchor_previous_match_end : Parser Anchor [InvalidAnchor]
anchor_previous_match_end = |str|
    parser = string("\\G") |> map(|_| Ok(AnchorPreviousMatchEnd))
    parser(str) |> Result.map_err(|_| InvalidAnchor)

## Anchor ::= "\Z"
anchor_end_of_string : Parser Anchor [InvalidAnchor]
anchor_end_of_string = |str|
    parser = string("\\$") |> map(|_| Ok(AnchorEndOfString))
    parser(str) |> Result.map_err(|_| InvalidAnchor)

# Anchor
#     ::= AnchorWordBoundary
#       | AnchorNonWordBoundary
#       | AnchorStartOfStringOnly
#       | AnchorEndOfStringOnlyNotNewline
#       | AnchorEndOfStringOnly
#       | AnchorPreviousMatchEnd
#       | AnchorEndOfString
anchor : Parser Anchor [InvalidAnchor]
anchor = |str|
    parser = one_of([anchor_word_boundary, anchor_non_word_boundary, anchor_start_of_string_only, anchor_end_of_string_only_not_newline, anchor_end_of_string_only, anchor_previous_match_end, anchor_end_of_string])
    parser(str) |> Result.map_err(|_| InvalidAnchor)

expect anchor("\\b") == Ok((AnchorWordBoundary, ""))
expect anchor("\\B") == Ok((AnchorNonWordBoundary, ""))
expect anchor("\\A") == Ok((AnchorStartOfStringOnly, ""))
expect anchor("\\z") == Ok((AnchorEndOfStringOnlyNotNewline, ""))
expect anchor("\\Z") == Ok((AnchorEndOfStringOnly, ""))
expect anchor("\\G") == Ok((AnchorPreviousMatchEnd, ""))
expect anchor("\\$") == Ok((AnchorEndOfString, ""))
