module [range_quantifier, character, character_group_item, character_range, character_group_negative_modifier]

import Parser exposing [Parser, Maybe, map, maybe, lhs, rhs, both, string, number, char, one_of, excluding]
import Types exposing [RangeQuantifier, Negation, CharacterGroupItem, CharacterRange, Character]

## Parse a regex range quantifier
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

character_group_negative_modifier : Parser Negation [InvalidCharacterGroupNegativeModifier]
character_group_negative_modifier = |str|
    parser = string("^") |> map(|_carrot| Ok(Negated))
    parser(str) |> Result.map_err(|_| InvalidCharacterGroupNegativeModifier)

expect character_group_negative_modifier("^") == Ok((Negated, ""))

character_range : Parser CharacterRange [InvalidCharacterRange]
character_range = |str|
    parser = char |> lhs(string("-")) |> both(char) |> map(|(start, end)| Ok(CharRange((start, end))))
    parser(str) |> Result.map_err(|_| InvalidCharacterRange)

expect character_range("a-b") == Ok((CharRange((97, 98)), ""))
expect character_range("a-") == Err(InvalidCharacterRange)

character : Parser Character [CharNotFound]
character = |str|
    parser = char |> excluding(|c| c == ']') |> map(|c| Ok(Char(c)))
    parser(str) |> Result.map_err(|_| CharNotFound)

expect character("a") == Ok((Char('a'), ""))

character_excluding : List U8 -> Parser Character [CharNotFound]
character_excluding = |excluded_characters|
    |str|
        parser = char |> excluding(|c| List.contains(excluded_characters, c)) |> map(|c| Ok(Char(c)))
        parser(str) |> Result.map_err(|_| CharNotFound)

expect character_excluding(['a'])("a") == Err(CharNotFound)

escaped_reserved_character : Parser Character [EscapedCharNotFound]
escaped_reserved_character = |str|
    reserved_character_strs =
        ["\\.", "\\^", "\\$", "\\*", "\\+", "\\?", "\\(", "\\)", "\\[", "\\]", "\\{", "\\}", "\\|", "\\\\", "\\/", "\\-", "\\ "]
    pattern = one_of(List.map(reserved_character_strs, string))
    parser = pattern |> map(|s| s |> Str.to_utf8 |> List.get(1) |> Result.map_ok(|c| Char(c)))
    parser(str) |> Result.map_err(|_| EscapedCharNotFound)

expect escaped_reserved_character("\\\\") == Ok((Char('\\'), ""))

character_group_item : Parser CharacterGroupItem [InvalidCharacterGroupItem]
character_group_item = |str|
    parser = one_of([character_range, escaped_reserved_character, character]) |> map(|item| Ok(item))
    parser(str) |> Result.map_err(|_| InvalidCharacterGroupItem)

expect character_group_item("a") == Ok((Char('a'), ""))
expect character_group_item("a-b") == Ok((CharRange(('a', 'b')), ""))

character_group : Parser (Maybe Negation, CharacterGroupItem) [InvalidCharacterGroup]
character_group = |str|
    parser =
        string("[")
        |> rhs(maybe(character_group_negative_modifier))
        |> both(one_of([character_group_item]))
        |> lhs(string("]"))
    parser(str) |> Result.map_err(|_| InvalidCharacterGroup)

expect character_group("[a]") == Ok ((None, Char 'a'), "")
expect character_group("[a-b]") == Ok ((None, CharRange ('a', 'b')), "")
expect character_group("[^a]") == Ok ((Some Negated, Char 'a'), "")
expect character_group("[\\]]") == Ok ((None, Char ']'), "")
expect character_group("[]b]") == Err(InvalidCharacterGroup)

