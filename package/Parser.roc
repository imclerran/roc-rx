module [
    Parser,
    Maybe,
    filter,
    excluding,
    map,
    flat_map,
    zero_or_more,
    one_or_more,
    zip,
    zip_3,
    zip_4,
    zip_5,
    maybe,
    one_of,
    lhs,
    rhs,
    both,
    string,
    char,
    digit,
    number,
]

import unicode.CodePoint
import Utils exposing [is_digit]

## TYPES ----------------------------------------------------------------------

## ```
## Parser a err : Str -> Result (a, Str) err
## ```
Parser a err : Str -> Result (a, Str) err

## ```
## Maybe a : [Some a, None]
## ```
Maybe a : [Some a, None]

## PARSER COMBINATORS ---------------------------------------------------------

## Create a parser that will filter out matches that do not satisfy the given predicate
filter : Parser a _, (a -> Bool) -> Parser a [FilteredOut]
filter = |parser, predicate|
    map(parser, |match| if predicate(match) then Ok(match) else Err FilteredOut)

excluding : Parser a _, (a -> Bool) -> Parser a [Excluded]_
excluding = |parser, predicate|
    map(parser, |match| if predicate(match) then Err Excluded else Ok(match))

## Convert a parser of one type into a parser of another type using a tranform function which turns the first type into a result of the second type
map : Parser a _, (a -> Result b _) -> Parser b _
map = |parser, transform|
    flat_map(
        parser,
        |match|
            |str|
                when transform(match) is
                    Ok(b) -> Ok((b, str))
                    Err err -> Err err,
    )

## Convert a parser of one type into a parser of another type using a transform function which turns the first type into a parser of the second type
flat_map : Parser a _, (a -> Parser b _) -> Parser b _
flat_map = |parser_a, transform|
    |str|
        when parser_a(str) is
            Ok((a, rest)) -> transform(a)(rest)
            Err err -> Err err

## Create a parser which matches one or more occurrences of the given parser
one_or_more : Parser a _ -> Parser (List a) [LessThanOneFound]_
one_or_more = |parser|
    map(
        zero_or_more(parser),
        |list|
            if List.is_empty(list) then
                Err LessThanOneFound
            else
                Ok(list),
    )

## Create a parser which matches zero or more occurrences of the given parser
zero_or_more : Parser a _ -> Parser (List a) _
zero_or_more = |parser|
    |str|
        helper = |acc, current_str|
            when parser(current_str) is
                Ok((match, rest)) -> helper (List.append acc match) rest
                Err _ -> Ok((acc, current_str))
        helper [] str

## Combine 2 parsers into a single parser that returns a tuple of 2 values
zip : Parser a _, Parser b _ -> Parser (a, b) _
zip = |parser_a, parser_b|
    flat_map(parser_a, |match_a| map(parser_b, |match_b| Ok((match_a, match_b))))

## Combine 3 parsers into a single parser that returns a tuple of 3 values
zip_3 : Parser a _, Parser b _, Parser c _ -> Parser (a, b, c) _
zip_3 = |parser_a, parser_b, parser_c|
    zip(parser_a, zip(parser_b, parser_c)) |> map(|(a, (b, c))| Ok((a, b, c)))

## Combine 4 parsers into a single parser that returns a tuple of 4 values
zip_4 : Parser a _, Parser b _, Parser c _, Parser d _ -> Parser (a, b, c, d) _
zip_4 = |parser_a, parser_b, parser_c, parser_d|
    zip(parser_a, zip_3(parser_b, parser_c, parser_d)) |> map(|(a, (b, c, d))| Ok((a, b, c, d)))

## Combine 5 parsers into a single parser that returns a tuple of 5 values
zip_5 : Parser a _, Parser b _, Parser c _, Parser d _, Parser e _ -> Parser (a, b, c, d, e) _
zip_5 = |parser_a, parser_b, parser_c, parser_d, parser_e|
    zip(parser_a, zip_4(parser_b, parser_c, parser_d, parser_e)) |> map(|(a, (b, c, d, e))| Ok((a, b, c, d, e)))

## Convert a parser that can fail into a parser that can return a Maybe
maybe : Parser a _ -> Parser (Maybe a) _
maybe = |parser|
    |str|
        when parser(str) is
            Ok((match, rest)) -> Ok((Some(match), rest))
            Err _ -> Ok((None, str))

## Try each parser in sequence until one succeeds
one_of : List (Parser a err) -> Parser a [NoMatchFound]
one_of = |parsers|
    |str|
        List.walk_until(
            parsers,
            Err(NoMatchFound),
            |none, parser|
                when parser(str) is
                    Ok(result) -> Break(Ok(result))
                    Err _ -> Continue(none),
        )

expect
    parser = one_of([string("abc"), string("def")])
    parser("abc") == Ok(("abc", ""))

expect
    parser = one_of([string("abc"), string("def")])
    parser("def") == Ok(("def", ""))

expect
    parser = one_of([string("abc"), string("def")])
    parser("ghi") == Err(NoMatchFound)

## OPERATORS ------------------------------------------------------------------

## keep the result of the left parser
lhs : Parser a _, Parser b _ -> Parser a _
lhs = |parser_l, parser_r|
    zip(parser_l, parser_r) |> map(|(l, _r)| Ok(l))

expect
    parser = string("l") |> lhs(string("r"))
    parser("lr") == Ok(("l", ""))

## keep the result of the right parser
rhs : Parser a _, Parser b _ -> Parser b _
rhs = |parser_l, parser_r|
    zip(parser_l, parser_r) |> map(|(_l, r)| Ok(r))

expect
    parser = string("l") |> rhs(string("r"))
    parser("lr") == Ok(("r", ""))

expect
    parser = string("{") |> rhs(number) |> lhs(string("}"))
    parser("{123}") == Ok((123, ""))

## keep the result of both parsers
both : Parser a _, Parser b _ -> Parser (a, b) _
both = |parser_l, parser_r| zip(parser_l, parser_r)

expect
    parser = string("{") |> both(string("}"))
    parser("{}") == Ok (("{", "}"), "")

## PARSERS --------------------------------------------------------------------

## Create a parser that will match a specific string
string : Str -> Parser Str [StringNotFound]
string = |prefix|
    |str|
        if Str.starts_with(str, prefix) then
            Ok((prefix, Str.drop_prefix(str, prefix)))
        else
            Err StringNotFound

expect
    string("{")("{") == Ok(("{", ""))

expect
    string("Hello")("Hello, world!") == Ok(("Hello", ", world!"))

## Parse a single character
char : Parser U8 [CharNotFound]
char = |str|
    when Str.to_utf8(str) is
        [c, .. as rest] -> Ok((c, Str.from_utf8_lossy(rest)))
        [] -> Err CharNotFound

expect
    char("1") == Ok(('1', ""))

## Parse a non-clustered grapheme
atomic_grapheme : Parser Str [GraphemeNotFound]
atomic_grapheme = |str|
    when Str.to_utf8(str) |> CodePoint.parse_partial_utf8 is
        Ok({code_point}) -> 
            cp_str = CodePoint.to_str([code_point]) ?  |_| GraphemeNotFound
            rest = Str.drop_prefix(str, cp_str)
            Ok((cp_str, rest))
        Err _ -> Err GraphemeNotFound

expect
    res = atomic_grapheme("🔥")
    res == Ok(("🔥", ""))

## Parse a digit
digit : Parser U8 [NotADigit]
digit = |str| filter(char, |c| is_digit(c))(str) |> Result.map_err(|_| NotADigit)

expect
    digit("1") == Ok(('1', ""))

## Parse a number
number : Parser U64 [NotANumber]
number = |str|
    parser = one_or_more(digit) |> map(|digits| digits |> Str.from_utf8_lossy |> Str.to_u64)
    parser(str) |> Result.map_err(|_| NotANumber)

expect
    number("1") == Ok((1, ""))

expect
    number("012345") == Ok((12345, ""))
