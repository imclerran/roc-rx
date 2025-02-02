module [Parser, Option, filter, map, flat_map, zero_or_more, one_or_more, zip, zip_3, option]

Parser a err : Str -> Result (a, Str) err

Option a : [Some a, None]

filter : Parser a _, (a -> Bool) -> Parser a [FilteredOut]_
filter = |parser, predicate|
    map(parser, |match| if predicate(match) then Ok(match) else Err FilteredOut)

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

flat_map : Parser a _, (a -> Parser b _) -> Parser b _
flat_map = |parser_a, a_to_parser_b|
    |str|
        when parser_a(str) is
            Ok((a, _rest)) ->
                parser_b = a_to_parser_b(a)
                parser_b(str)

            Err err -> Err err

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

zero_or_more : Parser a _ -> Parser (List a) _
zero_or_more = |parser|
    |str|
        helper = |current_str, acc|
            when parser(current_str) is
                Ok((match, rest)) ->
                    helper rest (List.append acc match)

                Err _ -> Ok((acc, current_str))

        when helper str [] is
            Ok(([], _rest)) -> Err NoneFound
            Ok((acc, rest)) -> Ok((acc, rest))

zip : Parser a _, Parser b _ -> Parser (a, b) _
zip = |parser_a, parser_b|
    flat_map(
        parser_a,
        |match_a|
            map(
                parser_b,
                |match_b| Ok((match_a, match_b)),
            ),
    )

zip_3 : Parser a _, Parser b _, Parser c _ -> Parser (a, b, c) _
zip_3 = |parser_a, parser_b, parser_c|
    zip(parser_a, zip(parser_b, parser_c)) |> map(|(a, (b, c))| Ok((a, b, c)))

option : Parser a _ -> Parser (Option a) _
option = |parser|
    |str|
        when parser(str) is
            Ok((match, rest)) -> Ok((Some(match), rest))
            Err _ -> Ok((None, str))