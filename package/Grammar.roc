module [range_quantifier]

import Parser exposing [Parser, map, optional, lhs, rhs, both, string, number]
import Types exposing [RangeQuantifier]

## Parse a regex range quantifier
range_quantifier : Parser RangeQuantifier [InvalidRangeQuantifier]
range_quantifier = |str|
    pattern = string("{") |> rhs(number) |> both(optional(string(",") |> rhs(optional(number)))) |> lhs(string("}"))
    parser = pattern
        |> map(|(min, maybe_comma_max)|
            when maybe_comma_max is
                None -> Ok(ExactRange(min))
                Some(None) -> Ok(LowerBounded(min)) 
                Some(Some(max)) -> Ok(LowerAndUpperBounded((min, max)))
        )
    parser(str) |> Result.map_err(|_| InvalidRangeQuantifier)


expect
    res = range_quantifier("{1}") 
    res == Ok((ExactRange(1), ""))

expect
    res = range_quantifier("{1,}")
    res == Ok((LowerBounded(1), ""))

expect
    res = range_quantifier("{1,2}")
    res == Ok((LowerAndUpperBounded((1, 2)), ""))

expect
    res = range_quantifier("{123,456}")
    res == Ok((LowerAndUpperBounded((123, 456)), ""))

expect
    res = range_quantifier("{1,2}abc")
    res == Ok((LowerAndUpperBounded((1, 2)), "abc"))

expect
    res = range_quantifier("{")
    res == Err(InvalidRangeQuantifier)

expect
    res = range_quantifier("{1,2")
    res == Err(InvalidRangeQuantifier)

expect
    res = range_quantifier("{1,2,}")
    res == Err(InvalidRangeQuantifier)