# The Date interface is based on the Elm Date module by Justin Nimbs,
# https://github.com/justinmimbs/date/tree/4.0.1.

# Note that so far this file only implements the parts that are needed
# for the computation of external interest rates.

interface Date
    exposes [
        Date,
        Month,
        compare,
        fromCalendarDate,
        ordinalDay,
        year,
    ]
    imports [
    ]

## The Month type enumerates all twelve months using standard three
## letter abbreviations.
Month : [Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec]

RataDie : I64

## The `Date` type represents a date.
Date : [RD RataDie]



isLeapYear : I64 -> Bool
isLeapYear = \y ->
    y % 4 == 0 && y % 100 != 0 || y % 400 == 0

expect isLeapYear 1900 == Bool.false

expect isLeapYear 1996 == Bool.true

expect isLeapYear 2000 == Bool.true

expect isLeapYear 2001 == Bool.false

expect isLeapYear 2002 == Bool.false

expect isLeapYear 2003 == Bool.false

expect isLeapYear 2004 == Bool.true

daysInYear : I64 -> I64
daysInYear = \y ->
    if isLeapYear y then
        366
    else
        365

expect daysInYear 1900 == 365

expect daysInYear 2000 == 366

expect daysInYear 2001 == 365

daysBeforeYear : I64 -> I64
daysBeforeYear = \y ->
    ym1 =
        y - 1

    leapYears =
        floorDiv ym1 4 - floorDiv ym1 100 + floorDiv ym1 400

    365 * ym1 + leapYears

expect daysBeforeYear 1 == 0

expect daysBeforeYear 2 == 365

expect daysBeforeYear 3 == 2 * 365

expect daysBeforeYear 4 == 3 * 365

expect daysBeforeYear 5 == 3 * 365 + 366

expect
    daysBeforeYear 2000
    ==
    List.sum ((List.range { start: At 1, end: At 1999 }) |> List.map (\y -> daysInYear y))

expect
    daysBeforeYear 2001
    ==
    List.sum ((List.range { start: At 1, end: At 2000 }) |> List.map (\y -> daysInYear y))

## The calendar year
year : Date -> I64
year = \RD rd ->
    (n400, r400) = divWithRemainder rd 146097

    (n100, r100) = divWithRemainder r400 36524

    (n4, r4) = divWithRemainder r100 1461

    (n1, r1) = divWithRemainder r4 365

    n =
        if r1 == 0 then
            0
        else
            1

    n400 * 400 + n100 * 100 + n4 * 4 + n1 + n

expect fromCalendarDate 2019 Dec 31 |> year == 2019

expect fromCalendarDate 2020 Jan 1 |> year == 2020

expect fromCalendarDate 2020 Dec 31 |> year == 2020

expect fromCalendarDate 2021 Jan 01 |> year == 2021

expect fromCalendarDate 2000 Jan 1 |> year == 2000

expect fromCalendarDate 2000 Dec 31 |> year == 2000

expect fromCalendarDate 2024 Feb 25 |> year == 2024

## Create a date from a calendar date by providing a year, a month,
## and a day of the month. Out-of-range day values will be clamped.
fromCalendarDate : I64, Month, I64 -> Date
fromCalendarDate = \y, m, d ->
    daysBeforeYear y + daysBeforeMonth y m + clamp 1 (daysInMonth y m) d |> RD

expect
    rd = fromCalendarDate 1 Jan 1
    rd == RD 1

expect
    rd = fromCalendarDate 2022 Sep 19
    rd == RD 738417

expect
    rd1 = fromCalendarDate 2024 Feb 29
    rd2 = fromCalendarDate 2024 Feb 30
    rd1 == rd2

toOrdinalDate : Date -> { year : I64, ordinalDay : I64 }
toOrdinalDate = \RD rd ->
    y = year (RD rd)

    { year: y, ordinalDay: rd - daysBeforeYear y }

expect fromCalendarDate 2020 Jan 1 |> toOrdinalDate == { year: 2020, ordinalDay: 1 }

expect fromCalendarDate 2020 Dec 31 |> toOrdinalDate == { year: 2020, ordinalDay: 366 }

expect fromCalendarDate 2021 Jan 1 |> toOrdinalDate == { year: 2021, ordinalDay: 1 }

expect fromCalendarDate 2021 Dec 31 |> toOrdinalDate == { year: 2021, ordinalDay: 365 }

## The day of the year (1-366).
ordinalDay : Date -> I64
ordinalDay = \date ->
    ordinalDate = toOrdinalDate date
    ordinalDate.ordinalDay

expect fromCalendarDate 2020 Jan 1 |> ordinalDay == 1

expect fromCalendarDate 2020 Dec 31 |> ordinalDay == 366

expect fromCalendarDate 2021 Jan 1 |> ordinalDay == 1

expect fromCalendarDate 2021 Dec 31 |> ordinalDay == 365

## Compare two dates. Returns `LT` if the first date is before the
## second date. Returns `GT` is the first data is after the second
## date. Returns `EQ` if the two dates are equal.
compare : Date, Date -> [LT, EQ, GT]
compare = \RD d1, RD d2 ->
    if d1 < d2 then
        LT
    else if d1 == d2 then
        EQ
    else
        GT

expect compare (RD 999) (RD 1000) == LT

expect compare (RD 1000) (RD 1000) == EQ

expect compare (RD 1000) (RD 999) == GT

expect compare (fromCalendarDate 2023 Jan 1) (fromCalendarDate 2024 Jan 1) == LT

clamp : I64, I64, I64 -> I64
clamp = \low, high, number ->
    if number <= low then
        low
    else if number >= high then
        high
    else
        number

expect clamp 1 10 -1 == 1

expect clamp 1 10 5 == 5

expect clamp 1 10 15 == 10

daysInMonth : I64, Month -> I64
daysInMonth = \y, m ->
    when m is
        Jan | Mar | May | Jul | Aug | Oct | Dec -> 31
        Feb ->
            if isLeapYear y then
                29
            else
                28

        Apr | Jun | Sep | Nov -> 30

expect daysInMonth 2008 Feb == 29

expect daysInMonth 2009 Feb == 28

expect daysInMonth 2022 Jan == 31

expect daysInMonth 2022 Apr == 30

daysBeforeMonth : I64, Month -> I64
daysBeforeMonth = \y, m ->
    leapDays =
        if isLeapYear y then
            1
        else
            0

    when m is
        Jan ->
            0

        Feb ->
            31

        Mar ->
            59 + leapDays

        Apr ->
            90 + leapDays

        May ->
            120 + leapDays

        Jun ->
            151 + leapDays

        Jul ->
            181 + leapDays

        Aug ->
            212 + leapDays

        Sep ->
            243 + leapDays

        Oct ->
            273 + leapDays

        Nov ->
            304 + leapDays

        Dec ->
            334 + leapDays

expect daysBeforeMonth 2022 Jan == 0

expect daysBeforeMonth 2023 Mar == 59

expect daysBeforeMonth 2024 Mar == 60

expect daysBeforeMonth 2023 Dec == 334

expect daysBeforeMonth 2024 Dec == 335


floorDiv : I64, I64 -> I64
floorDiv = \a, b ->
    Num.floor (Num.toF64 a / Num.toF64 b)

expect floorDiv 3 4 == 0

expect floorDiv -1 4 == -1

expect floorDiv 99 100 == 0

expect floorDiv 100 100 == 1

expect floorDiv -999 400 == -3

expect floorDiv 0 400 == 0

expect floorDiv 1001 400 == 2

divWithRemainder : I64, I64 -> (I64, I64)
divWithRemainder = \a, b ->
    div = floorDiv a b
    rem = Num.rem a b
    if rem < 0 then
        (div, rem + b)
    else
        (div, rem)

expect divWithRemainder 3 4 == (0, 3)

expect divWithRemainder -1 4 == (-1, 3)

expect divWithRemainder 99 100 == (0, 99)

expect divWithRemainder -999 400 == (-3, 201)

expect divWithRemainder 0 400 == (0, 0)

expect divWithRemainder 1001 400 == (2, 201)

