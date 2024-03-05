interface EffectiveInterestRate
    exposes [
    ]
    imports [
        Date,
        Min2ItemsList,
        NewtonIteration,
    ]

# Using F64 for the amount field in Payment type values should be a
# temporary workaround. It should be Dec instead of
# F64. Unfortunately, that does not work because of problems with
# alignment of Dec values on x86 64 bit machines, compare
# https://github.com/roc-lang/roc/issues/6343.

Payment : { amount : F64, date : Date.Date }

NormalizedPayment : { amount : F64, offset : F64 }

PaymentStream : Min2ItemsList.Min2ItemsList Payment

NormalizedPaymentStream : Min2ItemsList.Min2ItemsList NormalizedPayment

earliestPayment : PaymentStream -> Payment
earliestPayment = \paymentStream ->
    Min2ItemsList.minimumWith paymentStream (\p1, p2 -> Date.compare p1.date p2.date)

expect
    earliest = { amount: 1000, date: Date.fromCalendarDate 2020 Jan 1 }
    someOther = { amount: 500, date: Date.fromCalendarDate 2020 Jul 1 }
    latest = { amount: 500, date: Date.fromCalendarDate 2021 Jan 1 }

    paymentStream = Min2ItemsList latest someOther [earliest]

    paymentEqual (earliestPayment paymentStream) earliest

paymentEqual : Payment, Payment -> Bool
paymentEqual = \{ amount: amount1, date: date1 }, { amount: amount2, date: date2 } ->
    Num.isZero (amount1 - amount2) && date1 == date2

expect
    p1 = { amount: 1000, date: Date.fromCalendarDate 2020 Jan 1 }
    p2 = { amount: 1000, date: Date.fromCalendarDate 2020 Jan 1 }

    paymentEqual p1 p2

expect
    p1 = { amount: 1000, date: Date.fromCalendarDate 2020 Jan 1 }
    p2 = { amount: 1001, date: Date.fromCalendarDate 2020 Jan 1 }

    paymentEqual p1 p2 |> Bool.not

expect
    p1 = { amount: 0.1 + 0.2, date: Date.fromCalendarDate 2020 Jan 1 }
    p2 = { amount: 0.3, date: Date.fromCalendarDate 2020 Jan 1 }

    paymentEqual p1 p2 |> Bool.not

dayInYear : Date.Date -> I64
dayInYear = \date ->
    Date.ordinalDay date - 1

expect
    date = Date.fromCalendarDate 2003 Jan 1
    dayInYear date == 0

expect
    date = Date.fromCalendarDate 2004 Jan 1
    dayInYear date == 0

expect
    date = Date.fromCalendarDate 2003 Dec 31
    dayInYear date == 364

expect
    date = Date.fromCalendarDate 2004 Dec 31
    dayInYear date == 365

isLeapYear : I64 -> Bool
isLeapYear = \year ->
    year % 4 == 0 && year % 100 != 0 || year % 400 == 0

expect isLeapYear 1900 == Bool.false

expect isLeapYear 1996 == Bool.true

expect isLeapYear 2000 == Bool.true

expect isLeapYear 2001 == Bool.false

expect isLeapYear 2002 == Bool.false

expect isLeapYear 2003 == Bool.false

expect isLeapYear 2004 == Bool.true

isInLeapYear : Date.Date -> Bool
isInLeapYear = \date ->
    Date.year date |> isLeapYear

expect
    date = Date.fromCalendarDate 2003 Jun 20
    isInLeapYear date == Bool.false

expect
    date = Date.fromCalendarDate 2004 Jun 20
    isInLeapYear date == Bool.true

normalizedDayInYear : Date.Date -> F64
normalizedDayInYear = \date ->
    if isInLeapYear date then
        Num.toF64 (dayInYear date) / 366
    else
        Num.toF64 (dayInYear date) / 365

expect
    date = Date.fromCalendarDate 2003 Jan 1
    almostEqual (normalizedDayInYear date) 0.0

expect
    date = Date.fromCalendarDate 2003 Dec 31
    almostEqual (normalizedDayInYear date) (364.0 / 365.0)

expect
    date = Date.fromCalendarDate 2004 Dec 31
    almostEqual (normalizedDayInYear date) (365.0 / 366.0)

toNormalizedPayment : Payment, Payment -> NormalizedPayment
toNormalizedPayment = \referencePayment, payment -> {
    amount: payment.amount,
    offset: Num.toF64 (Date.year payment.date)
    - Num.toF64 (Date.year referencePayment.date)
    + normalizedDayInYear payment.date
    - normalizedDayInYear referencePayment.date,
}

expect
    referencePayment = { amount: 1000, date: Date.fromCalendarDate 2020 Jan 1 }
    payment = { amount: 1000, date: Date.fromCalendarDate 2020 Jan 1 }
    almostEqual (toNormalizedPayment referencePayment payment).offset 0.0

expect
    referencePayment = { amount: 1000, date: Date.fromCalendarDate 2021 Jan 1 }
    payment = { amount: 1000, date: Date.fromCalendarDate 2021 Dec 31 }
    almostEqual (toNormalizedPayment referencePayment payment).offset (364.0 / 365.0)

expect
    referencePayment = { amount: 1000, date: Date.fromCalendarDate 2020 Jan 1 }
    payment = { amount: 1000, date: Date.fromCalendarDate 2020 Dec 31 }
    almostEqual (toNormalizedPayment referencePayment payment).offset (365.0 / 366.0)

expect
    referencePayment = { amount: 1000, date: Date.fromCalendarDate 2020 Jan 1 }
    payment = { amount: 1000, date: Date.fromCalendarDate 2020 Dec 31 }
    almostEqual (toNormalizedPayment referencePayment payment).offset (365.0 / 366.0)

expect
    referencePayment = { amount: 1000, date: Date.fromCalendarDate 2020 Jan 1 }
    payment = { amount: 1000, date: Date.fromCalendarDate 2021 Jan 1 }
    almostEqual (toNormalizedPayment referencePayment payment).offset 1.0

expect
    referencePayment = { amount: 1000, date: Date.fromCalendarDate 2020 Dec 31 }
    payment = { amount: 1000, date: Date.fromCalendarDate 2021 Jan 2 }
    almostEqual (toNormalizedPayment referencePayment payment).offset ((1.0 / 365.0) + (1.0 / 366.0))

expect
    referencePayment = { amount: 1000, date: Date.fromCalendarDate 2020 Feb 1 }
    payment = { amount: 1000, date: Date.fromCalendarDate 2020 Feb 29 }
    almostEqual (toNormalizedPayment referencePayment payment).offset (28.0 / 366.0)

toNormalizedPaymentStream : PaymentStream -> NormalizedPaymentStream
toNormalizedPaymentStream = \paymentStream ->
    referencePayment = earliestPayment paymentStream
    Min2ItemsList.map paymentStream (\payment -> toNormalizedPayment referencePayment payment)

expect
    paymentJan01 = { amount: -1000, date: Date.fromCalendarDate 2020 Jan 1 }
    paymentJan02 = { amount: 500, date: Date.fromCalendarDate 2020 Jan 2 }
    paymentDec31 = { amount: 500, date: Date.fromCalendarDate 2020 Dec 31 }
    normalizedPaymentStream =
        Min2ItemsList paymentJan01 paymentJan02 [paymentDec31]
        |> toNormalizedPaymentStream

    almostEqual (Min2ItemsList.first normalizedPaymentStream).offset 0.0
    &&
    almostEqual (Min2ItemsList.second normalizedPaymentStream).offset (1.0 / 366.0)
    &&
    when Min2ItemsList.rest normalizedPaymentStream is
        [normalizedPaymnt] ->
            almostEqual normalizedPaymnt.offset (365.0 / 366.0)

        _ ->
            Bool.false

netPresentValue : NormalizedPaymentStream -> (F64 -> F64)
netPresentValue = \normalizedPaymentStream ->
    \x ->
        Min2ItemsList.walk
            normalizedPaymentStream
            0.0
            (\sum, np -> sum + np.amount * (1 + x) ^ -np.offset)

expect
    payment1 = { amount: -1000, date: Date.fromCalendarDate 2019 Jan 1 }
    payment2 = { amount: 1600, date: Date.fromCalendarDate 2019 Apr 4 }
    payment3 = { amount: -2000, date: Date.fromCalendarDate 2019 Jul 7 }
    payment4 = { amount: 1600, date: Date.fromCalendarDate 2019 Oct 10 }
    normalizedPaymentStream =
        Min2ItemsList payment1 payment2 [payment3, payment4]
        |> toNormalizedPaymentStream
    npv = netPresentValue normalizedPaymentStream

    almostEqual (npv 0.0) 200.0

expect
    payment1 = { amount: -1000, date: Date.fromCalendarDate 2019 Jan 1 }
    payment2 = { amount: 500, date: Date.fromCalendarDate 2020 Jan 1 }
    payment3 = { amount: 500, date: Date.fromCalendarDate 2021 Jan 1 }
    normalizedPaymentStream =
        Min2ItemsList payment1 payment2 [payment3]
        |> toNormalizedPaymentStream
    npv = netPresentValue normalizedPaymentStream

    almostEqual (npv 1.0) -625.0

netPresentValueDerivative : NormalizedPaymentStream -> (F64 -> F64)
netPresentValueDerivative = \normalizesPaymentStream ->
    \x ->
        Min2ItemsList.walk
            normalizesPaymentStream
            0.0
            (\sum, np -> sum + np.amount * -np.offset * (1 + x) ^ (-np.offset - 1))

expect
    payment1 = { amount: -1000, date: Date.fromCalendarDate 2019 Jan 1 }
    payment2 = { amount: 500, date: Date.fromCalendarDate 2020 Jan 1 }
    payment3 = { amount: 500, date: Date.fromCalendarDate 2021 Jan 1 }
    normalizedPaymentStream =
        Min2ItemsList payment1 payment2 [payment3]
        |> toNormalizedPaymentStream
    npvp = netPresentValueDerivative normalizedPaymentStream

    almostEqual (npvp 1.0) -250.0

effectiveInterestRate : PaymentStream -> Result F64 Str
effectiveInterestRate = \paymentStream ->
    normalizedPaymentStream = toNormalizedPaymentStream paymentStream
    startValue = -0.75
    maxIterationDifference = 1e-8
    maxIterations = 64

    NewtonIteration.iterate
        (netPresentValue normalizedPaymentStream)
        (netPresentValueDerivative normalizedPaymentStream)
        startValue
        maxIterationDifference
        maxIterations

expect
    payment1 = { amount: 2000, date: Date.fromCalendarDate 2013 Jun 1 }
    payment2 = { amount: -1000, date: Date.fromCalendarDate 2014 Jun 1 }
    payment3 = { amount: -1000, date: Date.fromCalendarDate 2015 Jun 1 }
    paymentStream = Min2ItemsList payment1 payment2 [payment3]

    when EffectiveInterestRate.effectiveInterestRate paymentStream is
        Ok interestRate ->
            almostEqual interestRate 0.0

        Err _ ->
            Bool.false

expect
    payment1 = { amount: 2000, date: Date.fromCalendarDate 2013 Jun 1 }
    payment2 = { amount: -1000, date: Date.fromCalendarDate 2014 Jun 1 }
    payment3 = { amount: -1000, date: Date.fromCalendarDate 2015 Jun 1 }
    payment4 = { amount: -100, date: Date.fromCalendarDate 2015 Jul 1 }
    paymentStream = Min2ItemsList payment1 payment2 [payment3, payment4]

    when EffectiveInterestRate.effectiveInterestRate paymentStream is
        Ok interestRate ->
            interestRate > 0.0

        Err _ ->
            Bool.false

expect
    payment1 = { amount: -1065.25, date: Date.fromCalendarDate 2011 Apr 21 }
    payment2 = { amount: 130.69, date: Date.fromCalendarDate 2014 May 23 }
    paymentStream = Min2ItemsList payment1 payment2 []

    when EffectiveInterestRate.effectiveInterestRate paymentStream is
        Ok interestRate ->
            Num.absDiff interestRate -0.4931 < 1e-4

        Err _ ->
            Bool.false

expect
    listProduct : List a, List b -> List (a, b)
    listProduct = \la, lb ->
        List.map la (\va -> List.map lb (\vb -> (va, vb)))
        |> List.join

    years = List.range { start: At 2015, end: At 2034 }
    months = [Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec]
    paymentStream =
        Min2ItemsList
            { amount: 240000, date: Date.fromCalendarDate 2015 Jan 1 }
            { amount: 0, date: Date.fromCalendarDate 2015 Jan 1 }
            (
                listProduct years months
                |> List.map
                    (\(year, month) ->
                        { amount: -1200, date: Date.fromCalendarDate year month 1 }
                    )
            )

    when EffectiveInterestRate.effectiveInterestRate paymentStream is
        Ok interestRate ->
            Num.absDiff interestRate 0.0191 < 1e-3

        Err _ ->
            Bool.false

almostEqual : F64, F64 -> Bool
almostEqual = \a, b ->
    Num.absDiff a b < 1e-8
