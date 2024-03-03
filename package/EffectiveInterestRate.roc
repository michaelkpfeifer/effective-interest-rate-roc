interface EffectiveInterestRate
    exposes [
    ]
    imports [
        Date,
        Min2ItemsList,
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
        |> EffectiveInterestRate.toNormalizedPaymentStream

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
        |> EffectiveInterestRate.toNormalizedPaymentStream
    npv = netPresentValue normalizedPaymentStream

    almostEqual (npv 0.0) 200.0

expect
    payment1 = { amount: -1000, date: Date.fromCalendarDate 2019 Jan 1 }
    payment2 = { amount: 500, date: Date.fromCalendarDate 2020 Jan 1 }
    payment3 = { amount: 500, date: Date.fromCalendarDate 2021 Jan 1 }
    normalizedPaymentStream =
        Min2ItemsList payment1 payment2 [payment3]
        |> EffectiveInterestRate.toNormalizedPaymentStream
    npv = netPresentValue normalizedPaymentStream

    almostEqual (npv 1.0) -625.0

almostEqual : F64, F64 -> Bool
almostEqual = \a, b ->
    Num.absDiff a b < 1e-8
