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

PaymentStream : Min2ItemsList.Min2ItemsList Payment

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

