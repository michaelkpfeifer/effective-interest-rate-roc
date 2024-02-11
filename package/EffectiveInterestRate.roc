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

Payment : { amount : F64, date : Date.RD }

PaymentStream : Min2ItemsList.Min2ItemsList Payment

earliestPayment : PaymentStream -> Payment
earliestPayment = \paymentStream ->
    Min2ItemsList.minimumWith paymentStream (\p1, p2 -> Date.compare p1.date p2.date)

expect
    earliest = { amount: 1000, date: Date.fromCalendarDate 2020 Jan 1 }
    someOther = { amount: 500, date: Date.fromCalendarDate 2020 Jul 1 }
    latest = { amount: 500, date: Date.fromCalendarDate 2021 Jan 1 }

    paymentStream = Min2ItemsList latest someOther [earliest]

    equal (earliestPayment paymentStream) earliest

equal : Payment, Payment -> Bool
equal = \{ amount: amount1, date: date1 }, { amount: amount2, date: date2 } ->
    Num.isZero (amount1 - amount2) && date1 == date2
