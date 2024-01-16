interface EffectiveInterestRate
    exposes [
    ]
    imports [
        Date,
        Min2ItemsList,
    ]

Payment : { amount : Dec, date : Date.RD }

PaymentStream : Min2ItemsList.Min2ItemsList Payment

# minimumWith : Min2ItemsList.Min2ItemsList Payment, (Payment, Payment -> [LT, EQ, GT]) -> Payment
# minimumWith = \Min2ItemsList fst snd rst, cmpFn ->
#     minFn = \x, y ->
#         dbg x
#         dbg y
#         if cmpFn x y == LT then
#             x
#         else
#             y

#     dbg fst
#     dbg snd
#     dbg [snd]
#     dbg rst

#     li = List.concat [snd] rst

#     dbg li

#     List.walk (List.concat [snd] rst) fst minFn

earliestPayment : PaymentStream -> Payment
earliestPayment = \paymentStream ->
    Min2ItemsList.minimumWith paymentStream (\p1, p2 -> Date.compare p1.date p2.date)

expect
    # earliest : { amount: Dec, date: Date.RD }
    earliest : Payment
    earliest = { amount: 1000, date: Date.fromCalendarDate 2020 Jan 1 }
    # someOther : { amount: Dec, date: Date.RD }
    someOther : Payment
    someOther = { amount: 500, date: Date.fromCalendarDate 2020 Jul 1 }
    # latest : { amount: Dec, date: Date.RD }
    latest : Payment
    latest = { amount: 500, date: Date.fromCalendarDate 2021 Jan 1 }

    dbg earliest

    dbg someOther

    dbg latest

    paymentStream = Min2ItemsList latest someOther [earliest]

    dbg paymentStream

    payment = earliestPayment paymentStream

    dbg payment

    # 1 == 1
    earliestPayment paymentStream == earliest
