interface Min2ItemsList
    exposes [
        Min2ItemsList,
        minimumWith,
    ]
    imports [
    ]

Min2ItemsList a : [Min2ItemsList a a (List a)]

first : Min2ItemsList a -> a
first = \Min2ItemsList fst _ _ ->
    fst

expect first (Min2ItemsList 1 2 [3]) == 1

second : Min2ItemsList a -> a
second = \Min2ItemsList _ snd _ ->
    snd

expect second (Min2ItemsList 1 2 [3]) == 2

rest : Min2ItemsList a -> List a
rest = \Min2ItemsList _ _ rst ->
    rst

expect rest (Min2ItemsList 1 2 [3]) == [3]

toList : Min2ItemsList a -> List a
toList = \Min2ItemsList fst snd rst ->
    List.concat [fst, snd] rst

expect toList (Min2ItemsList 1 2 [3]) == [1, 2, 3]

map : Min2ItemsList a, (a -> b) -> Min2ItemsList b
map = \Min2ItemsList fst snd rst, fn ->
    Min2ItemsList (fn fst) (fn snd) (List.map rst fn)

expect map (Min2ItemsList 1 2 [3]) (\n -> n * n) == Min2ItemsList 1 4 [9]

walk : Min2ItemsList a, b, (b, a -> b) -> b
walk = \list, state, fn ->
    List.walk (toList list) state fn

expect walk (Min2ItemsList 1 2 [3]) 0 Num.add == 6

minimumWith : Min2ItemsList a, (a, a -> [LT, EQ, GT]) -> a
minimumWith = \Min2ItemsList fst snd rst, cmpFn ->
    minFn = \x, y ->
        if cmpFn x y == LT then
            x
        else
            y

    List.walk (List.concat [snd] rst) fst minFn

expect
    minFn = \x, y ->
        if x < y then
            LT
        else if x > y then
            GT
        else
            EQ

    [
        minimumWith (Min2ItemsList 1 2 [3]) minFn,
        minimumWith (Min2ItemsList 1 1 [1]) minFn,
        minimumWith (Min2ItemsList 3 2 [1]) minFn,
    ]
    == [1, 1, 1]

expect
    minFn = \a, b ->
        if Num.abs a < Num.abs b then
            LT
        else if Num.abs a > Num.abs b then
            GT
        else
            EQ

    minimumWith (Min2ItemsList 3 -1 [2]) minFn == -1
