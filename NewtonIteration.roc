interface NewtonIteration
    exposes [
        iterate
    ]
    imports [
    ]

iterate : (F64 -> F64), (F64 -> F64), F64, F64, U64 -> Result F64 Str
iterate = \f, fp, startValue, maxIterationDifference, maxIterations ->
    iterationStep f fp startValue maxIterationDifference maxIterations 0


iterationStep : (F64 -> F64), (F64 -> F64), F64, F64, U64, U64 -> Result F64 Str
iterationStep = \f, fp, previousIteration, maxIterationDifference, maxIterations, iterationCount ->
    if iterationCount > maxIterations then
        Err "too many iterations"
    else
        iteration = previousIteration - f previousIteration / fp previousIteration

        if Num.abs (iteration - previousIteration) <= maxIterationDifference then
            Ok iteration
        else
            iterationStep f fp iteration maxIterationDifference maxIterations (iterationCount + 1)


## finds the root of the identity
expect
    f : F64 -> F64
    f = \x ->
        x

    fp : F64 -> F64
    fp = \_x ->
        1

    result = iterate f fp 1.0 1.0e-8 4

    when result is
        Ok root ->
            0.0 - 1.0e-8 <= root && root <= 0.0 + 1.0e-8
        Err _ ->
            Bool.false

## does not find a root of x^4 - 1 with an accuracy of 1.0e-8 in 4 iterations
expect
    f : F64 -> F64
    f = \x ->
        x^4 - 1

    fp : F64 -> F64
    fp = \x ->
        4 * x^3

    result = iterate f fp 2.0 1.0e-8 4

    when result is
        Ok _ ->
            Bool.false
        Err err ->
            "too many iterations" == err


## finds a root of x^4 - 1 with an accuracy of 1.0e-8 in 8 iterations
expect
    f : F64 -> F64
    f = \x ->
        x^4 - 1

    fp : F64 -> F64
    fp = \x ->
        4 * x^3

    result = iterate f fp 2.0 1.0e-8 8

    when result is
        Ok root ->
            1.0 - 1.0e-8 <= root && root <= 0.0 + 1.0e-8
        Err _ ->
            Bool.false
