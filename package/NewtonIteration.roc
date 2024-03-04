interface NewtonIteration
    exposes [
        iterate,
    ]
    imports [
    ]

## Tries to find a root of the specified function
##
## The first argument passed into `iterate` is a function `f` whose
## root is supposed to be determined.  The second argument `fp` is the
## derivative of `f`. `startValue` is the start value of the
## iteration. `maxIterationDifference` is the maximum difference
## between two iteration steps. The iteration process terminates if
## the difference between two approximations becomes less than
## `maxIterationDifference`. `maxIterations` is the maximum number of
## iterations.
##
## In the success case, `iterate` returns `Ok root` where `root` is an
## approximation of a root of `f`. Otherwise, `iterate` returns `Err
## "too many iterations"`.
##
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

# Finds the root of the identity
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

# Does not find a root of x^4 - 1 with an accuracy of 1.0e-8 in 4 iterations
expect
    f : F64 -> F64
    f = \x ->
        x ^ 4 - 1

    fp : F64 -> F64
    fp = \x ->
        4 * x ^ 3

    result = iterate f fp 2.0 1.0e-8 4

    when result is
        Ok _ ->
            Bool.false

        Err err ->
            "too many iterations" == err

# Finds a root of x^4 - 1 with an accuracy of 1.0e-8 in 8 iterations
expect
    f : F64 -> F64
    f = \x ->
        x ^ 4 - 1

    fp : F64 -> F64
    fp = \x ->
        4 * x ^ 3

    result = iterate f fp 2.0 1.0e-8 8

    when result is
        Ok root ->
            1.0 - 1.0e-8 <= root && root <= 1.0 + 1.0e-8

        Err _ ->
            Bool.false
