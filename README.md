## Work in Progress

### Introduction

The EffectiveInterestRate module exposes the `effectiveInterestRate`
function for computing the effective interest rate of a stream of
payments in a very general case. It is implemented in the
[Roc](https://www.roc-lang.org/) programming language.

### Warning

This repository has been written using a Roc compiler that was built
from scratch at the beginning of March 2024. Roc is under active
development. The compiler changes frequently. The code provided by
this repository may not even compile tomorrow.

### Installation

As of beginning of 2024, the easiest way to use the
`effeciveInterestRate` function is to copy the files in the package
subdirectory to some place in your project where the Roc compiler can
find them and import what you need.

### Payment Streams

The Payment type is defined as follows:
```roc
Payment : { amount : F64, date : Date.Date }
```
A payment consists of an amount (in whatever currency) and a date. The
amount can be positive of negative. (It can also be 0, but that may
not make too much sense.)

For example
```roc
{ amount: -2000, date: Date.fromCalendarDate 2015 Jan 1 }
```
represents an amount of -2000 transferred at Jan 01, 2015.

A payment stream is a list of payments. Note that payment streams are
implemented using the `Min2ItemsList` type that is defined as follows.
```roc
Min2ItemsList a : [Min2ItemsList a a (List a)]
```
This implies that a payment stream must have at least two payments.

The following is a very simple example that can also be found in a
similar form as a test in the source code of the EffectiveInterestRate
module.
```roc
payment1 = { amount: -1065.25, date: Date.fromCalendarDate 2011 Apr 21 }
payment2 = { amount: 130.69, date: Date.fromCalendarDate 2014 May 23 }
paymentStream = Min2ItemsList payment1 payment2 []
effectiveInterestRate = EffectiveInterestRate.effectiveInterestRate paymentStream
```
This should return about -0.4931. Not a particularly good investment.

### Tests

Run `roc test ./package/main.roc` in the top level directory of the
repository.

### Documentation

Run `roc docs ./package/main.roc` in the top level directory of the
repository.

### Licenses

This project includes code ported from the Date package by Justin
Mimbs, which is licensed under the BSD 3-Clause License. The original
license can be found in the LICENSE file of the
[Date](https://github.com/justinmimbs/date) repository.
