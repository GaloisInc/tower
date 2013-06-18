# [Tower][tower]

## About

Tower is a concurrency framework for the [Ivory language][ivory]. Tower
composes Ivory programs into tasks which communicate via shared state and
synchronous channels.

At the moment, Tower includes a backend targeting the [FreeRTOS][freertos]
operating system.

## Installing

Tower should be installed in a cabal-dev sandbox along with the packages from
the [Ivory language repository][ivory].

## Sample Tower Application

A sample tower application can be found at
`ivory-tower/src/Ivory/Tower/Test/FooBarTest.hs` ([github][foobartest])
and a sample for building `FooBarTest` for FreeRTOS is found at
`ivory-tower-freertos/examples/Main.hs` ([github][foobarmain]).

In the [smaccmpilot-stm32f4][] project build tree, the sample application
is built into an executable named `tower-test`.

## Using Tower

At this time, a brief overview of the Tower framework [is available at
smaccmpilot.org][overview]. Further materials are forthcoming.

## Copyright and license
Copyright 2013 [Galois, Inc.][galois]

Licensed under the BSD 3-Clause License; you may not use this work except in
compliance with the License. A copy of the License is included in the LICENSE
file.

[ivory]: http://github.com/GaloisInc/ivory
[tower]: http://github.com/GaloisInc/tower
[smaccmpilot-stm32f4]: http://github.com/GaloisInc/smaccmpilot-stm32f4
[overview]: http://smaccmpilot.org/software/tower-overview.html

[freertos]: http://freertos.org
[galois]: http://galois.com

[foobartest]: https://github.com/GaloisInc/tower/blob/master/ivory-tower/src/Ivory/Tower/Test/FooBarTower.hs
[foobarmain]: https://github.com/GaloisInc/tower/blob/master/ivory-tower-freertos/examples/Main.hs


