# [Tower][tower]

## About

Tower is a concurrency framework for the [Ivory language][ivory]. Tower
composes Ivory programs into monitors which communicate with synchronous
channels.

Tower uses pluggable backends to support individual operating systems and
target architectures. A backend for the [FreeRTOS][freertos] operating
system running on the [STM32][] line of microcontrollers is available in
the [ivory-tower-stm32][] repo, and a backend for general purpose POSIX
operating systems is available in the [ivory-tower-posix][] repo.

[![Build Status](https://travis-ci.org/GaloisInc/tower.svg?branch=tower-9)](https://travis-ci.org/GaloisInc/tower)

## Copyright and license
Copyright 2015 [Galois, Inc.][galois]

Licensed under the BSD 3-Clause License; you may not use this work except in
compliance with the License. A copy of the License is included in the LICENSE
file.

Portions Copyright (c) 2013-2014, Spiros Eliopoulos, derived from the now
unmaintained `toml` package.

[ivory]: http://github.com/GaloisInc/ivory
[tower]: http://github.com/GaloisInc/tower
[ivory-tower-stm32]: http://github.com/GaloisInc/ivory-tower-stm32
[ivory-tower-posix]: http://github.com/GaloisInc/ivory-tower-posix
[overview]: http://smaccmpilot.org/software/tower-overview.html

[STM32]: http://www.st.com/stm32
[freertos]: http://freertos.org
[galois]: http://galois.com


## Contributing

This project adheres to the
[Contributor Covenant code of conduct](CODE_OF_CONDUCT.md).
By participating, you are expected to uphold this code. Please report unaccpetable
behavior to [smaccm@galois.com](mailto:smaccm@galois.com).

