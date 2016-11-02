<a name="top"></a>

# FITTER [![GitHub tag](https://img.shields.io/github/tag/szaghi/FITTER.svg)]() [![Join the chat at https://gitter.im/szaghi/FITTER](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/szaghi/FITTER?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[![License](https://img.shields.io/badge/license-GNU%20GeneraL%20Public%20License%20v3,%20GPLv3-blue.svg)]()
[![License](https://img.shields.io/badge/license-BSD2-red.svg)]()
[![License](https://img.shields.io/badge/license-BSD3-red.svg)]()
[![License](https://img.shields.io/badge/license-MIT-red.svg)]()

[![Status](https://img.shields.io/badge/status-unstable-red.svg)]()
[![Build Status](https://travis-ci.org/szaghi/FITTER.svg?branch=master)](https://travis-ci.org/szaghi/FITTER)
[![Coverage Status](https://img.shields.io/codecov/c/github/szaghi/FITTER.svg)](http://codecov.io/github/szaghi/FITTER?branch=master)

### FITTER, Fortran tIc Tic TimER

> A KISS pure Fortran Library for easy *timing* code snippets.

- FITTER is a pure Fortran (KISS) library;
- FITTER is Fortran 2008+ standard compliant;
- FITTER is OOP designed;
- FITTER is a Free, Open Source Project.

#### A taste of FITTER

```fortran
use fitter
type(timer) :: chronos ! The timer.
integer     :: s       ! Counter.

call chronos%tic(name='foo')
call foo
call chronos%toc

do s=1, 4
  call chronos%tic(name='bar')
  call bar
  call chronos%toc
enddo

call chronos%tic(name='foo')
call foo
call chronos%toc

call chronos%print(statistics=.true.)

! output:

! Elapsed time into "foo": 0.603904970000000E-001 [s]
! Elapsed time into "bar": 0.103229564000000E+000 [s]
! Number of snippets tracked: 2
! Total elapsed time: 0.163620061000000E+000 [s]
! Average (snippet) elapsed time: 0.818100305000000E-001 [s]
! Relative elapsed time into each snippet:
!   + foo: 36.909%
!     Number of snippet hits: 2
!     Total elapsed time: 0.603904970000000E-001 [s]
!     Average elapsed time: 0.301952485000000E-001 [s]
!     Relative elapsed time into each hit:
!       + 001: 70.932%
!       + 002: 29.068%
!   + bar: 63.091%
!     Number of snippet hits: 4
!     Total elapsed time: 0.103229564000000E+000 [s]
!     Average elapsed time: 0.258073910000000E-001 [s]
!     Relative elapsed time into each hit:
!       + 001: 23.667%
!       + 002: 25.264%
!       + 003: 25.527%
!       + 004: 25.542%
```

#### Issues

[![GitHub issues](https://img.shields.io/github/issues/szaghi/FITTER.svg)]()
[![Ready in backlog](https://badge.waffle.io/szaghi/FITTER.png?label=ready&title=Ready)](https://waffle.io/szaghi/FITTER)
[![In Progress](https://badge.waffle.io/szaghi/FITTER.png?label=in%20progress&title=In%20Progress)](https://waffle.io/szaghi/FITTER)
[![Open bugs](https://badge.waffle.io/szaghi/FITTER.png?label=bug&title=Open%20Bugs)](https://waffle.io/szaghi/FITTER)

#### Compiler Support

[![Compiler](https://img.shields.io/badge/GNU-v6.1.1+-brightgreen.svg)]()
[![Compiler](https://img.shields.io/badge/Intel-v16.1+-brightgreen.svg)]()
[![Compiler](https://img.shields.io/badge/IBM%20XL-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/g95-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/NAG-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/PGI-not%20tested-yellow.svg)]()

---

[What is FITTER?](#what-is-FITTER) | [Main features](#main-features) | [Copyrights](#copyrights) | [Download](#download) | [Compilation](#compilation) | [Documentation](#documentation) | [References](#references)

---

## What is FITTER?

> **FITTER** is KISS pure Fortran library exposing a friendly class for *timing* code: the `timer` class is a *tic-toc* timer that easily track the time spent into parts of your codes, no more no less.

## Main features

> FITTER is a *one-single-class* library exposing the `timer` object that

+ [x] handle (automatically store) new timing for each snippet timed:
    + [x] handle (automatically store) multiple timing for each snippet timed, namely allow multiple-hits tracking of snippets:
+ [x] easy print statistics;
+ [x] easy retrieve timings;
* [x] Test Driven Developed (TDD);
* [x] collaborative developed;
* [ ] well documented;
* [x] free!

Any feature request is welcome.

Go to [Top](#top)

## Copyrights

FITTER is a Free and Open Source Software (FOSS), it is distributed under a **very permissive** multi-licensing system: selectable licenses are [GPLv3](http://www.gnu.org/licenses/gpl-3.0.html), [BSD2-Clause](http://opensource.org/licenses/BSD-2-Clause), [BSD3-Clause](http://opensource.org/licenses/BSD-3-Clause) and [MIT](http://opensource.org/licenses/MIT), feel free to select the license that best matches your workflow.

> Anyone is interest to use, to develop or to contribute to FITTER is welcome.

More details can be found on [wiki](https://github.com/szaghi/FITTER/wiki/Copyrights).

Go to [Top](#top)

## Download

To be written.

Go to [Top](#top)

## Compilation

FITTER is a modern Fortran project thus a modern Fortran compiler is need to compile the project. The project is modular, namely it exploits Fortran modules. As a consequence, there is compilation-cascade hierarchy to build the project. To correctly build the project the following approaches are supported

+ [Build by means of FoBiS](#build-by-means-of-fobis): full support;
+ [Build by means of GNU Make](#build-by-means-of-gnu-make): to be implemented.
+ [Build by means of CMake](#build-by-means-of-cmake): to be implemented.

The FoBiS building support is the most complete, as it is the one used for the developing FITTER.

### Build by means of FoBiS

A `fobos` file is provided to build the project by means of the Fortran Building System [FoBiS.py](https://github.com/szaghi/FoBiS).

#### Build all programs

Type

```shell
FoBiS.py build -mode tests-gnu
```

After (a successful) building a directory `./exe` is created containing all the compiled programs found recursively in the tree project.

### Build by means of GNU Make

To be implemented.

### Build by means of CMake

To be implemented.

Go to [Top](#top)

---

## Documentation

Besides this README file the FITTER documentation is contained into its own [wiki](https://github.com/szaghi/FITTER/wiki). Detailed documentation of the API is contained into the [GitHub Pages](http://szaghi.github.io/FITTER/index.html) that can also be created locally by means of [ford tool](https://github.com/cmacmackin/ford).

To be completed.

Go to [Top](#top)
