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

> A KISS pure Fortran Library exposing a simple class pimped for easy code timing:

- FITTER is a pure Fortran (KISS) library;
- FITTER is Fortran 2008+ standard compliant;
- FITTER is OOP designed;
- FITTER is a Free, Open Source Project.

#### A taste of FITTER

```fortran
use fitter
type(timer) :: chronos ! The timer.

call chronos%start(name='foo') ! 'name' is optional
call foo
call chronos%stop

call chronos%start(name='bar')
call bar
call chronos%stop

! print 'foo' timing
call chronos%print(name='foo')

! print all timings
call chronos%print

! directly retrieve elasped times (in seconds)
print '(A,I5)', 'Time (seconds) spent in "foo": ', chronos%time(name='foo') ! 'name' is optional

! clean all timer data
call chronos%clean
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

+ [ ] handle (automatically store) new profile for each snippet (part) of the code timed;
+ [ ] easy print statistics;
+ [ ] easy retrieve timings;
* [ ] Test Driven Developed (TDD);
* [ ] collaborative developed;
* [ ] well documented;
* [ ] free!

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
FoBiS.py build
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

Go to [Top](#top)
