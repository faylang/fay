## Changelog

See full history at: <https://github.com/faylang/fay/commits>

### 0.24.1.0 (2020-03-26)

* GHC 8.10.1 support.

#### 0.24.0.5 (2020-03-10)

* GHC 8.8 support.

#### 0.24.0.4 (2019-12-03)

* Fixes stack nix integration broken.

#### 0.24.0.3 (2019-04-29)

* Dependency updates including GHC-8.6 support.
* Added stack compatibility.

#### 0.24.0.2

* Fix dependent compilation fail when building project with stack (#457).

#### 0.24.0.1

* Dependency updates incl GHC 8.4 support

## 0.24.0.0

* Add the option to generate typescript output, thanks to Junji Hashimoto.

### 0.23.2.0

* GHC 8.2 support
* Use traverse-with-class 1.0.*, and as a result drop support for GHC < 8.

#### 0.23.1.16

* Fix build on GHC 7.4

#### 0.23.1.15

* Allow optparse-applicative 0.13.*.

#### 0.23.1.14

* Fix a compilation error introduced in 0.23.1.13.

#### 0.23.1.13

* Add support for haskell-src-exts 1.18.1.

#### 0.23.1.12

* Fix compilation on GHC < 7.8.

#### 0.23.1.11

* Tighten some impossible lower bounds we can't support.

#### 0.23.1.10

* Don't compile with `-fprof-auto` by default.

#### 0.23.1.9

* Allow and require `haskell-src-exts 1.17.*`

#### 0.23.1.8

* Allow `vector 0.11.*`

#### 0.23.1.7

* Fix panic when compiling irrefutable pattern matches on lists (thanks Christopher Parks)

#### 0.23.1.6

* Allow `syb 0.5.*`

#### 0.23.1.5

* Allow `aeson 0.9.*`

#### 0.23.1.4

* Fix compilation on at least GHC 7.6 (maybe older versions as well...)

#### 0.23.1.3

* Inline parts of `haskell-names 0.4.1` and `haskell-packages` to drop transitive dependencies on Cabal and other packages.

#### 0.23.1.2

* Allow filepath 1.4.*

#### 0.23.1.1

* Allow do let bindings of newtypes.

### 0.23.1.0

* Add `--show-ghc-calls` and `configShowGhcCalls` to print invocations to GHC.

#### 0.23.0.1

* Allow `mtl-compat 0.2.*` and `transformers-compat 0.4.*"`.

## 0.23.0.0

New features:

* GHC 7.10 support
* Add a `--pretty-operators` flag to replace the escaped operator names with their actual names - By Michal Seweryn
* Add a `--pretty-all` flag that enables `--pretty`, `--pretty-operators`, and `--pretty-thunks` - By Michal Seweryn
* De/serialize type variables as 'automatic'. You no longer have to annotate FFI functions with Automatic, this is now the default. If you need the old behavior, use Ptr. For most existing cases this change shouldn't change anything. - By Zachary Mason

Example output with `--pretty-all`:
```javascript
Main.main = new $(function(){
  return _(_(Prelude["$"])(Prelude.print))(_(_(Prelude["++"])(Main.g))(Fay$$list("b")));
});
```


API changes - By Michal Seweryn:

* Export CompileResult from Fay.Types
* Export PrintReader and PrintWriter, and reducing number of PrintState fields (these are moved to PrinterReader and PrintWriter)
* In the meanwhile PrintWriter field type changed - output is now stored as ShowS - tests run faster now
* Replace monadic functions (askP, getP, ...) with higher level ones: indented, askIf, newline, write, mapping. askIf is pretty much like previous askP, but works like if-then-else.
* Internally we are now using `ExceptT` instead of `ErrorT`. This fixes deprecation warnings when using `transformers 0.4.*`.

Bug fixes:
* Defer automatic_function fayToJs call until after argument application - by Zachary Mason

Dependency bumps:
* Allow `utf8-string 1.0.*`
* Allow `time == 1.5.*`

## 0.22.0.0

* Add a `--pretty-thunks` flag and compiler option that replaces `Fay$$_` and `Fay$$$` with `_` and `$` respectively. Consider this a development flag since it may clash with JS libraries (notably jQuery and underscore)
* Allow `language-ecmascript 0.17.*`

#### 0.21.2.1 (2014-11-10)

* Hide all packages by default when typechecking. This avoids conflicting with packages that haven't been specified on the command-line with `--package`, e.g. the `text` package when you import `Data.Text`.

### 0.21.2 (2014-10-21)

* Previously all package imports were ignored, now we only ignore `"base"` package imports.

### 0.21.1 (2014-10-21)

* Lots of additions to in `fay-base` adding the following modules:
  * Data.Var - Mutable variables, Reactive variables, and reactive signals
  * Unsafe.Coerce
  * Data.Text (fay-text will be updated to reuse this module)
  * Data.Time
  * Data.Ord, Data.Function, Data.Maybe, Data.List, Data.Either
  * Data.Defined and Data.Nullable
  * Data.Mutex - Simple mutexes
  * Control.Exception
  * Data.LocalStorage
  * Data.MutMap - Mutable maps

The introduction of `Data.Var` required some additions to fay's runtime.

#### 0.21.0.2 (2014-10-19)

* Fallback to ghc and ghc-pkg in PATH if not available from GHC.Paths

#### 0.21.0.1 (2014-10-12)

* Update to `optparse-applicative == 0.11.*`

## 0.21.0.0 (2014-10-11)

* Errors are now properly thrown from `encodeFay`. Changes the type signature to `encodeFay :: (GenericQ Value -> GenericQ Value) -> GenericQ Value`
* Update to `haskell-src-exts == 1.16.*`, This changes the type signature of `readerCompileLit` to `:: S.Sign -> S.Literal -> Compile JsExp`
* Fixes `ghc-pkg describe` stdout errors not always being printed

### 0.20.2.0 (2014-09-14)

* Config option to disable optimizations of newtypes, treating them as
  normal data types. This can be triggered by setting
  `configOptimizeNewtypes = False` or passing
  `--no-optimized-newtypes`.

#### 0.20.1.4 (2014-09-04)

* Update to `optparse-applicative == 0.10.*`

#### 0.20.1.3 (2014-08-29)

* Test suite is no longer built by default, cabal install with `-ftest` to enable.

#### 0.20.1.2 (2014-08-18)

* Updated homepage URLs, fay-lang.org was 301'd

#### 0.20.1.1 (2014-06-17)

* Don't cache the `main` thunk in the generated `main` call.

### 0.20.1.0 (2014-06-14)

* Add default case for UTCTime in Fay.Convert using the aeson instances. Note that this serializes to a json string so you won't be able to deserialize it as a separate type (such as Date) when using `Automatic` in Fay.

* Added `Fay.Config.defaultConfigWithSandbox` that reads the `HASKELL_PACKAGE_SANDBOX` environment variable. Client libraries can use this instead of manually reading from `getEnvironment`.

#### 0.20.0.4 (2014-05-23)

* Allow `optparse-applicative 0.9.*`

#### 0.20.0.3 (2014-05-09)

* Allow `mtl 2.2.*`

#### 0.20.0.2 (2014-05-08)

* Allow `haskell-names 0.4.*`

#### 0.20.0.1 (2014-05-08)

* Allow `transformers >= 0.4.1 && < 0.5`

## 0.20.0.0 (2014-04-29)

* Adds support for LambdaCase and MultiWayIf

* Modules have moved around a lot and several modules have been un-exposed. From now on you will probably only need to deal with at most `Fay` (which re-exports a lot of things), `Fay.Config`, `Fay.Types.CompileError`, `Fay.Convert`, and `Fay.Types.CompileResult`. Please let us know if you would like us to expose more things

* Config:
  * `CompileConfig` has been renamed to `Config` and is now located in `Fay.Config`.
  * `CompileConfig` has become a temporary type alias for `Config`.
  * `Fay.Compiler.Config` is deprecated, import `Fay` or `Fay.Config` instead.
  * The `data-default` instance for `Config` is deprecated, use `defaultConfig` instead.

* compiling
  * `compileFileWithState` is deprecated, use `compileFileWithResult` which returns a `Fay.Types.CompileResult` instead. As a consequence `CompileState` is also deprecated from public consumption.
  * `compileFile`, `compileFromToAndGenerateHtml` no longer return a triple with the sourcemap, use `compileFileWithResult` if you want access to this.

* Importing `Fay.Types` has been deprecated, import `Fay` instead.

* `readFromFay` has been rewritten using `syb` instead of `pretty-show` (Thanks to Michael Sloan and Chris Done)
  * This introduces the following breaking changes:
    * `readFromFay` has a `Data` constraint instead of `Show`.
    * Drops support for Rational and Integer (see below for migration steps). The reason is that neither was serialized in a way that would roundtrip for all values. Also, for similar reasons, fromRational is potentially divergent for aeson's new use of the Scientific type.
  * And adds the following features:
    * You can now write custom `Show` instances targeting GHC for types shared with Fay.
    * Better performance.
    * Allows the serialization and deserialization to be customized on a per-type basis, via encodeFay and decodeFay.
  * To migrate code using Rational or Integer, use encodeFay and pass an argument e.g. `(\f x -> maybe (f x) myIntegerToValueConversion (cast x))` and likewise to decodeFay.

Bugfixes:

* Mltiple guards on a pattern in a case expression skipped everything but the first guard. To fix this an optimization we had on pattern conditions was disabled.

Dependency bumps:

* Allow Cabal 1.20 and 1.21

Internal:

* Test cases are now using `tasty` instead of `test-framework`. To run cases in parallel use `fay-tests --num-threads=N` (see `fay-tests --help` for more info).
* Added a test group for desugaring.

#### 0.19.2.1 (2014-04-14)

* Allow `haskell-src-exts 1.15.*`

### 0.19.2 (2014-04-10)

* Fixes a bug where arrays used with empty data decls would be deserialized into a Fay list instead of kept as is.

#### 0.19.1.2 (2014-04-07)

* Fix optimizations that were not applied and add codegen test cases.

#### 0.19.1.1 (2014-03-17)

* Allow `optparse-applicative 0.8.*`

### 0.19.1 (2014-03-13)

* Added Data.Char to fay-base

Dependency bumps:
* Allow `Cabal 1.19.*`
* Allow `process 1.2.*`

#### 0.19.0.2 (2014-01-30)

Bugfixes:
* Don't export transcoding information for fay-base packages when compiling with --no-stdlib
* Better error messages when forgetting the type signature in an FFI declaration

#### 0.19.0.1 (2014-01-15)

Dependency bumps:
* Allow `aeson 0.7.*`

## 0.19 (2014-01-14)

* Made import Prelude is implicit, but note that RebindableSyntax implies NoImplicitPrelude.
* Allow FFI declarations in let and where statements: `let f :: X; f = ffi "..."` and `where f :: X; f = ffi "..."`

Bugfixes:
* Removed extra </script> tag that was generated by --html-wrapper
* FFI expressions in top level declarations now produce identical code to a normal top level FFI declaration
* Don't export Data.Ratio and Debug.Trace when using --no-stdlib

Dependency bumps:
* Allow text 1.1
* Allow attoparsec 0.11


**Note: 0.18.0.1 added source mappings returned by `Fay:compileFile` and friends meaning it should have been a major bump. Sorry about this!**

#### 0.18.1.3 (2013-12-14)

* Add parsing of Integer to Fay.Convert (note that the runtime doesn't have arbitrary precision Integers)
* Allow text 1.0.*

#### 0.18.1.2 (2013-11-26)

* Add support for indirect application of newtypes (such as `p = NewType; foo = p x` and `bar = NewType $ y`)

#### 0.18.1.1 (2013-11-22)

* Fix a bug where records with the same name as top level modules wouldn't be initialized correctly.
* Fail when using enum syntax on unsupported literal types (for instance ['a'..'z'])

### 0.18.1 (2013-11-07)

* Add support for TupleSections

#### 0.18.0.5 (2013-10-28)

Bugfixes:
* Disallow unsupported patterns in where/let declarations instead of `<<loop>>`ing on them

Minor:
* Put upper bounds on all dependencies

#### 0.18.0.4 (2013-10-25)

Bugfixes:
* Allow `//` as an operator name (added flag to `hse-cpp`)
* Don't transcode function values when using an EmptyDataDecl

#### 0.18.0.3 (2013-10-23)

Minor:
* Allow `optparse-applicative == 0.7.*`
* Fix `examples/Cont.hs`

#### 0.18.0.2 (2013-10-16)

Bug fixes:
* Regression: Work around a bug in optparse-applicative 0.6 that prevents `--strict` from being used.

#### 0.18.0.1 (2013-10-16)

* Source maps for top level definitions, use `--sourcemap`

Bug fixes:
* Regression: Equality checks for (G)ADTs (`deriving Eq`)
* Fix `--strict` for top level ADT values (such as `module M where g = R`)
* Regression: Serialization in the presence of compression/renaming
* Pass NoImplicitPrelude (and other enabled extensions) to haskell-names to resolve ambiguities when Prelude isn't imported.

Minor:
* Bump optparse-applicative to 0.6.*
* Bump haskell-names to 0.3.1 to allow compilation with Cabal 1.14
* Ignore more declarations (useful when code sharing with GHC)

## 0.18.0.0 (2013-09-24)

New features:
* Support for qualified imports. Note: You still can't have multiple constructors with the same name in the FFI since the `instance` field in the serialization is still unqualified.
* `Automatic` transcoding now works for functions. See [Calling Fay From JavaScript](https://github.com/faylang/fay/wiki/Calling-Fay-from-JavaScript)
* `--strict modulename[, ..]` generates strict and transcoding wrappers for a module's exports. See [Calling Fay From JavaScript](https://github.com/faylang/fay/wiki/Calling-Fay-from-JavaScript)
* `--typecheck-only` just runs the GHC type checker with the appropriate Fay flags.
* `--runtime-path FILEPATH` allows you to supply a custom runtime. Probably only useful for debugging.

Bug fixes:
* Don't crash when trying to get the fayToJsFun of an object without constructor.name
* Fixed bug that accidentally flattened list arguments in `jsToFay`
* Fix construction with RecordWildCards not taking already listed fields into account

Breaking Changes:
* Fay.Compiler.Debug has been removed (for now)
* The interactive compilation mode has been removed (for now)

Internal changes:
* Migrated to haskell-src-ext's annotated AST.
* Name resolution is now done using haskell-names, Fay's name resolution code is now pure and a lot simpler.

## 0.17.0.0 (2013-08-27)

* With the `RebindableSyntax` and `OverloadedStrings` extensions Fay will treat Haskell string literals as JavaScript Strings. Add this in all modules and import Fay.Text (from the `fay-text` package). This is *not* a breaking change, without these extensions in a module `String` will be used, as before. All modules can still interoperate normally even if only some of them use this feature. Note that you may have to define `fromInteger` when using this with Num literals.

* The type signature of `Fay.FFI.ffi` (in fay) and `FFI.ffi` (in fay-base) has been generalized to `IsString s => s -> a` to support `RebindableSyntax`.

* Much faster compile time (of the compiler itself) by having the executables depend on the library.

Bugfixes:
* The empty list and unit is now serialized to `null` when using Automatic (it used to throw an error).

Minor:
* Restrict upper bound on `language-ecmascript` to `< 1.0`

#### 0.16.0.3 (2013-08-23)

* Support for tuple constructors (`(,,) 1,2,3`)

Minor:
* Bump `pretty-show` to `>= 1.6`
* Remove the `-fdevel` flag (when compiling fay itself)

#### 0.16.0.2 (2013-08-21)

Minor:
* Bump `haskell-src-exts` to `>= 1.14`

#### 0.16.0.1 (2013-08-08)

Bugfixes:
 * Allow combining multiline strings with CPP

## 0.16.0.0 (2013-08-05)
 * New module generation, modules generate code separately in the format `My.Module.foo` instead of `My$Module$foo`
 * Transcoding information is also produced separately for each module
 * Removed `--naked`, `--dispatcher`, and `--no-dispatcher`. They are probably not needed anymore
 * Removed `Fay$$fayToJsUserDefined` and `Fay$$jsToFayUserDefined`, instead call `Fay$$fayToJs` and `Fay$$jsToFay` respectively
 * Escape semi reserved words from `Object` when printing (`constructor` -> `$constructor`). This only matters if you call Fay from JS
 * `Automatic` now handles lists and tuples (#251)
 * `Language.Fay.FFI` renamed to `Fay.FFI` (as before, Fay code can import `FFI` from `fay-base`)

Minor:
 * Print location of parse errors
 * Compile with -XNoImplicitPrelude
 * Support for testing nested modules (module A, module A.B)
 * Bump `language-ecmascript to >= 0.15` (new API)
 * Rename/remove some CompileErrors
 * All tests are now included in dist

Bug fixes:
 * Force cars in string serialization (#306)

## 0.15.0.0 (2013-06-08)
 * Expression level FFI calls, `ffi "alert('hello!')" :: Fay ()`
 * Support let pattern matches
 * Smaller output for serialization code
 * --base-path flag to use a custom base (mainly for fay-prim)
 * Allow ExistentialQuantification, FlexibleContexts, FlexibleInstances, KindSignatures
 * Verify that GADTs using non-record syntax works
 * JS->Fay function serialization
 * Serialization support for `()`, tuples and `Char`
 * Add more reserved words for Google Closure

Bugfixes:
 * Fix a bug when an imported module contains types
 * Fix a bug with EModuleContents exports
 * Fix where clause inside pattern guards in function definitions
 * Fix type variables in serialization for multiple constructors
 * Fix EThingAll exports for types with constructors with a different name
 * Don't export types in EThingAll and EThingWith


### 0.14.5.0 (2013-04-24)
* Support for newtypes (with no runtime cost!)
* --base-path flag to specify custom locations for fay-base
* Fix a bug where imports shadowing local bindings would prevent the local binding from being exported


### 0.14.4.0 (2013-04-21)
* Fix record updates on IE <= 8
* Import tweaks, will make compilation a lot faster (4x reported) when there are a lot of imports
* Parse hs sources with base fixities
