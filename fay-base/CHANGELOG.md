## Changelog

## 0.21.1.1 (2019-05-01)

* Added `Show` instance for `Text` (#453).

## 0.21.1.0 (2018-03-09)

* fay 0.24.0.0 support

## 0.21.0.0 (2017-08-02)

* Changed the definition of `splitOn` from `Char -> Text -> [Text]` to `Text -> Text -> [Text]` to make it more general and to match `text:Data.Text.splitOn`. Thanks to A. Bram Neijt!

## 0.20.0.1 (2015-06-24)

* Fix polymorphic arguments in `Data.Var` (thanks Jakub Ryška)

## 0.20.0.0 (2015-02-10)

* Changes the type signature of `when` and `unless` from `:: Bool -> Fay a -> Fay ()` to `:: Bool -> Fay () -> Fay ()`. This allows tail call optimization for these functions and matches base - by Zachary Mason

#### 0.19.4.2 (2015-01-05)

* Allow `fay 0.22.*`.

#### 0.19.4.1 (2014-10-25)

* Merged fay-base into the main fay repository.

### 0.19.4 (2014-10-21)

* Fix type signature of `Data.Text.pack`.
* Add IsString instance for `Text` for using the ffi with `RebindableSyntax` enabled.

### 0.19.3 (2014-10-21)

* Lots of additions adding the following modules:
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

#### 0.19.2.1 (2014-10-11)

* Allow `fay 0.21`

### 0.19.2 (2014-07-29)

* Remove the `Base.Ord` constraint from `Ord` so new Ord instances can be added. Note that you cannot use a custom implementation of the methods, JavaScript's native operators are used for comparisons.

#### 0.19.1.2 (2014-04-29)

* Allow `fay 0.20`

#### 0.19.1.1 (2014-04-07)

* Fix compilation on GHC 7.8

### 0.19.1 (2014-03-13)

* Add Data.Char

## 0.19 (2014-01-14)

* Ord instance for Integer
* Export base versions of Maybe, Ordering, and Either instead of redefining when compiling with GHC. This helps when writing libraries targeting both Fay and GHC
* `Prelude`: Add `void`, `>=>` and `<=<`, `unless`, `forM`, `mapM`
* Add `Debug.Trace` module exporting `trace` and `traceShow`

Minor:
* Upper bound to the latest major Fay version

## 0.18.0.0 (2013-09-24)

* Fixed implementation of `>>` and `>>=` (this bug didn't affect normal usage)
* Add `fail`
* Generalize type signatures for `fromIntegral` and `fromInteger`


## 0.17.0.0 (2013-08-27)

* The type signature of `FFI.ffi` has been generalized to `IsString s => s -> a` to support `RebindableSyntax`.
* Prelude now exports `ifThenElse` as a default for `RebindableSyntax`


## 0.16.0.0 (2013-08-05)

* Added more Ratio functions and move them all into Data.Ratio
