
# GiottoUtils 0.1.6

## New
- `%null%`, `%na%`, and `%none%` infix operators for setting a default value when an object is, respectively, NULL, NA, or length of 0.
- `local_seed()` for transiently setting a seed only for the duration of a function


# GiottoUtils 0.1.5 (2024/02/11)

## Bug Fixes
- `str_locate2()` now works with vectorized inputs like the *stringr* counterpart
- `str_locate2()` no-matches now properly return `NA_integer_` instead of negative values

## New
- `to_scipy_sparse()` conversion of R matrices to scipy sparse representations

# GiottoUtils 0.1.4 (2024/02/05)

## Enhancement
- new `seed` param for `getDistinctColors()` that allows the ordering of the distinct colors to be changed.
- `package_check()` now allows more than one package to be checked using repo:location notation.



# GiottoUtils 0.1.3

## Added
- Add: `mixedsort()` and `mixedorder()` from *gtools*

# GiottoUtils 0.1.2 (2024/01/02)

## Added
- Add: `getMonochromeColors()` basic color palette function. Useful for image colorization.


# GiottoUtils 0.1.1 (2023/12/16)

## Breaking changes
- Remove checkmate reexports
- Remove deprecated `flex_lapply()`
- Rename `guard_against_notgiotto()` to `assert_giotto()`
- Rename `set_row_order_dt()` to `dt_set_row_order()`
- Rename `dcast_dt_string()` to `dt_dcast_string()`
- Rename `sort_combine_two_DT_columns()` to `dt_sort_combine_two_columns()`
- Rename `DT_removeNA()` to `dt_remove_na()`
- Rename `assert_DT()` to `assert_dt()`

## Added
- Add `gstop()` as a framework for sending module specific error messages
- Add `str_locate2()` (implementation of `stringr::str_locate()` in base R)
- Add `str_vector()` for pretty printing a vector as a string
- Add: `str_bracket()`, `str_parenth()`, `str_quote()`, `str_double_quote()` convenience functions
- Move: `get_prev_fname()`, `get_args()`, and `get_prev_call()` to this package
- Add `get_prev_call()`
- Move basic color palettes `getDistinctColors()` and `getRainbowColors()` to this package
- Add: *RColorBrewer* to suggests for `getDistinctColors()`

## Changes
- improvements to `assert_giotto()`




# GiottoUtils 0.1.0 (2023/11/29)

Initial release
