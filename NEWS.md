
# GiottoUtils 0.1.10 (2024/07/26)


## New
- `dir_manifest()` for creating a named `list` of files within a directory. Mostly wraps `list.files()`


# GiottoUtils 0.1.9 (2024/07/12)

## New
- `deprecate_param()` utility function for streamlining code
- `print_list()` for pretty printing of list-like objects
- `from_scipy_sparse()` for conversion from scipy `csr` and `csc` to Matrix `dgCMatrix` and `dgRMatrix`

## Changes
- newer github version checking refactored into `new_github_ver_avail()`

# GiottoUtils 0.1.8 (2024/05/22)

## New
- `wrap_txtf()` variant of `wrap_txt()` that uses `sprintf()` formatting
- `init_option()` for setting an option if it does not exist

## Enhancement
- `dt_to_matrix()` can now chunk the conversion to `Matrix` to avoid memory issues
- `get_args_list()` now has a `keep` param that allows selection of which collected args to keep


# GiottoUtils 0.1.6 (2024/02/26)

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
