

# GiottoUtils 0.1.1 (TBD)

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
