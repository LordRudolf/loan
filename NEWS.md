# loan 0.1.0

A rework of the package core: a declared data contract, one naming scheme, and
one way for every analysis function to take its arguments.

## Data contract (new)

* `loan_tbl()` declares which column plays which role: `application_id`,
  `loan_id`, `client_id`, `application_created_at`, `first_delay_at`,
  `application_status`, `outcomes` + `primary_outcome`, `predictors` and
  `supplementary` (kept but never analysed). Every column holds exactly one
  role; an outcome declared as a predictor is an error. Roles survive dplyr
  verbs.
* Unlisted columns get a role from `unlisted_role`: `"predictors"`,
  `"supplementary"`, or `"auto"` (default) -- analysable types become
  predictors, others supplementary, and a near-copy (> 0.95; |Pearson r|, or
  Tschuprow's T for nominal columns) of a listed id, timestamp, outcome or
  predictor becomes supplementary. Declared predictors of an unanalysable type
  are kept with a warning.
* `value_map()` maps an institution's raw status values onto the canonical
  `approved` / `rejected` / `cancelled`. `cancelled` (closed outside
  risk-policy control) is excluded from approval-rate denominators.
* `binary_outcome()` declares which raw outcome values are `bad`. Several
  outcomes can be declared; functions use the primary one unless given
  `outcome = "<name>"`. An undeclared outcome is inferred, with a warning when
  the bad class has to be guessed.

## Analysis functions

* Every analysis function accepts three input forms: bare vectors, a data
  frame with column names (or vectors -- a single string is a column name), or
  a `loan_tbl` whose roles fill in the arguments. All forms give identical
  results and fail with identical messages.
* `contingency_table()` adds canonical `count_approved` / `count_rejected` /
  `count_cancelled` and `decisioned_total` next to the raw status counts, and
  counts a binary outcome as `count_good` / `count_bad`.
* `group_stats()` reports `approval_rate` (approved / (approved + rejected)).
* Logical variables are analysed, grouped `TRUE` / `FALSE`.

## Renamed

* Functions: `produce_contingency_table()` -> `contingency_table()`,
`get_group_stats()` -> `group_stats()`, `get_WOE()` -> `add_woe()`,
`get_fisher_p_val()` -> `add_fisher_p()`, `calculate_PSI()` -> `psi()`,
`calculate_PSI_table()` -> `psi_from_tables()`, `get_dynamic_stats()` ->
`dynamic_stats()`, `visualize_paired_u_test()` -> `plot_paired_u_test()`,
`visualize_variable_importance()` -> `plot_variable_importance()`,
`visualize_worth()` -> `plot_profit_curve()`, `train_model()` -> `fit_model()`,
`autopreproc()` -> `auto_recipe()`, `create_cv_folds()` -> `cv_folds()`,
`step_woe2()` -> `step_woebin()`.

* Arguments: the first argument of every S3 generic and its methods is `x`; the
analysed variable is `variable` (not `the_var` / `varname` / `var_name`); the
outcome is `outcome` (not `target` / `binary_outcome` / `target_name`); the
status is `application_status` everywhere (not `application_status_name`);
`param` / `param_name` -> `measure`; `template_mat` -> `template_matrix`; the
data argument of data-only functions is `data` (not `df`).

## Bug fixes

* S3 methods are registered: the data-frame forms failed to dispatch.
* dplyr and ggplot2 functions are imported or qualified: functions failed
  unless the user had attached those packages.
* A method's first argument no longer differs from its generic's: named or
  reordered calls could silently analyse the wrong argument.
* The missing-value group of a factor variable no longer disappears.
* The vector form of `group_stats()` no longer bins twice (which merged a bin
  into `value_other`), and no longer loses the status `value_map()`.
* `print()` of a PSI accepts print arguments; `plot()` of `group_stats()`
  honours `plots_to_make`; `plot_univariate_smooth(x_log_scale = TRUE)` works.
* A mistyped column name gives a clear error in every function instead of a
  silent `NULL`.

## Removed

* The `loan_df` class, `define_aliases()`, the `$` alias accessor and
  `map_the_variable()` -- replaced by `loan_tbl()`, `value_map()` and
  `binary_outcome()`.
* `detect_bad_label()` and the `bad_label`, `accept_label` and `make_var`
  arguments -- superseded by the canonical vocabularies.
* Prototype and exploration code (`train_scorecard()`, the reject-inference
  script) and the vignettes written for `loan_df`.

## Experimental, being redesigned

* `dynamic_stats()` and the modelling layer (`evaluate_features()`,
  `fit_model()`, `cv_folds()`, `auto_recipe()`, `step_woebin()`).
