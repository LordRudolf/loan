# loan 0.1.0

A rework of the package core: a declared data contract, one naming scheme, and
one way for every analysis function to take its arguments.

## Data contract (new)

* `loan_tbl()` declares which column plays which role: `application_id`,
  `loan_id`, `client_id`, `application_created_at`, `first_delay_at`,
  `application_status`, `outcomes` + `primary_outcome`, `predictors` and
  `supplementary` (excluded from automatic predictor selection, but available
  when named explicitly). Every column holds exactly one
  role; an outcome declared as a predictor is an error. Roles survive dplyr
  verbs.
* Unlisted columns get a role from `unlisted_role`: `"predictors"`,
  `"supplementary"`, or `"auto"` (default) -- analysable types become
  predictors, others supplementary, and a near-copy (> 0.95; |Pearson r|, or
  Tschuprow's T for nominal columns) of a listed id, timestamp, outcome or
  predictor becomes supplementary. Declared predictors of an unanalysable type
  are kept with a warning.
* `predictor_provenance()` identifies predictors listed by the analyst versus
  those assigned by `unlisted_role`. The split appears in `print(loan_tbl)` and
  follows column renames and removals.
* `value_map()` maps an institution's raw status values onto the canonical
  `approved` / `rejected` / `cancelled`. `cancelled` (closed outside
  risk-policy control) is excluded from approval-rate denominators.
* `binary_outcome()` declares which raw outcome values are `bad`. Several
  outcomes can be declared; functions use the primary one unless given
  `outcome = "<name>"`. An undeclared outcome is inferred, with a warning when
  the bad class has to be guessed.

## Analysis functions

* Single-variable functions accept a `loan_tbl` whose roles fill in the
  arguments, a plain data frame with column names (or aligned values), and
  vectors when there is a natural vector form. All forms use the same
  calculation for the same values and options. Functions operating on an
  existing `loan_*` result accept that result.
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

* Public exports are now explicit; internal helpers are no longer exported.
  Recipes methods for `step_woebin()` are registered for dispatch, and the
  empty `plot_density()` stub was removed.
* `plot_univariate_smooth(grouping_var = ...)` now accepts column names,
  aligned character, factor or logical vectors, and expressions evaluated in
  the data with the caller's environment as fallback. Invalid names and lengths
  use the shared argument errors; missing groups use `value_NA`.
* `dplyr::rename()`, `dplyr::select()` and `[` now update `loan_tbl` roles when
  columns are renamed or removed, including columns inside status and outcome
  declarations. Removing the primary outcome leaves it unset; analyses need an
  explicit `outcome =` if another outcome remains.
* `psi()` and `psi_from_tables()` align groups from both samples, count absent
  groups as zero, and calculate nonnegative per-group contributions from
  proportions. Zero counts now use an explicit rule: add 0.5 to every count by
  default, or floor proportions at `floor_value`. Nominal `template_matrix`
  groups are reused and new comparison categories are retained. Previously a
  category present only in the comparison sample made the reported PSI
  exactly 0, and per-group contributions could be negative. Expect PSI values
  to change: missing values now count as a group (WP3), so e.g. on `fintech`
  `education_level`, mostly missing from mid-2025, moves from 0.11 to 5.7.
* Nominal missing values now remain a `value_NA` group in contingency tables,
  including when infrequent values are merged. Shared grouping with
  `group_stats()` renames a genuine `value_NA` / `value_other` value with a
  unique suffix and a message when it would collide with the group the package
  creates (missing values present / infrequent values merged), without
  changing the input column.
* `loan_tbl()` rejects missing or duplicated declared application ids, reporting
  offending row counts and example values; loan and client ids may still repeat.
* `group_stats()` computes approval rates and canonical status counts only with
  an explicit status `value_map()`. Unmapped statuses retain raw counts and a
  message requesting a map. Without status it returns outcome statistics only,
  never inferring rejection from missing outcomes. Its plot method handles
  absent rates and `stats = character()`.
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
* A `value_map()`'s `.default` must be a label of the role's vocabulary, like
  the mapped labels (`.default = "maybe"` was accepted for a status).
* `contingency_table()` rejects a raw status that spells a canonical label but
  is mapped to a different one (raw `"approved"` mapped to `rejected`), which
  made `count_approved` ambiguous. Mapping it to the label it spells is fine.
* A `template_matrix` re-bins a numeric variable into the template's own
  left-closed intervals `[a, b)`; values lying on a cut point used to move to
  the neighbouring bin. This inflated `psi()` for every numeric variable (a
  sample compared with itself gave 0.024 instead of 0).

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
