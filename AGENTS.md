# AGENTS.md — loan package

Guidance for AI agents (and humans) working on this repository.

## What this package is

`loan` is an R package for loan portfolio analysis and credit risk management:
automated portfolio monitoring (contingency tables, group stats, WOE/IV, PSI,
dynamic stability), scorecard modelling helpers, reject inference, and
integrations with tidymodels/recipes and caret. Tidyverse-style syntax.
Long-term goal: implement analytic tools from scientific literature not
available in general R packages.

**Current phase: core architecture first.** Do not add new analytic tools
until the core (data contract, argument resolution, S3 classes, naming) is
stable. CRAN compliance is explicitly NOT a goal right now — but
`devtools::load_all()` and `devtools::check()` must not error.

## Domain model (read before touching anything)

The lending funnel: client → application → (rejected | cancelled | counter-offer
| approved) → disbursement → repayment behaviour → (default | paid) → repeat
loans / top-ups. Consequences for the package:

- **Row = one CLOSED application.** The package currently expects every row to
  be a *finalised* application; non-final statuses (`in_progress`, `on_hold`,
  ...) are not yet supported — see TODO.md. Only `application_id` is
  guaranteed unique. `loan_id` and `client_id` may duplicate (line-of-credit
  withdrawals, repeat clients). Aggregation levels: application (default),
  loan, client, disbursement.
- **Rejected/cancelled applications stay in the data.** They have no outcome
  (NA) but must appear in issuance-rate columns and reject-inference tools.
  Never silently drop NA-outcome rows.
- **Column roles** (declared with `loan_tbl()`): ids (application / loan /
  client), timestamps (application created, first delay), application status,
  outcomes (several allowed, one primary; binary implemented, multi-class /
  ordinal / numeric planned — a loan status is an outcome, not a role of its
  own), predictors, and supplementary (kept but never analysed: other ids,
  other timestamps, bookkeeping). Every column holds exactly one role.
- **Time trends matter** (PSI, stability reports) — time grouping must stay
  flexible (lubridate period strings, explicit date ranges, or user vectors).
- **NA values in features are a legitimate category** (`value_NA` group), not
  something to impute away in descriptive analysis.

## Repository layout

- `R/` — package code. The **data contract** is three files, one concept each:
  - `loan_tbl.R` — the roles CLASS: `loan_tbl()`, `print`, `is_loan_tbl`,
    the accessors `loan_roles()` / `primary_outcome_name()`, and the
    `unlisted_role` auto-assignment with its near-copy screen.
    Roles: `application_id`, `loan_id`, `client_id`, `application_created_at`,
    `first_delay_at`, `application_status`, `outcomes` + `primary_outcome`,
    `predictors`, `supplementary`. Every column gets exactly one role:
    unlisted columns get the role chosen by `unlisted_role` — `"predictors"`,
    `"supplementary"`, or `"auto"` (default): analysable types (numeric,
    logical, nominal) -> predictors, others -> supplementary, and an
    auto-predictor that is a near-copy (> 0.95) of a column LISTED as an id,
    timestamp, outcome or predictor -> supplementary (`near_copies()`;
    |Pearson r| for numeric pairs, Tschuprow's T when either side is nominal —
    never eta / Cramer's V, which flag nested columns like city -> province).
    `unlisted_role` only ever touches unlisted columns. Declared
    predictors of an unanalysable type are kept with a warning. There is no
    `loan_status` role — a loan status is declared as an outcome
    (`binary_outcome("loan_status", ...)`).
  - `arguments.R` — how every analysis function takes its ARGUMENTS, one
    path, one set of messages: `resolve_roles()` (the ONE place a column
    name / vector / declaration becomes values; every `.data.frame` method
    calls it), then `check_variable()` (via `is_variable_type()`),
    `check_outcome()` (returns a binary outcome as the canonical `good` /
    `bad` factor; type detection via `detect_outcome_type()`),
    `check_same_length()` and `frame_with_status()` for the vector form.
  - `value_map.R` — VOCABULARIES: `value_map()`, `binary_outcome()`,
    `loan_vocabulary()`, `check_vocabulary()`, `apply_value_map()`, and the
    fallbacks used when nothing was declared: `detect_status_map()`,
    `detect_binary_outcome()` (both warn when they have to guess).

  The analysis core:
  - `contingency_table.R` — `contingency_table()` S3 generic; builds
    `loan_cont_table` (data.frame subclass with `target_var_dict`,
    `app_status_dict` (incl. the status map), `cuts` attributes). Emits raw
    `count_<raw status>` columns AND canonical `count_approved` /
    `count_rejected` / `count_cancelled` plus `decisioned_total`.
    The central building block.
  - `group_stats.R` — `group_stats()` (`loan_group_stats` class,
    plot method).
  - `measures.R` — per-bin measures on `loan_cont_table`: `add_woe()`,
    `add_fisher_p()`.
  - `psi.R` — `psi()`, `psi_from_tables()` (`loan_psi` class).
  - `dynamic_stats.R` — `dynamic_stats()` time-split loops. **Set aside for
    redesign** (see Known debts).
  - `discretize.R` — variable prep: `discretize_values()`,
    `group_infrequent()`, `bin_variable()`.
  - `univariate_plots.R` — `plot_univariate_smooth()` (S3), `plot_density()`
    stub.
  - `utils.R` — package-level help page (`?loan`), pipe import, `auroc()`.

  The modelling layer (**set aside for redesign**):
  - `preprocessing.R` — `auto_recipe()` recipe builder.
  - `recipes.R` — recipe step definitions (`step_woebin`, wraps
    `scorecard::woebin`); future steps land here too.
  - `cross_validation.R` — `cv_folds()` and resampling helpers.
  - `scorecard_training.R` — `fit_model()`.
  - `evaluate_features.R` — `evaluate_features()` + `plot_*()`.
  - `utils.R` — pipe import, `auroc()`, small generic helpers.
- Dev scripts, NOT package code (never add such files to `R/`; dev-purpose
  code that stops being useful is deleted, git history is the archive):
  `development.R` (manual test walkthrough — the de-facto spec of intended
  syntax), `dataset_preps.R`, `private_dev.R`,
  `feature_evaluation_presentation.R`.
- `C:\Users\Rudolfs\Desktop\R_scripts\loan_old` — previous implementation
  attempt. Reference only; never edit it.

## History / lessons learned

Commit 91ae006 "Major simplification. Not using loan_df data class which
halted the development": the first `loan_df` design (alias mappings with NSE
evaluation, `$` overloading with `.alias` dot-prefix lookup, value remapping
inside aliases) was too clever and stalled the project. Any revival of a
portfolio data class must be radically simpler: store column **roles** as
plain metadata; no `$` magic, no NSE evaluation of alias lists. Value
remapping is allowed but must be **explicit** — declared once via
`value_map()`, inspectable, and never hidden inside an accessor; it never
rewrites the user's original column.

## Conventions

### Syntax philosophy (the package's key UX promise)

Every analysis function accepts progressively richer input, doing more
automation the more context it has:

1. Bare vectors: `f(x, outcome)`
2. data.frame + column names: `f(df, "x", outcome = "class")`
3. role-aware table: `f(loan_tbl(df, ...))` — roles resolved
   automatically.

Implement this with S3 dispatch on the first argument. All methods must funnel
into ONE workhorse so behaviour never diverges between input forms. Numeric
inputs are auto-discretized; character/factor pass through (infrequent levels
grouped); the workhorse operates on tabular (contingency) data.

### Naming

Function names follow a closed scheme — **nouns for functions that return an
object, verbs only for genuine actions**:

| Kind | Form | Examples |
|---|---|---|
| returns an object | bare noun | `contingency_table()`, `group_stats()`, `psi()`, `dynamic_stats()`, `cv_folds()` |
| adds a column to a loan object | `add_*()` | `add_woe()`, `add_fisher_p()` |
| draws | `plot_*()` | `plot_univariate_smooth()`, `plot_profit_curve()` |
| fits a model | `fit_*()` | `fit_model()` |
| infers a value from data (internal) | `detect_*()` | `detect_binary_outcome()`, `detect_outcome_type()` |
| validates (internal) | `check_*()` | `check_variable()`, `check_outcome()`, `check_psi_args()` |
| recipes step | `step_*()` | `step_woebin()` |

Rules: all lowercase `snake_case` — never `PSI`/`WOE` in a function or class
name. No `get_`/`produce_`/`calculate_`/`create_`/`visualize_` prefixes; they
carry no information. Avoid names generic enough to collide with a user's own
objects.

- S3 classes: `loan_` prefix (`loan_cont_table`, `loan_group_stats`,
  `loan_psi`).
- Standard argument names — **partly settled.**
  - **`x`** — the first argument of every S3 generic AND all of its methods
    (R requires them to match; see "S3 methods" below). It is a data frame /
    `loan_tbl` in the data-frame form, and the variable's values in the
    vector form.
  - **`variable`** — the single variable under analysis, wherever it is not
    the dispatch argument: `f(df, variable = "client_age")`, internal helpers
    (`bin_variable(variable)`, ...). Never `the_var` / `varname` / `var_name`.
    Several variables at once: **`variables`** (names only, never values).
  - Settled: `measure` (the PSI column being compared), `template_matrix`
    (a contingency table reused as a binning template).
  - **`outcome`** — the one outcome a function uses; never `target` /
    `binary_outcome` / `target_name`. It accepts a column name, a vector, a
    `binary_outcome()` declaration, or on a `loan_tbl` the name of a declared
    outcome; omitted, it is the `primary_outcome`. **`outcomes`** (plural) is
    what `loan_tbl()` declares — the same singular/plural pattern as
    `variable` / `variables`.
  - **Outcome type is a property of each declared outcome, not of the role
    name.** Declare it (`binary_outcome("fpd15", bad = 1)`); undeclared
    outcomes are detected at use time, with a warning when the bad class is
    guessed. Every function states the types it supports via
    `check_outcome(types = ...)`. Only binary is implemented; add
    `numeric_outcome()` / `multiclass_outcome()` with their first user.
  - **`data`** — the data argument of functions that only ever take a data
    frame (`evaluate_features()`, `fit_model()`); never `df` (it also masks
    `stats::df()`). Analysis functions are S3 generics on `x` instead.
  - **`application_status`** — everywhere, including `psi()`; never
    `application_status_name`.
  - `var_name` survives only as a display *label* in the PSI vector form and
    `psi_from_tables()`; it is not the variable.
- **Validating `variable` — one path, one set of messages.** Where a
  data frame is the first argument, `resolve_roles()` turns `variable` into
  values: a **length-1 character is a column name** (a 1-row data frame is
  the one ambiguous case and resolves that way), a full-length vector is the
  values themselves, anything else is an error. Then `check_variable()`
  checks the type (numeric / logical / character / factor unless a
  function narrows it; `is_variable_type()` is the one definition). Logical
  variables are grouped TRUE / FALSE like a nominal, via `.logical` methods. Generics call `check_variable()` before `UseMethod()`, so the vector
  form and the data-frame form fail identically. Never write a bespoke
  existence or type check for a variable. Document the column-name rule on
  every function that accepts both forms (`@inheritParams
  contingency_table`).
- **S3 methods** take exactly the generic's first argument name (`x`, or
  `object` for recipes' `bake`) and keep its `...`. Internal calls into a
  generic pass the dispatch argument **positionally** — a named first
  argument silently falls back to dispatching on whichever argument happens
  to come first.

#### Controlled vocabularies (data values, not identifiers)

All package-canonical labels are **lowercase snake_case**, like every other
name in the package. Institution-specific raw values are preserved as-is and
mapped onto a canonical label with `value_map()`; each raw value maps to
exactly one canonical label, and unmapped values are an error unless
`.default` is given.

| Role | Canonical labels |
|---|---|
| `application_status` | `approved`, `rejected`, `cancelled` |
| `outcome` (binary only for now) | `good`, `bad` — counted as `count_good` / `count_bad` (provisional: output naming of outcomes is an open decision) |

- `approved` = the lender's risk decision was positive. Never "accepted",
  which is ambiguous about whose decision it was.
- `cancelled` = closed for reasons **outside risk-policy control** — including
  a borrower who walks away after approval. Excluded from approval-rate
  denominators for this reason.
- `disbursed` is a *loan*-side term, not an application status: it names the
  population in which an outcome can be observed. Reserved for outcome-side
  column naming; never use "issued".
- Non-final statuses (`in_progress`, …) are out of scope — see TODO.md.
- Generated columns and output labels are NOT yet part of the settled scheme —
  `fisher_p_val`, `issued_loans_total`, `issuance_rate`, the `PSI_table`
  attribute and the `stats = c('woe', ...)` identifiers keep their current
  names until the column-naming pass (TODO.md).
- Generated columns: counts as `count_<label>`, totals as
  `applications_total` / `issued_loans_total`, rates as `*_rate`
  (`bad_rate`, `issuance_rate`), stats by their name (`woe`, `fisher_p_val`).
- Reserved group labels: `value_NA` (missing), `_OTHER_`/`value_other`
  (infrequent) — inconsistent with each other; unify in the naming pass.

### Code style

- Tidyverse style guide; `%>%` pipe (magrittr) for now.
- External packages: always `pkg::fun()` inside package code, except dplyr
  verbs and `%>%` which are imported.
- Never assume outcome or status count column names — read
  `bad_label` / `good_label` / `accept_label` from the `loan_cont_table`
  attributes (`binary_count_labels()`). `contingency_table.factor()` is the
  only place those names are chosen.
- Attributes carry metadata between functions; helper accessors preferred
  over raw `attributes(x)$...` in new code.
- Roxygen2 for all exported functions; `@export` on generics AND methods.
- No top-level executable code in `R/` files — scripts go in the project
  root or `dev/`.

### Testing / dev workflow

- `devtools::load_all()` must always work. Test data: `data(fintech)`
  (bundled) — `development.R` shows the canonical usage walkthrough.
- testthat (edition 3) is the target framework; tests live in
  `tests/testthat/` once introduced.
- **Documentation**: roxygen2 blocks in `R/` generate `man/` with
  `roxygen2::roxygenise(roclets = "rd")` — the `rd` roclet only, because
  `NAMESPACE` is still hand-written (roxygen refuses to overwrite it anyway).
  `vignettes/getting_started.Rmd` is the user-facing tour of the API; keep it
  in sync when a signature or role changes. `NEWS.md` records user-visible
  changes. `README.md` is the front page.
- Full check: `R CMD build .` then `R CMD check --no-manual loan_<version>.tar.gz`
  (run outside the repo). `devtools::check()` / `rcmdcheck` refuse to run on
  this machine without Rtools, which a pure-R package does not need. Expected
  result today: 0 errors, 1 WARNING (undocumented objects -- `exportPattern`
  exports every internal helper), 1 NOTE (the global `X` in `fit_model()`).
- Windows environment, RStudio project (`loan.Rproj`), git branches:
  `main` + `stage` (work happens on `stage`).

## Known debts (do not repeat, fix when touched)

- The modelling layer (`evaluate_features()`, `fit_model()`, `cv_folds()`,
  `auto_recipe()`) still uses `target` and hard-codes `'BAD'`/`'GOOD'`; it
  does not use `resolve_roles()` / `check_outcome()` or `loan_tbl` roles yet.
  **Deliberately set aside: it gets a dedicated redesign session** — do not
  patch it piecemeal.
- `NAMESPACE` is hand-written (`exportPattern` + explicit `S3method`/
  `importFrom` entries) — migrate to roxygen-generated. **Every new S3
  method must be added to `NAMESPACE` by hand until then**: since R 3.6
  dispatch does not find unregistered methods, an unregistered method
  silently fails for users even though it works when the file is `source()`d.
- `dynamic_stats()` is **deliberately set aside: it gets a dedicated
  redesign session**, like the modelling layer — do not patch it piecemeal.
  Known input for that session: over ALL predictors of a real portfolio it
  fails, because `discretize_values()` hands degenerate numeric columns (one
  value in a time split, mostly-zero counts, columns empty before a data
  source existed) to `arules::discretize()`, which errors ("Less than 2
  uniques breaks left"); low-cardinality numerics need treating as discrete.
  Its `stats_funcs` must accept `outcome =` (renamed from `target`).
- NSE column names (`Freq`, `the_var`, `value`, ...) need a
  `utils::globalVariables()` declaration to silence check NOTEs.
- `evaluate_features()` has a known indexing bug in the fold loop
  (`all_obs[-cv_folds[[f]], f] <- pred`, marked `##error here`); fix when
  the modelling layer is reworked.
- `fit_model()` references a global `X` in its `computation_load`
  calculation (only hit when `tuneLength`/`tuneGrid` given).
