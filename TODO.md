# TODO — planned additions to the `loan` package

Deferred work and future-release ideas. Not a bug tracker for in-progress
work (that lives in AGENTS.md → "Known debts") and not a plan of record —
items here are candidates, ordered loosely by area, newest area last.

Format: `**Title** — what, and why it is deferred.`

## Application statuses / funnel

- **Non-closed application statuses** — the package currently assumes every
  row is a *closed* application and supports exactly three core statuses
  (`approved`, `rejected`, `cancelled`). Add `in_progress` and the other
  non-final statuses institutions use (`on_hold`, `redirected`, `error`,
  `pending`, …). Blocked on deciding how open applications enter each
  formula: they must be excluded from approval-rate denominators or recent
  vintages read as a drop in acceptance.
- **`approved` vs `disbursed` as separate stages** — model borrower-side
  attrition between the lender's approval and money actually moving
  (counter-offers, abandonment, take-up rate = disbursed / approved).
  Today an approved application the borrower walked away from is expected
  to be recorded as `cancelled`.
- **Configurable approval-rate denominator** — whether `cancelled`
  applications belong in the denominator depends on whether you want the
  risk-policy view (exclude) or the commercial funnel view (include).
  Currently a fixed choice; make it a parameter.
- **`accepted_at_lower_tier` handling** — institutions that down-sell to a
  cheaper product. Maps to `approved` under the two-level model, but the
  break-out deserves first-class reporting.

## Outcomes

- **Multi-class, ordinal and numeric outcomes** — only binary
  (`good`/`bad`, via `binary_outcome()`) is implemented. The design is
  settled: add `numeric_outcome()` / `multiclass_outcome()` declarations and
  extend `check_outcome()` types when the first function needs them.
- **Output naming of outcomes** — how outcomes appear in output tables:
  canonical only (`count_bad`, current), prefixed by the outcome name
  (`fpd15_count_bad`), or with a raw-value breakdown like application
  status. Decide before multi-outcome tables exist.
- **Outcome-window / maturity handling** — a disbursed loan that has not
  matured yet has an NA outcome and is currently indistinguishable from a
  loan with no outcome at all.

## Redesign sessions (set aside, not patched piecemeal)

- **`dynamic_stats()`** — statistics over many time splits; see AGENTS.md
  "Known debts" for the inputs to that session.
- **Modelling layer** — `evaluate_features()`, `fit_model()`, `cv_folds()`,
  `auto_recipe()`, `step_woebin()`; onto `loan_tbl` roles, `outcome`, and
  `resolve_roles()` / `check_outcome()`.

## Aggregation levels

- **Client-level and disbursement-level aggregation** — analysis is
  application-level only. Support summarising by client (one client, many
  loans over a lifetime) and per new-money disbursement (line-of-credit
  top-ups, where one loan has many withdrawals each with its own decision).

## Analytics to add

- **Reject inference module** — designed fresh; the old exploration script
  was deleted (see git history for `rejection_inference_measures.R`).
  Includes the unknown-label-rate and marginal-bad-rate-by-acceptance-rate
  comparisons between an old and a new scorecard.
- **Marginal effect / profit plot** — profit by score threshold; option to
  adjust bad rates and lifetime values per cutoff for economic conditions
  not captured by the scorecard.
- **Survival analysis: LGD by score bucket.**
- **PSI with traffic-light thresholds** — banded severity rather than a
  bare number.
- **Retention rates between loan sequence numbers** — applications created
  and loans disbursed, from one sequence to the next.
- **Information Value (IV)** alongside WOE.

## Naming / API

- **Column and output-label naming pass** — `fisher_p_val`,
  `issued_loans_total` (really "rows with a known outcome", not "disbursed"),
  `issuance_rate` (actually an approval rate), the `PSI_table` attribute,
  and the `list(PSI = psi)` dimname (`GOOD`/`BAD` counts are already
  canonical `count_good`/`count_bad`). Deferred
  as one coordinated pass so the package does not trade one inconsistency
  for another.
- **tidyselect / NSE column references** — `group_stats(df, client_age,
  outcome = class)` alongside the string form.

## Infrastructure

- **Roxygen-generated NAMESPACE** — currently hand-written; every new S3
  method must be registered by hand or it silently fails to dispatch.
- **testthat suite** — no automated tests yet; `development.R` is the
  manual walkthrough.
- **`utils::globalVariables()`** for NSE column-name check NOTEs.
- **tidymodels and base-R (GLM) training frameworks** — `fit_model()`
  supports caret only.
