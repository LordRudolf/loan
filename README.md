# loan

Loan portfolio analysis and credit risk management in R.

`loan` analyses a lender's application funnel and loan book together. Every row
is one closed application -- approved, rejected or cancelled -- so approval
rates, outcomes, population stability and (in time) reject inference are studied
on the same data.

## The idea in one example

Declare the roles of your columns once:

```r
library(loan)
data(fintech)

lt <- loan_tbl(
  fintech,
  application_id         = "application_id",
  loan_id                = "loan_id",
  client_id              = "client_id",
  application_created_at = "app_created_at",
  application_status     = value_map("application_status",
                                     approved  = "LOAN_ISSUED",
                                     rejected  = "REJECTED",
                                     cancelled = "CANCELLED"),
  outcomes = list(
    fpd15           = binary_outcome("fpd15", bad = 1),
    fpd31           = binary_outcome("fpd31", bad = 1),
    defaulted       = binary_outcome("loan_status", bad = c("DEFAULTED", "WRITE_OFF")),
    dpd             = "current_dpd",
    extension_times = "extension_times",
    cash_in         = "cash_in"
  ),
  primary_outcome        = "fpd15",
  supplementary          = c("app_close_reason", "cash_out", "principal_disbursed")
)
# every other column is assigned automatically; loan_tbl() reports how
```

and every analysis function fills in its own arguments:

```r
group_stats(lt, "client_age")                         # approval rate, bad rate per age group
group_stats(lt, "client_age", outcome = "defaulted")  # another declared outcome
contingency_table(lt, "gender")
psi(lt, "education_level", time_split_base = ..., time_split_comparison = ...)
plot_univariate_smooth(lt, "client_age")
```

The same functions also take bare vectors, or a plain data frame with column
names. See `vignette("getting_started", package = "loan")`.

## Status

The data contract (`loan_tbl()`, `value_map()`, `binary_outcome()`) and the
analysis functions above are the core that further tools are built on.
`dynamic_stats()` and the modelling layer are experimental and being redesigned.

## Installation

```r
# install.packages("devtools")
devtools::install_github("LordRudolf/loan")
```
