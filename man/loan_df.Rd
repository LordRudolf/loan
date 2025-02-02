% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loandf.R
\name{loan_df}
\alias{loan_df}
\title{Create a data frame object easily recognized by the other `loan` package functions with the relevant credit-loan-portfolio-indicative.
It is designed to work seamlessly with aliases defined using the `define_aliases` function, facilitating consistent data analysis workflows in credit portfolio analysis.}
\usage{
loan_df(df, aliases = list(), workflow = NA, data_downloaded_at = Sys.time())
}
\arguments{
\item{df}{A data.frame or tibble containing the loan portfolio dataset. This dataset should include columns that correspond to the aliases defined in the `aliases` argument.}

\item{aliases}{A list of alias mappings created using the `define_aliases` function. This list specifies how columns in `df` should be renamed or transformed to fit standardized analysis templates. The `aliases` argument can directly take the output of `define_aliases`, which allows for a flexible and powerful way to manage dataset preprocessing. For more information on how to use `define_aliases`, see its detailed documentation and examples.}

\item{data_downloaded_at}{The timestamp indicating when the data was downloaded or last updated. This parameter defaults to the current system time but can be overridden to reflect the actual data acquisition time.}
}
\value{
A `loan_df` object, which can be seamlessly used with the other `loan` package functions under standardized column names.
}
\description{
The function takes an existing data frame containing columns that are typically available for the loan portfolio data sets
(e.g., loan status, application created_at, client id, first payment due, etc.) and turns them into a new data frame object recognized by the other
`loan` package functions for the automatic and easy data analysis.
}
\details{
The `loan_df` function is part of a suite of tools designed for credit portfolio analysis. It leverages the `aliases` defined by the user to standardize dataset columns, making it easier to apply consistent analysis and modeling techniques across different datasets. The function evaluates the `aliases` within the context of the provided dataset, applying the necessary transformations and mappings.

The integration with `define_aliases` allows for a flexible and powerful way to manage dataset preprocessing, enabling analysts to define complex mappings and transformations in a structured and reusable manner.
}
\examples{
# Load the fintech dataset included with the package
data(fintech)

# Transform 'fintech' into a 'loan_df' object with standardized aliases
df <- loan_df(fintech,
             define_aliases(
               application_status = list(application_status = list(ACCEPTED = 'LOAN_ISSUED', 
                                                                   REJECTED = c('REJECTED'),
                                                                   CANCELLED = 'CANCELLED',
                                                                   IN_PROCESS = c('APPROVED', 'PENDING'))),
               loan_status = list(loan_status = list(
                 PAID = 'PAID_OFF',
                 DEFAULTED = 'DEFAULTED',
                 VOIDED = 'VOIDED',
                 ACTIVE = c('PERFORMING', 'PAST_DUE')
               )),
               days_past_due_outcome = c('fpd'),
               application_id = c('application_id'),
               loan_id = c('loan_id'),
               client_id = c('client_id'),
               application_created_timestamp = c('app_created_at')
             ),
             data_downloaded_at = as.Date('2021-10-30')
)

}
