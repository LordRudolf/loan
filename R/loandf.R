#' Create a data frame object easily recognized by the other `loan` package functions with the relevant credit-loan-portfolio-indicative.
#'
#' @description
#' The function takes an existing data frame containing columns that are typically available for the loan portfolio data sets
#' (e.g., loan status, application created_at, client id, first payment due, etc.) and turns them into a new data frame object recognized by the other
#' `loan` package functions for the automatic and easy data analysis.
#'
#' @param df A data frame or matrix of the credit loan/application portfolio data set where each row indicates individual loans or applications but the columns -
#' all the information related to the corresponding loan/application including, but not limited to, observations data (characteristics available before taking a loan issuance decision),
#' client/application/loan id fields, created at or registered at time stamp fields, current days passed due, and more.
#' @param client_id The column name that contains unique client identification number. Leave it NA if not available.
#' @param application_id The column name that contains unique loan application identification number. Leave it NA if not available.
#' @param loan_id The column name that contains unique loan identification number. Leave it NA if not available.
#' @param application_created_at The column name that indicates the application's creation datetime. For a higher accuracy, it is recommended
#' that the column is `POSIXct` class but `Date` class will also work. Leave it NA if not available.
#' @param data_downloaded_at The data set acquisition date. May be used for the forecasting related analysis where it becomes relevant. The default is the current date.
#' @param application_status The column name that indicates the loans applications' statuses. The column must be a character or factor class. The recognized
#' application statuses: \n
#'  * `ISSUED`: The application has been accepted resulted in an issued loan. \n
#'  * `REJECTED`: The application has been rejected due to the internal lending rules and/or the scorecard. \n
#'  * `NOT_TAKEN_UP`: The client application has been approved by the lending processes but the client hasn't approved herself the final offer resulting in the application cancellation. \n
#'  * `WAITING_FOR_APPROVAL`: Similarly as `NOT_TAKEN_UP` the client has been approved by the lending rules but the loan hasn't been issued yet because the client hasn't accepted the
#'  final offer. The difference is that `WAITING_FOR_APPROVAL` applications still can be approved by the client resulting in `ISSUED` (if the client accepts) status or
#'  `NOT_TAKEN_UP` (client rejects the final offer or the offer expires) status. \n
#'  * `CANCELLED`: The client has started creating the application but it was cancelled (most often, unfinished; therefore, not submitted) even before the evaluation. \n
#'  * `IN_PROCESS`: The application currently is being evaluated and, in the near future, the status will change to the `REJECTED` (if it did no pass the internal evaluation),
#'  `WAITING_FOR_APPROVAL` (if it passed the internal evaluation and the offer needs to be accepted by the client), or `ISSUED` (if it passed the internal evaluation and additional
#'  offer acceptance by the client is not required).
#'  @param loan_status The column name that indicates the issued loans statuses. The column must be a character or factor class. The recognized
#' loan statuses: \n
#'  * `ACTIVE`: Currently performing loan. \n
#'  * `CLOSED`: Either paid or written of loans. \n
#'  * `PAID`: Paid off loans Are not expecting payments in the future. \n
#'  * `WRITTEN_OFF`: Written off loan. \n
#'  * `VOIDED`: Manually closed loans or other types of loans which were open due to mistake. The loans with these statuses will be excluded from the analysis. \n
#'  * `RESTRUCTURED`: A loan that may still be active but  \n
#'
#' @return A `loan_df` object, which can be easily used with the other `loan` package functions. A `loan_df`object is also a data frame.
#'
#' @export
#' @examples
#' Lalallaaaa
#' a <- 1:100r find

loan_df <- function(df,
                    client_id = NA,
                    application_id = NA,
                    loan_id = NA,
                    application_created_at = NA,
                    application_status = NA,
                    loan_status = NA,
                    target = NA,
                    workflow = NA,
                    data_downloaded_at = Sys.time()) {

  ###########
  #Verify if the variable names are correct and are available

  stopifnot(any(c('POSIXt', 'POSIXct', 'Date') %in% class(data_downloaded_at)))

  x_names <- colnames(df)
  names_to_check <- c(client_id, application_id, loan_id, application_created_at, application_status, loan_status)
  sapply(names_to_check, function(x) check_name_existance(x, x_names))

  #And more checks
  # Verify timestamps
  names_to_check <- c(application_created_at)
  sapply(application_created_at, function(xx) check_timestamp_fields(xx, df))

  #Application IDs must be unique and not missing if given
  if(!is.na(application_id)) {
    stopifnot(!anyNA(df[[application_id]]))
    stopifnot(!any(duplicated((df[[application_id]]))))
  }

  #Application status match
  if(!is.na(application_status)) {

    stopifnot(!anyNA(df[[application_status]]))

    apps_statuses <- c('ISSUED', 'REJECTED', 'NOT_TAKEN_UP', 'WAITING_FOR_APPROVAL', 'CANCELLED', 'IN_PROCESS')
    unique_app_statuses <- unique(df[[application_status]])

    unrecognised_app_statuses <- unique_app_statuses[!unique_app_statuses %in% apps_statuses]
    unmatched_statuses <- apps_statuses[!apps_statuses %in% unique_app_statuses]

    if(length(unmatched_statuses) == length(apps_statuses)) {
      stop('None of the application statuses were identified as valid. Please, check the help of the function loan_df() for more information.')
    }

    if(length(unrecognised_app_statuses) > 0) {
      warning(paste0('The following application statuses has not been recognised and all of them will be treated as REJECTED: ', paste(unrecognised_app_statuses, collapse = ', ')))
    }

    if(any(unmatched_statuses %in% c('ISSUED', 'REJECTED'))) {
      warning('Possibly, an important application status has not been identified (could not recognise ISSUED and/or REJECTED statuses). You may want to double check your application
              status namings. See the help of the function loan_df() for more information.')
    }
  }

  ## TO DO: FINISH THIS
  #Loan status match
  if(!is.na(loan_status)) {

  }

  ###########
  #Create attributes here

  # aliases <- list2env(
  #   list(
  #     ## id_fields
  #     client_id = df[['client_id']],
  #     application_id = df[['application_id']],
  #     loan_id = df[['loan_id']],
  #
  #     ## timestamps
  #     application_created_at = df[['application_created_at']],
  #
  #     ## status fields
  #     application_created_at = df[['application_created_at']]
  #   ),
  #   parent = emptyenv()
  # )

  aliases <- create_aliases(
    df,

    ## id_fields
    client_id = client_id,
    application_id = application_id,
    loan_id = loan_id,

    ## timestamps
    application_created_at = application_created_at,

    ## status fields
    application_status = application_status,

    ## outcome fields
    target = target
  )


  ## TO DO: predictors shall be as an argument that the user may provide
  aliases_names <- names(aliases)
  predictors <- colnames(df)[!aliases_names %in% colnames(df)]
  coltypes <- sapply(df[, predictors], class) %>% unlist()
  predictors <- predictors[coltypes %in% c('character', 'factor', 'numeric', 'integer')]
  predictors <- predictors[!is.na(predictors)]

  df <- new_loan_df(
    df,
    data_downloaded_at = data_downloaded_at,
    predictors = predictors,
    aliases = aliases
  )

  #available_attributes <- generate_attribute_list(df)

  ## Checks whether all the attributes are logical
  # if(any(!is.na(available_attributes$loan_id) & available_attributes$application_status != 'ISSUED' )) {
  #   stop('There are rows that has loan_id but the application status is not "ISSUED". There are no possible loans if the application status is not "ISSUED"')
  # }
  # if(any(is.na(available_attributes$loan_id) & available_attributes$application_status == 'ISSUED')) {
  #   stop('There are rows with the application status "ISSUED" but without loan IDs. There must be a loan id if the application is "ISSUED".')
  # }
  #
  # if(any(is.na(available_attributes$loan_id) & !is.na(available_attributes$loan_status) )) {
  #   stop('There are rows that have loan_status but does not have loan_id. Each loan with a status must have an ID as well.')
  # }
  # if(any(!is.na(available_attributes$loan_id) & is.na(available_attributes$loan_status) )) {
  #   stop('There are rows that have loan_id but does not have loan status. Each loan must have a status.')
  # }

  return(df)
}

new_loan_df <- function(df = data.frame(),  predictors = NA,
                        aliases = environment(), data_downloaded_at = NA) {
  stopifnot(is.data.frame(df))
  stopifnot(is.environment(aliases))

  structure(
    tibble::as_tibble(df),
    class = c('loan_df', class(df)),
    data_downloaded_at =  data_downloaded_at,
    predictors = predictors,
    aliases = aliases
  )
}

print.loan_df <- function(df, ...) {

  cat('Loans data set contains ', nrow(df), ' rows and ', ncol(df), ' columns. \n \n')
  non_na <- unlist(lapply(attributes(df)[["column_markers"]], function(df) names(df)[!is.na(df)]))
  if(length(non_na) > 0) {
    cat(paste0('With the following column markers: ', paste(non_na, collapse = ', '), '\n \n'))
    if(any(non_na == 'application_created_at')) cat(paste0('Application_created_at quantiles: ', paste0(
      as.Date(summary(df[, attributes(df)[["column_markers"]][['timestamp_fields']][['application_created_at']]])), collapse = ', '
    ), '\n \n'))
  } else {
    cat('There are no column  markers.\n \n')
  }

  cat('The data downloaded at ', as.character(attributes(df)[['data_downloaded_at']]), '\n \n')

  print(tibble::as.tibble(df)) #!!!

  invisible(df)
}

`$.loan_df` <- function(df, name) {
  aliases <- attr(df, "aliases")
  if (!is.null(aliases) && exists(name, envir = aliases)) {
    # If the name exists in aliases, return it

    get(name, envir = aliases)
  } else {
    # Otherwise, use the default behavior
    df[[name]]
  }
}


####### Helper functions below
# Create an environment and set up active bindings
create_aliases <- function(df, ...) {

  # Create a closure that captures the current column
  create_binding <- function(env, name, df, column) {
    # Create a closure that captures the current column
    binding_function <- local({
      col <- column
      function() df[[col]]
    })
    makeActiveBinding(name, binding_function, env)
  }

  env <- new.env(parent = emptyenv())
  aliases <- list(...)
  for (alias in names(aliases)) {
    create_binding(env, alias, df, aliases[[alias]])
  }

  return(env)
}

