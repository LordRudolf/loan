
#' Population stability index from two contingency tables
#'
#' The low-level form of [psi()], for tables you have already built.
#'
#' @param cont_table_prev,cont_table The base and comparison `loan_cont_table`s.
#'   Groups present in either table are aligned; absent groups have zero counts,
#'   and groups empty in both are dropped.
#' @inheritParams psi
#' @return A `loan_psi`.
#' @export
psi_from_tables <- function(cont_table_prev, cont_table, measure = 'issued_loans_total',
                            var_name = 'variable', zero_counts = c('add_half', 'floor'),
                            floor_value = 1e-4) {
  if(identical(zero_counts, c('add_half', 'floor'))) zero_counts <- 'add_half'
  if(!is.character(zero_counts) || length(zero_counts) != 1L ||
     is.na(zero_counts) || !zero_counts %in% c('add_half', 'floor')) {
    stop('`zero_counts` must be "add_half" or "floor" -- got ',
         paste(zero_counts, collapse = ', '), '.', call. = FALSE)
  }
  if(!is.numeric(floor_value) || length(floor_value) != 1L ||
     !is.finite(floor_value) || floor_value <= 0 || floor_value >= 1) {
    stop('`floor_value` must be one finite number between 0 and 1 -- got ',
         paste(floor_value, collapse = ', '), '.', call. = FALSE)
  }

  tables <- list(cont_table_prev = cont_table_prev, cont_table = cont_table)
  for(table_name in names(tables)) {
    tab <- tables[[table_name]]
    if(!inherits(tab, 'loan_cont_table') || !'the_var' %in% names(tab) ||
       !is.character(measure) || length(measure) != 1L ||
       !measure %in% names(tab)) {
      stop('`', table_name, '` must be a `loan_cont_table` with `the_var` and ',
           'the `measure` column -- got an incompatible table.', call. = FALSE)
    }
    groups <- as.character(tab$the_var)
    counts <- tab[[measure]]
    if(anyNA(groups) || anyDuplicated(groups) || !is.numeric(counts) ||
       anyNA(counts) || any(!is.finite(counts)) || any(counts < 0)) {
      stop('`', table_name, '` must have unique, non-missing groups and finite, ',
           'non-negative `measure` counts -- got incompatible groups or counts.',
           call. = FALSE)
    }
    if(sum(counts) == 0) {
      stop('`', table_name, '` must have a positive total for `measure` -- got zero.',
           call. = FALSE)
    }
  }

  groups <- union(as.character(cont_table_prev$the_var), as.character(cont_table$the_var))
  base <- cont_table_prev[[measure]][match(groups, as.character(cont_table_prev$the_var))]
  comparison <- cont_table[[measure]][match(groups, as.character(cont_table$the_var))]
  base[is.na(base)] <- 0
  comparison[is.na(comparison)] <- 0
  ## a group empty in both samples carries no information; smoothing it would
  ## add a spurious contribution whenever the two samples differ in size
  informative <- base > 0 | comparison > 0
  groups <- groups[informative]
  base <- base[informative]
  comparison <- comparison[informative]

  if(zero_counts == 'add_half') {
    p_base <- (base + 0.5) / sum(base + 0.5)
    p_comparison <- (comparison + 0.5) / sum(comparison + 0.5)
  } else {
    p_base <- pmax(base / sum(base), floor_value)
    p_comparison <- pmax(comparison / sum(comparison), floor_value)
  }
  tt <- data.frame(the_var = groups, count_base = base,
                   count_comparison = comparison, p1 = p_comparison, p2 = p_base,
                   rate_diff = p_comparison - p_base,
                   ratio = log(p_comparison / p_base))
  tt$PSI <- tt$rate_diff * tt$ratio

  structure(
    sum(tt$PSI),
    class = c('loan_psi', 'numeric'),
    PSI_table = tt,
    var_name = var_name,
    measure = measure,
    zero_counts = zero_counts,
    floor_value = floor_value
  )
}


check_psi_args <- function(variable, time_split) {

  if(!any(is.logical(time_split), is.integer(time_split))) {
    stop('The time_split base or time_split_comparison variables must be either logical or integer vectors')
  }
  
  l <- length(variable)
  
  if(is.logical(time_split)) {
    if(length(time_split) != l) {
      stop('The time_split, if logical, must match with the length of the variable')
    }
  }
  
  if(is.integer(time_split)) {
    if(any(time_split) < 1) {
      stop('The time split,if integer, cannot hold negative numbers')
    }
    if(max(time_split) > l) {
      stop('The time split, if integer, cannot have larger elements than the length of the variable.')
    }
  }
}


#' Population stability index of a variable
#'
#' Compares a variable's distribution between a base and a comparison sample
#' -- typically two time periods: the sum over groups of
#' `(p_comparison - p_base) * log(p_comparison / p_base)`. The variable is
#' grouped on the base sample and the comparison sample reuses those groups, so
#' both are binned identically; new comparison groups are also included.
#' Missing groups have zero counts; groups empty in both samples are dropped.
#' By default, 0.5 is added to every count
#' before proportions are computed. With `zero_counts = "floor"`, raw
#' proportions are floored at `floor_value` instead. Accepts the same three
#' input forms as [contingency_table()].
#'
#' @inheritParams contingency_table
#' @param ... In the data-frame form, passed on to the vector form: the time
#'   splits, `measure`, `breaks`.
#' @param zero_counts How to handle zero groups: `"add_half"` adds 0.5 to every
#'   group's count in both samples before computing proportions (default);
#'   `"floor"` floors each unadjusted proportion at `floor_value`.
#' @param floor_value Positive proportion floor for `zero_counts = "floor"`
#'   (default `1e-4`).
#' @return A `loan_psi`: the sum of nonnegative per-group contributions, with
#'   the group table as its `PSI_table` attribute. That table includes aligned
#'   `count_base` and `count_comparison` columns (zero for absent groups).
#'   The `zero_counts` and `floor_value` attributes record the chosen rule.
#' @examples
#' data(fintech)
#' base <- as.Date(fintech$app_created_at) <  as.Date("2024-04-01")
#' comp <- as.Date(fintech$app_created_at) >= as.Date("2024-07-01")
#' psi(fintech, "gender", outcome = binary_outcome("fpd15", bad = 1),
#'     time_split_base = base, time_split_comparison = comp)
#' @export
psi <- function(x, ...) {
  if(!is.data.frame(x)) check_variable(x)
  UseMethod('psi')
}

#' @rdname psi
#' @export
psi.data.frame <- function(x, variable, outcome = NULL, application_status = NULL, ...) {

  r <- resolve_roles(x,
                     variable           = variable,
                     outcome            = outcome,
                     application_status = application_status,
                     .required = c('variable', 'outcome'))

  var_label <- if(is.character(variable) && length(variable) == 1L) variable else 'variable'

  psi(r$variable,
      check_outcome(r$outcome, attr(r, 'maps')$outcome, types = c('binary', 'multiclass')),
      ...,
      var_name           = var_label,
      application_status = r$application_status,
      status_map         = attr(r, 'maps')$application_status)
}


#' @rdname psi
#' @param time_split_base,time_split_comparison Rows of the base and the
#'   comparison sample: logical vectors with one element per row, or integer
#'   row indices.
#' @param measure The count compared between the samples:
#'   `"issued_loans_total"` (default: applications with a known outcome),
#'   `"applications_total"` (all applications; needs `application_status`), or
#'   any other count column of the [contingency_table()], e.g.
#'   `"count_rejected"`.
#' @param var_name Label stored in the result; the data-frame form sets it to
#'   the column name.
#' @param requires_verification Check the inputs first (default `TRUE`).
#' @export
psi.character <-
  psi.factor <-
  psi.logical <-
  psi.numeric <- function(x, outcome, time_split_base, time_split_comparison,
                          measure = 'issued_loans_total', 
                          var_name = 'variable',
                          application_status = NULL, 
                          ..., 
                          requires_verification = TRUE,
                          zero_counts = c('add_half', 'floor'), floor_value = 1e-4) {
  
  if(requires_verification) {
    check_psi_args(x, time_split_base)
    check_psi_args(x, time_split_comparison)
    check_same_length(x, outcome = outcome, application_status = application_status)
    
    if(is.null(application_status) && measure == 'applications_total') {
      stop('You need to provide the application_status to apply the PSI on application counts')
    }
  }
  
  ## canonicalise once here, not in each of the two contingency tables below
  outcome <- check_outcome(outcome, types = c('binary', 'multiclass'))

  the_base_var <- x[time_split_base]
  if(is.numeric(the_base_var)) the_base_var <- bin_variable(the_base_var, ...)
  
  cont_table_prev <- contingency_table(the_base_var, 
                                               outcome[time_split_base], 
                                               application_status = application_status[time_split_base],
                                               ...)
  
  cont_table <- contingency_table(x[time_split_comparison], 
                                          outcome[time_split_comparison], 
                                          application_status = application_status[time_split_comparison], 
                                          template_matrix = cont_table_prev, 
                                          ...)

  psi_from_tables(cont_table_prev, cont_table,
                      measure = measure,
                      var_name = var_name,
                      zero_counts = zero_counts,
                      floor_value = floor_value)
}

#' @export
print.loan_psi <- function(x, ...) {
  print(unclass(x), ...)
  invisible(x)
}
