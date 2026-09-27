#' Contingency table of a variable against the outcome
#'
#' Counts applications per group of a variable, broken down by application
#' status and by outcome. It is the building block of the feature analysis:
#' [group_stats()], [psi()] and the measures ([add_woe()], [add_fisher_p()])
#' all work on its result. A numeric variable is binned first; missing values
#' form their own group.
#' Missing nominal values form `value_NA`, including when infrequent values
#' are merged. A genuine value spelled `value_NA` is renamed with a unique
#' suffix (`value_NA_1`), with a message, when the variable also has missing
#' values -- only then would the two be confused; the original column is
#' unchanged.
#'
#' This single-variable function accepts three input forms, chosen by the first
#' argument:
#' * **a [loan_tbl()]** -- `contingency_table(lt, "client_age")`: the outcome,
#'   the application status and its value map come from the declared roles;
#' * **a data frame** -- `contingency_table(df, "client_age", outcome = ...)`;
#' * **vectors** -- `contingency_table(values, outcome_values)`.
#' The same values and options use the same calculation in each form.
#'
#' @param x A data frame (or [loan_tbl()]), or the values of the variable to
#'   analyse. S3 dispatches on this argument, which is why it is called `x`
#'   rather than `variable`: it is the variable itself only in the vector form.
#' @param variable Used when `x` is a data frame: the variable to analyse,
#'   given either as a **single column name** or as a vector of values with one
#'   element per row. A length-1 character is always read as a column name; a
#'   one-row data frame is the only ambiguous case and resolves that way too.
#'   The variable must be numeric (binned automatically), character or factor.
#' @param outcome The outcome: a column name, a vector of values, a
#'   [binary_outcome()] declaration, or -- on a `loan_tbl` -- the name of a
#'   declared outcome (default: the primary outcome). A binary outcome is
#'   counted as canonical `good` / `bad`; an undeclared one is mapped by
#'   inference, with a warning when the bad class has to be guessed.
#' @param application_status Optional application status: a column name, a
#'   vector, or a [value_map()] naming its column (on a `loan_tbl`: the
#'   declared status). With it, the table adds raw `count_<status>` columns,
#'   `applications_total`, and canonical `count_approved` / `count_rejected` /
#'   `count_cancelled` with `decisioned_total` (approved + rejected).
#'   A raw status that spells a canonical label must map to that same label;
#'   mapping it to a different label is an error because the count names conflict.
#' @param ... Passed on to the method, e.g. `breaks` or `classes_limit`.
#' @return A `loan_cont_table`: a data frame with one row per group of the
#'   variable (column `the_var`), the status counts above, the outcome counts
#'   (`count_good` / `count_bad` for a binary outcome) and `issued_loans_total`
#'   (applications with a known outcome). Attributes record the count-column
#'   labels the measures read.
#' @examples
#' data(fintech)
#' status <- value_map("application_status", approved = "LOAN_ISSUED",
#'                     rejected = "REJECTED", cancelled = "CANCELLED")
#'
#' # a data frame and column names
#' contingency_table(fintech, "gender", outcome = binary_outcome("fpd15", bad = 1),
#'                   application_status = status)
#'
#' # a loan_tbl: roles declared once
#' lt <- loan_tbl(fintech, application_id = "application_id",
#'                outcomes = binary_outcome("fpd15", bad = 1),
#'                application_status = status, unlisted_role = "supplementary")
#' contingency_table(lt, "gender")
#' @export
contingency_table <- function(x, ...) {
  if(!is.data.frame(x)) check_variable(x)
  UseMethod('contingency_table')
}

#' @rdname contingency_table
#' @export
contingency_table.data.frame <- function(x, variable, outcome = NULL, application_status = NULL, ...) {

  r <- resolve_roles(x,
                     variable           = variable,
                     outcome            = outcome,
                     application_status = application_status,
                     .required = c('variable', 'outcome'))
  outcome <- check_outcome(r$outcome, attr(r, 'maps')$outcome, types = c('binary', 'multiclass'))

  contingency_table(r$variable, outcome, r$application_status,
                    status_map = attr(r, 'maps')$application_status, ...)
}

#' @rdname contingency_table
#' @param status_map Vector form only: the [value_map()] for
#'   `application_status`. (The data-frame form takes it from
#'   `application_status` itself.) Without one, the approved status is
#'   inferred, with a warning.
#' @param template_matrix A `loan_cont_table` whose numeric bins or nominal
#'   groups are reused. New nominal groups in the comparison are added.
#' @param classes_limit Maximum number of groups; the least frequent merge into
#'   `infrequent_group_name`.
#' @param infrequent_group_name Label of the merged group.
#' @export
contingency_table.factor <- function(x, outcome, application_status = NULL,
                                             status_map = NULL,
                                             template_matrix = NULL,
                                             classes_limit = Inf,
                                             infrequent_group_name = '_OTHER_',
                                             ...) {
  .contingency_table(x, outcome, application_status, status_map, template_matrix,
                     classes_limit, infrequent_group_name)
}

## Shared counting workhorse. Group statistics require declared status meaning;
## the public contingency table retains its warning-based inference.
.contingency_table <- function(x, outcome, application_status = NULL,
                               status_map = NULL, template_matrix = NULL,
                               classes_limit = Inf, infrequent_group_name = '_OTHER_',
                               infer_status = TRUE) {
  the_var <- x  # output column keeps the name `the_var` until the column-naming pass (TODO.md)
  ## The ONE place a missing value becomes the `value_NA` group, for every caller.
  ## Numeric bins arrive with it already assigned, so they carry no NA here.
  if(anyNA(the_var)) the_var <- nominal_groups(the_var)
  ## a nominal template: reuse its groups (after the step above, so its
  ## `value_NA` group matches); numeric bins were re-cut from the template already
  if(!is.null(template_matrix) && is.null(attr(x, 'discretized:breaks'))) {
    if(!inherits(template_matrix, 'loan_cont_table') ||
       !'the_var' %in% names(template_matrix)) {
      stop('`template_matrix` must be a `loan_cont_table` with `the_var` -- got ',
           class(template_matrix)[[1L]], '.', call. = FALSE)
    }
    the_var <- factor(as.character(the_var),
                      levels = union(as.character(template_matrix$the_var), levels(the_var)))
  }
  check_same_length(the_var, outcome = outcome, application_status = application_status)
  outcome <- check_outcome(outcome, types = c('binary', 'multiclass'))
  stopifnot(classes_limit >= 2)
  
  if(!is.null(attributes(the_var)$`discretized:breaks`)) { #indicates discretized numeric variable. 
    cuts <- attributes(the_var)$`discretized:breaks`
  } else {
    cuts <- NULL
  }
  
  cont_table <- table(the_var) %>% as.data.frame()
  
  if(nrow(cont_table) > classes_limit) {
    freq_vector <- cont_table %>% arrange(desc(Freq)) %>% pull(Freq)
    freq_threshold <- freq_vector[[classes_limit]]
    
    the_var <- as.character(the_var)
    the_var[the_var != 'value_NA' &
              the_var %in% cont_table$the_var[cont_table$Freq <= freq_threshold]] <- infrequent_group_name
    the_var <- as.factor(the_var)
    cont_table <- table(the_var) %>% as.data.frame()
    
    infreq_row_order <- which(cont_table$the_var == infrequent_group_name)
    cont_table <- cont_table[-infreq_row_order, ] %>% rbind(cont_table[infreq_row_order,])
  }
  
  cont_loan <- table(the_var, outcome) %>% as.data.frame.matrix()
  colnames(cont_loan) <- paste0('count_', colnames(cont_loan))
  cont_loan$issued_loans_total <- rowSums(cont_loan)
  cont_loan$the_var <- rownames(cont_loan)
  
  ## the ONE place outcome count columns are named; measures read these labels
  ## from the attributes instead of assuming column names
  binary     <- identical(levels(outcome), c('good', 'bad'))
  bad_label  <- if(binary) 'count_bad'  else NULL
  good_label <- if(binary) 'count_good' else NULL

  if(!is.null(application_status)) {
    #TO DO: do not calculate the approval rate (and the the contingency table below) if all the applications have been approved

    ## Level 1 of the two-level model: the institution's raw statuses, kept as-is
    cont_app <- table(the_var, application_status) %>% as.data.frame.matrix()
    colnames(cont_app) <- paste0('count_', colnames(cont_app))
    cont_app$applications_total <- rowSums(cont_app)

    ## Level 2: canonical status classes, which every formula uses
    if(is.null(status_map) && infer_status) {
      status_map <- detect_status_map(outcome, application_status)
    }
    status_class <- NULL
    if(!is.null(status_map)) {
      status_class <- apply_value_map(application_status, status_map,
                                      role = 'application_status')

      raw_labels <- intersect(substring(colnames(cont_app), 7L),
                              loan_vocabulary('application_status'))
      mapped_labels <- rep(names(status_map$map), lengths(status_map$map))[
        match(raw_labels, unlist(status_map$map, use.names = FALSE))]
      if(!is.null(status_map$default)) {
        mapped_labels[is.na(mapped_labels)] <- status_map$default
      }
      conflicting <- raw_labels[!is.na(mapped_labels) & raw_labels != mapped_labels]
      if(length(conflicting)) {
        stop('`application_status` values that spell canonical labels must map to ',
             'the same label; got different mappings for: ',
             paste(conflicting, collapse = ', '), '. Conflicting count column(s): ',
             paste0('count_', conflicting, collapse = ', '), '.', call. = FALSE)
      }

      cont_class <- table(the_var, status_class) %>% as.data.frame.matrix()
      colnames(cont_class) <- paste0('count_', colnames(cont_class))
      ## The risk-policy denominator excludes `cancelled`: those applications were
      ## closed for reasons outside policy control, so they never received a decision.
      decisioned <- intersect(c('count_approved', 'count_rejected'), colnames(cont_class))
      cont_class$decisioned_total <- rowSums(cont_class[, decisioned, drop = FALSE])
      ## drop canonical columns the raw breakdown already provides (raw == canonical)
      cont_class <- cont_class[, setdiff(colnames(cont_class), colnames(cont_app)), drop = FALSE]

      cont_app <- cbind(cont_app, cont_class)
    }
    cont_app$the_var <- rownames(cont_app)

    cont_table <- merge(subset(cont_table, select = 1),  cont_app, by = 'the_var', all.x = TRUE, sort = FALSE)
    cont_table <- merge(cont_table,  cont_loan, by = 'the_var', all.x = TRUE, sort = FALSE)

    app_status_attr <- list(
      values = unique(application_status),
      table = table(application_status, exclude = NULL),
      accept_label = if(!is.null(status_map)) 'count_approved' else NULL,
      status_map = status_map,
      class_table = if(!is.null(status_map)) table(status_class, exclude = NULL) else NULL
    )
  } else {
    cont_table <- merge(subset(cont_table, select = 1),  cont_loan, by = 'the_var', all.x = TRUE, sort = FALSE)
    if(infer_status && anyNA(outcome)) warning('Missing values in the outcome. They are not shown in contingency tables.')
    app_status_attr <- list(
      values = NULL,
      table = NULL,
      accept_label = NULL,
      status_map = NULL,
      class_table = NULL
    )
  }

  ## the loan_cont_table class: measures (add_woe(), ...) and group_stats() build on it
  cont_table <- structure(
    cont_table,
    class = c('loan_cont_table', 'data.frame'),
    target_var_dict = list(
      values = unique(outcome),
      table = table(outcome, exclude = NULL),
      bad_label = bad_label,
      good_label = good_label
    ),
    app_status_dict = app_status_attr,
    cuts = cuts
  )

  return(cont_table)
}

#' @rdname contingency_table
#' @export
contingency_table.character <- function(x, outcome, ...) {
  contingency_table(as.factor(x), outcome, ...)
}

#' @rdname contingency_table
#' @export
contingency_table.logical <- function(x, outcome, ...) {
  contingency_table(as.character(x), outcome, ...)
}

#' @rdname contingency_table
#' @param breaks Number of quantile bins for a numeric variable.
#' @export
contingency_table.numeric <- function(x, outcome, application_status = NULL, template_matrix = NULL, breaks = 5, ...) {
  if(!is.null(template_matrix)) {
    stopifnot('loan_cont_table' %in% class(template_matrix))

    discretized_intervals <- template_matrix$the_var
    cuts <- attributes(template_matrix)$cuts
    the_var_num <- discretize_values(x, breaks = breaks, discretized_intervals = discretized_intervals, cuts = cuts)
    
  } else {
    the_var_num <- discretize_values(x, breaks = breaks)
  }

  ## Numeric bins already contain the package's missing label, not raw values.
  .contingency_table(the_var_num, outcome = outcome, application_status = application_status, ...)
}

