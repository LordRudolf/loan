## A tibble that remembers which column plays which role, so analysis functions
## can fill in their own arguments. Roles only -- value remapping is explicit
## (value_map(), binary_outcome()), no `$` overloading, no non-standard evaluation.

#' Roles a `loan_tbl` can declare
#' @keywords internal
loan_role_names <- function() {
  c('application_id', 'loan_id', 'client_id',
    'application_created_at', 'first_delay_at',
    'application_status', 'outcomes', 'primary_outcome',
    'predictors', 'supplementary')
}

#' Declare the roles of a loan portfolio dataset
#'
#' Each row must be one **closed** application. A declared `application_id`
#' must have no missing or duplicated values; `loan_id` and `client_id` may
#' repeat. Errors report offending rows (all occurrences of repeated ids) and
#' example values. Rejected and cancelled applications belong in
#' the data -- they carry no outcome but are needed for approval rates and
#' reject inference.
#'
#' @param data A data frame or tibble.
#' @param ... Role declarations. Each is a column name, except:
#'   * `outcomes`: one or several outcomes -- column names and/or
#'     [binary_outcome()] declarations, optionally named
#'     (`list(fpd15 = binary_outcome("fpd15", bad = 1), dpd = "current_dpd")`).
#'   * `primary_outcome`: the name of the outcome that functions working with a
#'     single outcome use by default. Needed only when several are declared.
#'   * `application_status`: a column name or a [value_map()].
#'   * `predictors`: the features to analyse -- numeric, logical or nominal
#'     (character / factor) columns. Any other type is kept, with a warning.
#'   * `supplementary`: columns kept with the data but excluded from automatic
#'     predictor selection -- other ids, other timestamps, bookkeeping fields.
#'     They can still be analysed when named explicitly.
#'
#'   A column may hold only one role (an outcome may not also be a predictor).
#' @param unlisted_role The role given to every column not listed in `...`:
#'   * `"auto"` (default): numeric, logical and nominal columns become
#'     predictors, anything else (dates, lists, ...) supplementary. A column
#'     that would become a predictor but is almost a copy of a column you listed
#'     as an id, timestamp, outcome or predictor (association > 0.95) becomes
#'     supplementary instead -- see Details.
#'   * `"predictors"`: every unlisted column becomes a predictor.
#'   * `"supplementary"`: every unlisted column becomes supplementary.
#'
#'   All three apply to unlisted columns only; a listed column keeps its role.
#'
#'   `loan_tbl()` reports what it assigned. Review it: a post-outcome column
#'   you did not declare (repayment amount, current days past due, ...) is not
#'   a near-copy of anything, so `"auto"` would still make it a predictor.
#'   [predictor_provenance()] distinguishes declared predictors from those
#'   assigned by `unlisted_role`.
#' @details The `"auto"` screen measures each candidate predictor against every
#'   column listed as an id, timestamp, outcome or predictor (a binary outcome
#'   counts as bad = 1): the absolute Pearson correlation when both columns are
#'   numeric, logical or timestamps; Tschuprow's T when either is nominal (a
#'   numeric column then counts its distinct values as categories). T reaches 1
#'   only when one column is a one-to-one relabelling of the other -- unlike
#'   Cramer's V or eta, which also reach 1 when a many-level column (`city`)
#'   merely determines a few-level one (`province`). The screen catches
#'   duplicated keys, timestamps stored as numbers, re-coded copies of an
#'   outcome or a predictor -- not subtler leakage.
#' @return A `loan_tbl`: the data as a tibble, with the roles stored as its
#'   `loan_roles` attribute. `dplyr::rename()`, `dplyr::select()` and `[` keep
#'   roles aligned with the columns. Removing a role's column drops that role;
#'   removing the primary outcome leaves it unset, even if another remains.
#' @examples
#' data(fintech)
#' lt <- loan_tbl(
#'   fintech,
#'   application_id         = "application_id",
#'   loan_id                = "loan_id",
#'   client_id              = "client_id",
#'   application_created_at = "app_created_at",
#'   application_status     = value_map("application_status",
#'                                      approved  = "LOAN_ISSUED",
#'                                      rejected  = "REJECTED",
#'                                      cancelled = "CANCELLED"),
#'   outcomes = list(
#'     fpd15     = binary_outcome("fpd15", bad = 1, good = 0),
#'     defaulted = binary_outcome("loan_status", bad = c("DEFAULTED", "WRITE_OFF")),
#'     dpd       = "current_dpd"
#'   ),
#'   primary_outcome        = "fpd15",
#'   supplementary          = c("app_close_reason", "cash_out", "principal_disbursed")
#' )
#' lt
#' group_stats(lt, "client_age")                        # the primary outcome
#' group_stats(lt, "client_age", outcome = "defaulted") # another declared one
#' @export
loan_tbl <- function(data, ..., unlisted_role = 'auto') {
  if(!is.data.frame(data)) stop('`data` must be a data frame.', call. = FALSE)

  roles <- list(...)
  if(length(roles) && (is.null(names(roles)) || any(names(roles) == ''))) {
    stop('Every role given to `loan_tbl()` must be named.', call. = FALSE)
  }

  unknown <- setdiff(names(roles), loan_role_names())
  if(length(unknown)) {
    stop('Unknown role(s): ', paste(unknown, collapse = ', '),
         '. Known roles: ', paste(loan_role_names(), collapse = ', '), '.', call. = FALSE)
  }

  nms <- colnames(data)

  ## a declaration either maps values (value_map / binary_outcome) or names columns
  check_declared_columns <- function(spec, label) {
    cols <- if(is_value_map(spec)) spec$column else spec
    if(is_value_map(spec) && is.null(cols)) {
      stop('The declaration for `', label, '` must name its column.', call. = FALSE)
    }
    if(!is.character(cols)) {
      stop('`', label, '` must be given as column name(s)', call. = FALSE)
    }
    absent <- setdiff(cols, nms)
    if(length(absent)) {
      stop('Role `', label, '`: column(s) not found: ', paste(absent, collapse = ', '),
           '.', call. = FALSE)
    }
  }

  if(!is.null(roles$outcomes)) {
    roles$outcomes <- normalize_outcomes(roles$outcomes)
    for(nm in names(roles$outcomes)) {
      spec <- roles$outcomes[[nm]]
      check_declared_columns(spec, paste0('outcomes$', nm))
      ## fail now, not deep inside an analysis, if the declaration does not cover the data
      if(is_value_map(spec)) apply_value_map(data[[spec$column]], spec, role = 'outcome')
    }
  }
  if(!is.null(roles$primary_outcome)) {
    p <- roles$primary_outcome
    if(!is.character(p) || length(p) != 1L || !p %in% names(roles$outcomes)) {
      stop('`primary_outcome` must be the name of one declared outcome',
           if(length(roles$outcomes)) paste0(': ', paste(names(roles$outcomes), collapse = ', '))
           else ' (no `outcomes` are declared)', '.', call. = FALSE)
    }
  }

  for(role in setdiff(names(roles), c('outcomes', 'primary_outcome'))) {
    spec <- roles[[role]]
    if(is_value_map(spec)) {
      if(!identical(role, 'application_status')) {
        stop('`value_map()` is supported for `application_status` (and inside ',
             '`outcomes`) only -- got it for `', role, '`; see TODO.md.', call. = FALSE)
      }
      check_vocabulary(spec, role)
    }
    check_declared_columns(spec, role)
  }

  for(column in roles$application_id) {
    id <- data[[column]]
    missing <- is.na(id)
    repeated <- !missing & (duplicated(id) | duplicated(id, fromLast = TRUE))
    offending <- missing | repeated
    if(any(offending)) {
      stop('`application_id` must have no missing or duplicated values; column "',
           column, '" has ', sum(offending), ' offending rows (', sum(missing),
           ' missing, ', sum(repeated), ' with duplicated values). Examples: ',
           paste(utils::head(unique(id[offending]), 5L), collapse = ', '), '.',
           call. = FALSE)
    }
  }

  if(is_value_map(roles$application_status)) {
    apply_value_map(data[[roles$application_status$column]],
                    roles$application_status, role = 'application_status')
  }

  ## one role per column: an outcome that is also a predictor is target leakage
  fixed <- role_columns(roles[setdiff(names(roles), c('predictors', 'supplementary'))])
  for(role in c('predictors', 'supplementary')) {
    clash <- intersect(roles[[role]], fixed)
    if(length(clash)) {
      stop('Column(s) declared as `', role, '` already have another role: ',
           paste(clash, collapse = ', '), '.', call. = FALSE)
    }
  }
  both <- intersect(roles$predictors, roles$supplementary)
  if(length(both)) {
    stop('Column(s) declared as both `predictors` and `supplementary`: ',
         paste(both, collapse = ', '), '.', call. = FALSE)
  }

  declared_predictors <- roles$predictors
  roles <- assign_unlisted(data, roles, unlisted_role)
  predictors <- unique(roles$predictors)
  attr(roles, 'predictor_provenance') <- stats::setNames(
    ifelse(predictors %in% declared_predictors, 'declared', 'auto'), predictors)

  ## a predictor of a type no analysis function accepts is allowed, but flagged
  odd <- Filter(function(cl) !is_variable_type(data[[cl]]), roles$predictors)
  if(length(odd)) {
    warning('These `predictors` are neither numeric, logical nor nominal (character / factor), ',
            'so the analysis functions will reject them: ',
            paste0(odd, ' (', vapply(odd, function(cl) class(data[[cl]])[[1]], ''), ')', collapse = ', '),
            '. Declare them as `supplementary` if they are not features.', call. = FALSE)
  }

  data <- tibble::as_tibble(data)
  structure(data,
            class = c('loan_tbl', class(data)),
            loan_roles = roles)
}

## Every column a set of roles refers to (value maps and outcome lists included).
role_columns <- function(roles) {
  col_of <- function(spec) if(is_value_map(spec)) spec$column else spec
  cols <- lapply(roles[setdiff(names(roles), 'primary_outcome')], function(spec) {
    if(is.list(spec) && !is_value_map(spec)) unlist(lapply(spec, col_of)) else col_of(spec)
  })
  unique(unlist(cols, use.names = FALSE))
}

## Give every unlisted column a role, so the roles account for the whole table.
assign_unlisted <- function(data, roles, unlisted_role) {
  modes <- c('auto', 'predictors', 'supplementary')
  if(!(is.character(unlisted_role) && length(unlisted_role) == 1L && unlisted_role %in% modes)) {
    stop('`unlisted_role` must be "auto", "predictors" or "supplementary".', call. = FALSE)
  }
  rest <- setdiff(colnames(data), role_columns(roles))
  if(!length(rest)) return(roles)

  demoted <- character()
  to_pred <- switch(unlisted_role,
    predictors    = rest,
    supplementary = character(),
    auto          = {
      candidates <- Filter(function(cl) is_variable_type(data[[cl]]), rest)
      demoted    <- near_copies(data, candidates, reference_columns(data, roles))
      setdiff(candidates, names(demoted))
    })
  to_supp <- setdiff(rest, to_pred)

  roles$predictors    <- c(roles$predictors, to_pred)
  roles$supplementary <- c(roles$supplementary, to_supp)
  if(!length(roles$predictors))    roles$predictors    <- NULL
  if(!length(roles$supplementary)) roles$supplementary <- NULL

  preview <- function(x) paste0(paste(utils::head(x, 8), collapse = ', '), if(length(x) > 8) ', ...' else '')
  message('loan_tbl(): ', length(rest), ' unlisted column(s) assigned (unlisted_role = "', unlisted_role, '")',
          if(length(to_pred)) paste0('\n  predictors (', length(to_pred), '): ', preview(to_pred)) else '',
          if(length(to_supp)) paste0('\n  supplementary (', length(to_supp), '): ', preview(to_supp)) else '',
          if(length(demoted)) paste0('\n  supplementary, not predictors, because each is a near-copy of a declared column: ',
                                     paste0(names(demoted), ' (', demoted, ')', collapse = ', ')) else '',
          '\n  Declare columns in `loan_tbl()` or set `unlisted_role` to change this.')
  roles
}

## Columns listed as ids, timestamps, outcomes or predictors -- what the "auto"
## screen compares each candidate against. A binary outcome is scored bad = 1.
reference_columns <- function(data, roles) {
  refs <- list()
  for(role in c('application_id', 'loan_id', 'client_id', 'application_created_at', 'first_delay_at')) {
    cl <- roles[[role]]
    if(!is.null(cl)) refs[[cl]] <- data[[cl]]
  }
  for(nm in names(roles$outcomes)) {
    spec <- roles$outcomes[[nm]]
    v <- data[[if(is_value_map(spec)) spec$column else spec]]
    refs[[paste('outcome', nm)]] <- if(is_value_map(spec)) as.numeric(check_outcome(v, spec) == 'bad') else v
  }
  for(cl in roles$predictors) refs[[paste('predictor', cl)]] <- data[[cl]]
  refs
}

## Candidates whose association with any reference exceeds `threshold`, named by
## candidate, valued with the strongest match (e.g. "r = 1.00 with application_id").
near_copies <- function(data, candidates, refs, threshold = 0.95) {
  out <- character()
  for(cl in candidates) {
    a <- vapply(refs, function(ref) association(data[[cl]], ref), numeric(1))
    if(all(is.na(a)) || max(a, na.rm = TRUE) <= threshold) next
    best <- which.max(a)
    measure <- if(has_numeric_scale(data[[cl]]) && has_numeric_scale(refs[[best]])) 'r' else 'T'
    out[[cl]] <- sprintf('%s = %.2f with %s', measure, a[[best]], names(refs)[[best]])
  }
  out
}

## How close two columns are to being copies of each other, 0..1, on complete
## pairs: |Pearson r| when both have a numeric scale; Tschuprow's T otherwise,
## which is 1 only for a one-to-one relabelling (see ?loan_tbl, Details).
has_numeric_scale <- function(v) is.numeric(v) || is.logical(v) || inherits(v, c('Date', 'POSIXt'))

association <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  if(sum(ok) < 3L) return(NA_real_)
  a <- a[ok]
  b <- b[ok]

  if(has_numeric_scale(a) && has_numeric_scale(b)) {
    a <- as.numeric(a)
    b <- as.numeric(b)
    if(stats::var(a) == 0 || stats::var(b) == 0) return(NA_real_)
    return(abs(stats::cor(a, b)))
  }

  ## Tschuprow's T from sparse counts -- a dense table of two near-unique
  ## columns would not fit in memory
  ia <- match(a, unique(a))
  ib <- match(b, unique(b))
  ka <- max(ia)
  kb <- max(ib)
  if(ka < 2L || kb < 2L) return(NA_real_)
  n    <- length(ia)
  cell <- match(ia + (ib - 1) * ka, unique(ia + (ib - 1) * ka))   # double key: no overflow
  nij  <- tabulate(cell)
  first <- !duplicated(cell)                                       # first row of each cell, in cell order
  na <- as.numeric(tabulate(ia, ka))   # double: count products overflow integers
  nb <- as.numeric(tabulate(ib, kb))
  chi2 <- n * (sum(nij^2 / (na[ia[first]] * nb[ib[first]])) - 1)
  sqrt(chi2 / (n * sqrt((ka - 1) * (kb - 1))))
}

## `outcomes` as a named list: character -> one entry per column; a single
## declaration -> a list of one; names default to the column names.
normalize_outcomes <- function(outcomes) {
  if(is_value_map(outcomes)) outcomes <- list(outcomes)
  if(is.character(outcomes)) outcomes <- as.list(outcomes)
  if(!is.list(outcomes)) {
    stop('`outcomes` must be column name(s) and/or `binary_outcome()` declarations.',
         call. = FALSE)
  }

  col_of <- function(spec) if(is_value_map(spec)) spec$column else spec
  nms <- names(outcomes)
  if(is.null(nms)) nms <- rep('', length(outcomes))
  for(i in seq_along(outcomes)) {
    spec <- outcomes[[i]]
    if(!(is_value_map(spec) || (is.character(spec) && length(spec) == 1L))) {
      stop('Each outcome must be a single column name or a `binary_outcome()`.',
           call. = FALSE)
    }
    if(nms[[i]] == '') nms[[i]] <- col_of(spec)
  }
  if(anyDuplicated(nms)) {
    stop('Duplicated outcome name(s): ', paste(unique(nms[duplicated(nms)]), collapse = ', '),
         '.', call. = FALSE)
  }
  stats::setNames(outcomes, nms)
}

#' @rdname loan_tbl
#' @param x An object.
#' @export
is_loan_tbl <- function(x) inherits(x, 'loan_tbl')

#' Predictor provenance in a `loan_tbl`
#'
#' Shows whether each predictor was listed in `predictors =` (`"declared"`) or
#' assigned by `unlisted_role = "auto"` or `"predictors"` (`"auto"`). The names
#' follow column renames and removals. This is metadata, not another column role.
#'
#' @param x A `loan_tbl`.
#' @return A named character vector with one entry per predictor, or
#'   `character()` when there are no predictors.
#' @export
predictor_provenance <- function(x) {
  if(!is_loan_tbl(x)) {
    got <- if(length(class(x))) class(x)[[1]] else typeof(x)
    stop('`x` must be a `loan_tbl`; got `', got, '`.', call. = FALSE)
  }
  provenance <- attr(loan_roles(x), 'predictor_provenance', exact = TRUE)
  if(length(provenance)) provenance else character()
}

#' @export
print.loan_tbl <- function(x, ...) {
  roles <- loan_roles(x)
  cat('# loan_tbl: ', nrow(x), ' closed applications x ', ncol(x), ' columns\n', sep = '')

  describe <- function(spec) {
    if(inherits(spec, 'loan_binary_outcome')) {
      paste0(spec$column, '  [binary: bad = ', paste(spec$map$bad, collapse = ', '),
             if(is.null(spec$map$good)) '; good = all other values'
             else paste0('; good = ', paste(spec$map$good, collapse = ', ')), ']')
    } else if(is_value_map(spec)) {
      paste0(spec$column, '  [mapped to: ', paste(names(spec$map), collapse = ', '), ']')
    } else if(length(spec) > 6L) {
      paste0('(', length(spec), ') ', paste(spec[1:5], collapse = ', '), ', ...')
    } else paste(spec, collapse = ', ')
  }

  if(length(roles)) {
    cat('# roles:\n')
    for(role in setdiff(names(roles), 'primary_outcome')) {
      if(identical(role, 'outcomes')) {
        primary <- tryCatch(primary_outcome_name(roles), error = function(e) NA)
        cat('#   outcomes:\n')
        for(nm in names(roles$outcomes)) {
          cat('#     ', nm, ': ', describe(roles$outcomes[[nm]]),
              if(identical(nm, primary)) '  (primary)' else '', '\n', sep = '')
        }
      } else if(identical(role, 'predictors')) {
        provenance <- predictor_provenance(x)
        cat('#   predictors (', length(provenance), ': ',
            sum(provenance == 'declared'), ' declared, ',
            sum(provenance == 'auto'), ' auto): ',
            if(length(provenance) > 6L) {
              paste0(paste(utils::head(roles$predictors, 5L), collapse = ', '), ', ...')
            } else paste(roles$predictors, collapse = ', '), '\n', sep = '')
      } else {
        cat('#   ', role, ': ', describe(roles[[role]]), '\n', sep = '')
      }
    }
  } else {
    cat('# no roles declared\n')
  }
  print(tibble::as_tibble(x), ...)
  invisible(x)
}

#' Roles declared on a `loan_tbl`
#'
#' @param x A data frame.
#' @return Named list of role declarations, or `NULL` for a plain data frame.
#' @keywords internal
loan_roles <- function(x) attr(x, 'loan_roles', exact = TRUE)

## Column changes are positional for renames and name-based for removals.
## Keep declaration names (outcome keys) stable; only their columns move.
update_loan_roles <- function(roles, old, new) {
  replacement <- stats::setNames(new, old)
  provenance <- attr(roles, 'predictor_provenance', exact = TRUE)
  column <- function(spec) {
    if(is_value_map(spec)) {
      name <- replacement[[spec$column]]
      if(is.null(name) || is.na(name)) return(NULL)
      spec$column <- name
      return(spec)
    }
    kept <- replacement[spec]
    unname(kept[!is.na(kept)])
  }

  for(role in setdiff(names(roles), c('outcomes', 'primary_outcome'))) {
    updated <- column(roles[[role]])
    if(length(updated)) roles[[role]] <- updated else roles[[role]] <- NULL
  }
  if(length(roles$outcomes)) {
    roles$outcomes <- lapply(roles$outcomes, function(spec) {
      updated <- column(spec)
      if(length(updated)) updated else NULL
    })
    roles$outcomes <- Filter(Negate(is.null), roles$outcomes)
  }
  if(length(roles$primary_outcome) == 1L &&
     !roles$primary_outcome %in% names(roles$outcomes)) {
    roles$primary_outcome <- character()
  }
  if(length(provenance)) {
    renamed <- replacement[names(provenance)]
    keep <- !is.na(renamed)
    attr(roles, 'predictor_provenance') <- stats::setNames(
      unname(provenance[keep]), unname(renamed[keep]))
  }
  roles
}

#' @export
`[.loan_tbl` <- function(x, ...) {
  out <- NextMethod('[')
  if(is.data.frame(out)) {
    old <- names(x)
    remaining <- old %in% names(out)
    attr(out, 'loan_roles') <- update_loan_roles(loan_roles(x), old,
                                                  ifelse(remaining, old, NA_character_))
  }
  out
}

#' @export
`names<-.loan_tbl` <- function(x, value) {
  old <- names(x)
  roles <- loan_roles(x)
  x <- NextMethod('names<-')
  attr(x, 'loan_roles') <- update_loan_roles(roles, old, names(x))
  x
}

## Which declared outcome a single-outcome function uses when none is named.
primary_outcome_name <- function(declared) {
  outs <- names(declared$outcomes)
  if(length(declared$primary_outcome) == 1L) return(declared$primary_outcome)
  if(!is.null(declared$primary_outcome)) {
    stop('No `primary_outcome` remains. Choose one with `outcome = "<name>"`.',
         call. = FALSE)
  }
  if(length(outs) == 1L) return(outs)
  stop('Several outcomes are declared (', paste(outs, collapse = ', '), ') but no ',
       '`primary_outcome`. Choose one with `outcome = "<name>"`, or set ',
       '`primary_outcome` in `loan_tbl()`.', call. = FALSE)
}
