## How every analysis function takes its arguments -- one path, one set of
## error messages:
##   1. resolve_roles()      a column name / vector / declaration -> values
##                           (a plain list; never assigns into the caller's frame)
##   2. check_variable()     is the variable an analysable type?
##      check_outcome()      is the outcome a supported type? binary -> good / bad
##      check_same_length()  vector form: do the companion vectors line up?


#' Resolve role arguments to vectors
#'
#' Each role may be given as a single column name, as a vector of values, as a
#' [value_map()] naming its own column, or omitted (in which case a `loan_tbl`
#' declaration is used, if present).
#'
#' Where a data frame is the first argument, **a length-1 character is treated
#' as a column name**, not as data. A one-row data frame is the only ambiguous
#' case and resolves in favour of the column name.
#'
#' @param data A data frame or `loan_tbl`.
#' @param ... Named role specifications.
#' @param .required Roles that must resolve to something.
#' @param .multi Roles that take several column names and never values.
#' @return Named list of resolved vectors, with a `"maps"` attribute holding the
#'   [value_map()] supplied for each role (or `NULL`).
#' @keywords internal
resolve_roles <- function(data, ..., .required = character(), .multi = character()) {
  args <- list(...)
  roles <- names(args)

  if(!is.data.frame(data)) stop('`data` must be a data frame.', call. = FALSE)
  if(length(args) && (is.null(roles) || any(roles == ''))) {
    stop('All roles given to `resolve_roles()` must be named.', call. = FALSE)
  }

  n        <- nrow(data)
  nms      <- colnames(data)
  declared <- loan_roles(data)

  out  <- stats::setNames(vector('list', length(roles)), roles)
  maps <- stats::setNames(vector('list', length(roles)), roles)

  for(role in roles) {
    spec <- args[[role]]
    from_declaration <- FALSE

    ## `outcome` on a loan_tbl: omitted -> the primary outcome; the name of a
    ## declared outcome -> that declaration (which may carry a binary_outcome()).
    if(identical(role, 'outcome') && length(declared$outcomes)) {
      if(is.null(spec)) {
        spec <- declared$outcomes[[primary_outcome_name(declared)]]
        from_declaration <- TRUE
      } else if(is.character(spec) && length(spec) == 1L && spec %in% names(declared$outcomes)) {
        spec <- declared$outcomes[[spec]]
        from_declaration <- TRUE
      }
    }

    ## not supplied -> fall back to the loan_tbl declaration.
    if(is.null(spec)) {
      spec <- declared[[role]]
      from_declaration <- !is.null(spec)
    }

    ## A declared column can disappear later (`select()`, `[`, ...), and the roles
    ## attribute travels with the data, so check it is still there rather than
    ## failing obscurely further down. Only requested roles are checked.
    if(from_declaration) {
      declared_cols <- if(is_value_map(spec)) spec$column else spec
      if(is.character(declared_cols) && !all(declared_cols %in% nms)) {
        stop('`', role, '` is declared in `loan_tbl()` as column "',
             paste(setdiff(declared_cols, nms), collapse = '", "'),
             '", which is no longer present in the data.', call. = FALSE)
      }
    }

    if(is.null(spec)) {
      if(role %in% .required) {
        stop('`', role, '` must be supplied',
             if(is.null(declared)) ' (or declare it once with `loan_tbl()`)'
             else ' or declared in `loan_tbl()`', '.', call. = FALSE)
      }
      next
    }

    ## a value_map carries its own column and its vocabulary
    if(is_value_map(spec)) {
      if(!role %in% c('application_status', 'outcome')) {
        stop('`value_map()` is currently supported for `application_status` and `outcome` only ',
             '(got it for `', role, '`); see TODO.md.', call. = FALSE)
      }
      if(is.null(spec$column)) {
        stop('The `value_map()` given for `', role, '` must name its column.', call. = FALSE)
      }
      check_vocabulary(spec, role)
      maps[[role]] <- spec
      spec <- spec$column
    }

    ## several column names (never values)
    if(role %in% .multi) {
      if(!is.character(spec)) {
        stop('`', role, '` must be given as column name(s).', call. = FALSE)
      }
      absent <- setdiff(spec, nms)
      if(length(absent)) {
        stop('`', role, '`: column(s) not found in the data: ',
             paste(absent, collapse = ', '), '.', call. = FALSE)
      }
      out[[role]] <- stats::setNames(lapply(spec, function(cl) data[[cl]]), spec)
      next
    }

    ## a single column name
    if(is.character(spec) && length(spec) == 1L) {
      if(!spec %in% nms) {
        stop('`', role, '`: the column "', spec, '" was not found in the data. ',
             'Available columns: ', paste(nms[seq_len(min(10L, length(nms)))], collapse = ', '),
             if(length(nms) > 10L) ', ...' else '', '.', call. = FALSE)
      }
      out[[role]] <- data[[spec]]
      next
    }

    ## the values themselves
    if(length(spec) == n) {
      out[[role]] <- spec
      next
    }

    stop('`', role, '` must be a single column name, or a vector of length ',
         n, ' -- got ', class(spec)[[1]], ' of length ', length(spec), '.', call. = FALSE)
  }

  attr(out, 'maps') <- maps
  out
}

## Is `v` one of the kinds of vector the package analyses as a variable? The one
## definition, shared by check_variable() and loan_tbl()'s predictor checks.
## Logical counts as a valid variable (grouped TRUE / FALSE, like a nominal).
is_variable_type <- function(v, types = c('numeric', 'logical', 'character', 'factor')) {
  ('numeric'   %in% types && is.numeric(v))   ||
  ('logical'   %in% types && is.logical(v))   ||
  ('character' %in% types && is.character(v)) ||
  ('factor'    %in% types && is.factor(v))
}

#' Check that `variable` can be analysed
#'
#' The one type check shared by every function that analyses a single variable,
#' so an unusable input fails with the same message everywhere -- whether it was
#' passed as a vector or resolved from a column name by [resolve_roles()].
#'
#' @param variable The values to be analysed.
#' @param types Allowed kinds: any of `"numeric"`, `"logical"`, `"character"`,
#'   `"factor"`.
#' @return `variable`, invisibly.
#' @keywords internal
check_variable <- function(variable, types = c('numeric', 'logical', 'character', 'factor')) {
  if(!is_variable_type(variable, types)) {
    kinds <- if(length(types) > 1L) {
      paste(paste(types[-length(types)], collapse = ', '), 'or', types[length(types)])
    } else types
    stop('`variable` must be a ', kinds, ' vector -- got ', class(variable)[[1]], '.',
         call. = FALSE)
  }
  invisible(variable)
}


#' Check an outcome's type and return it in canonical form
#'
#' The outcome counterpart of [check_variable()]: every function states which
#' outcome types it supports, and fails with the same message otherwise. A
#' binary outcome is returned as a factor with the canonical levels
#' `c("good", "bad")` -- through its [binary_outcome()] declaration when there is
#' one, otherwise by inference (with a warning when it has to guess). Calling it
#' on an outcome that is already canonical is free and silent.
#'
#' @param outcome Outcome values.
#' @param map The `binary_outcome()` / `value_map()` declared for it, or `NULL`.
#' @param types Supported types: any of `"binary"`, `"multiclass"`, `"numeric"`.
#' @return The outcome; canonical `good` / `bad` factor when binary.
#' @keywords internal
check_outcome <- function(outcome, map = NULL, types = 'binary') {
  canonical <- is.factor(outcome) && identical(levels(outcome), c('good', 'bad'))
  type <- if(canonical || !is.null(map)) 'binary' else detect_outcome_type(outcome)

  if(!type %in% types) {
    kinds <- if(length(types) > 1L) {
      paste(paste(types[-length(types)], collapse = ', '), 'or', types[length(types)])
    } else types
    hint <- if(type == 'numeric' && length(unique(stats::na.omit(outcome))) == 2L) {
      ' If it is a two-valued flag, declare it with `binary_outcome()`.'
    } else ''
    stop('`outcome` must be a ', kinds, ' outcome -- got a ', type, ' outcome.', hint,
         call. = FALSE)
  }

  if(type != 'binary' || canonical) return(outcome)
  canon <- if(!is.null(map)) apply_value_map(outcome, map, role = 'outcome')
           else detect_binary_outcome(outcome)
  factor(as.character(canon), levels = c('good', 'bad'))
}

## Vector form: every companion vector must line up with `variable`. The data-frame
## form gets the same guarantee from resolve_roles().
check_same_length <- function(variable, ...) {
  others <- Filter(Negate(is.null), list(...))
  n <- length(variable)
  for(nm in names(others)) {
    if(length(others[[nm]]) != n) {
      stop('`', nm, '` must have one value per element of `variable` (', n,
           ') -- got length ', length(others[[nm]]), '.', call. = FALSE)
    }
  }
  invisible(TRUE)
}

## 'binary', 'multiclass' or 'numeric'. A 0/1 numeric or a logical counts as binary.
detect_outcome_type <- function(outcome) {
  values <- unique(stats::na.omit(outcome))

  if(is.logical(outcome)) return('binary')
  if(is.numeric(outcome)) {
    if(length(values) == 2L && all(c(0, 1) %in% values)) return('binary')
    return('numeric')
  }
  if(is.character(outcome) || is.factor(outcome)) {
    return(if(length(values) == 2L) 'binary' else 'multiclass')
  }
  stop('`outcome` must be numeric, logical, character or factor -- got ',
       class(outcome)[[1]], '.', call. = FALSE)
}

## Vector form of a function that funnels into its data-frame method: the
## variable becomes a column, and a status vector with its value_map becomes a
## column plus a value_map pointing at it -- so the map is not lost on the way.
frame_with_status <- function(x, application_status, status_map) {
  frame <- data.frame(variable = x)
  if(is.null(status_map) || is.null(application_status)) {
    return(list(frame = frame, status = application_status))
  }
  frame$application_status <- application_status
  status_map$column <- 'application_status'
  list(frame = frame, status = status_map)
}
