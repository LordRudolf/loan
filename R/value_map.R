## Controlled vocabularies for categorical roles.
##
## Two-level model: the institution's raw values are always preserved, and each
## raw value maps onto exactly one package-canonical label. Formulas use the
## canonical label; output tables keep the raw breakdown as well.

#' Canonical labels allowed for a role
#'
#' @param role Role name, e.g. `"application_status"`.
#' @return Character vector of allowed labels, or `NULL` if the role has no
#'   controlled vocabulary (ids, timestamps, features).
#' @keywords internal
loan_vocabulary <- function(role) {
  switch(role,
    application_status = c('approved', 'rejected', 'cancelled'),
    outcome            = c('good', 'bad'),
    NULL
  )
}

#' Map an institution's raw category values onto canonical labels
#'
#' @param column Name of the column holding the raw values. May be `NULL` when
#'   the map is applied to a bare vector.
#' @param ... One argument per canonical label, each a character vector of the
#'   raw values that belong to it, e.g. `approved = c("LOAN_ISSUED", "DISBURSED")`.
#' @param .default Label for raw values not listed in `...`. When `NULL` (the
#'   default) an unlisted value is an error rather than being silently
#'   misclassified. When used for a role with a controlled vocabulary, this
#'   label must belong to that vocabulary, just like the labels in `...`.
#' @return A `loan_value_map` object.
#' @export
#' @examples
#' value_map("application_status",
#'           approved  = "LOAN_ISSUED",
#'           rejected  = "REJECTED",
#'           cancelled = c("CANCELLED", "CUSTOMER_WITHDREW"))
value_map <- function(column = NULL, ..., .default = NULL) {
  map <- list(...)

  if(!is.null(column)) {
    if(!is.character(column) || length(column) != 1L || is.na(column)) {
      stop('`column` must be a single column name, or NULL.', call. = FALSE)
    }
  }
  if(length(map) == 0L) {
    stop('`value_map()` needs at least one canonical label, e.g. approved = "LOAN_ISSUED".',
         call. = FALSE)
  }
  if(is.null(names(map)) || any(names(map) == '')) {
    stop('Every value given to `value_map()` must be named with a canonical label.',
         call. = FALSE)
  }
  if(anyDuplicated(names(map))) {
    dup <- unique(names(map)[duplicated(names(map))])
    stop('Duplicated canonical label(s): ', paste(dup, collapse = ', '), '.', call. = FALSE)
  }
  map <- lapply(map, as.character)

  raw <- unlist(map, use.names = FALSE)
  if(anyDuplicated(raw)) {
    dup <- unique(raw[duplicated(raw)])
    stop('These raw values are mapped to more than one canonical label: ',
         paste(dup, collapse = ', '),
         '. Each raw value must map to exactly one label.', call. = FALSE)
  }
  if(!is.null(.default) && (!is.character(.default) || length(.default) != 1L)) {
    stop('`.default` must be a single canonical label.', call. = FALSE)
  }

  structure(list(column = column, map = map, default = .default),
            class = 'loan_value_map')
}

#' @rdname value_map
#' @param x An object.
#' @export
is_value_map <- function(x) inherits(x, 'loan_value_map')

#' @export
print.loan_value_map <- function(x, ...) {
  cat('<loan_value_map>')
  if(!is.null(x$column)) cat(' column: ', x$column, sep = '')
  cat('\n')
  for(label in names(x$map)) {
    cat('  ', label, ' <- ', paste(x$map[[label]], collapse = ', '), '\n', sep = '')
  }
  if(!is.null(x$default)) cat('  .default -> ', x$default, '\n', sep = '')
  invisible(x)
}

## Canonical labels must belong to the role's vocabulary. Checked when the role
## is known (at resolve time), not in the constructor, so that value_map() stays
## role-generic.
check_vocabulary <- function(vm, role) {
  allowed <- loan_vocabulary(role)
  if(is.null(allowed)) return(invisible(TRUE))

  unknown <- setdiff(c(names(vm$map), vm$default), allowed)
  if(length(unknown)) {
    stop('Unknown ', role, ' label(s): ', paste(unknown, collapse = ', '),
         '. Allowed: ', paste(allowed, collapse = ', '), '.', call. = FALSE)
  }
  invisible(TRUE)
}

#' Apply a value map to a raw vector
#'
#' @param x Raw values.
#' @param vm A `loan_value_map`, or `NULL` (returns `NULL`).
#' @param role Role name, used to validate the canonical labels.
#' @return A factor of canonical labels. `NA` in `x` stays `NA`.
#' @keywords internal
apply_value_map <- function(x, vm, role = NULL) {
  if(is.null(vm)) return(NULL)
  if(!is.null(role)) check_vocabulary(vm, role)

  x <- as.character(x)
  out <- rep(NA_character_, length(x))
  for(label in names(vm$map)) {
    out[x %in% vm$map[[label]]] <- label
  }

  unmapped <- unique(x[is.na(out) & !is.na(x)])
  if(length(unmapped)) {
    if(is.null(vm$default)) {
      stop('These values were not mapped', if(!is.null(role)) paste0(' for `', role, '`') else '',
           ': ', paste(unmapped, collapse = ', '),
           '. Add them to `value_map()`, or set `.default` to group them.', call. = FALSE)
    }
    ## `.default` is a catch-all, so say what it caught -- a status left out of the
    ## map by mistake would otherwise be misclassified without a trace. Not when the
    ## catch-all IS the declaration (`binary_outcome(bad = ...)` without `good`).
    if(!isTRUE(vm$implicit_default)) warning('`.default = "', vm$default, '"', if(!is.null(role)) paste0(' for `', role, '`') else '',
            ' absorbed unlisted value(s): ', paste(unmapped, collapse = ', '),
            '. Check that this is intended.', call. = FALSE)
    out[is.na(out) & !is.na(x)] <- vm$default
  }

  factor(out, levels = unique(c(names(vm$map), vm$default)))
}

## Fallback used when no value_map() is supplied: guess which raw status is the
## approved one (outcomes are observable only for approved applications) and
## treat every other status as rejected. This reproduces the package's previous
## behaviour, so results do not change silently -- supplying a value_map() is
## what lets `cancelled` be excluded from the approval-rate denominator.
detect_status_map <- function(target, application_status) {
  tt <- table(target, application_status, exclude = NULL) %>%
    prop.table(margin = 2) %>%
    as.data.frame.matrix()

  if(!any(rownames(tt) == 'NA.')) {
    stop('Could not infer which application status means `approved`, because no ',
         'application has a missing outcome. Supply `value_map()` for the ',
         'application status.', call. = FALSE)
  }
  na_row <- tt[rownames(tt) == 'NA.', ]
  approved_raw <- colnames(na_row)[which.min(unlist(na_row))]

  other_raw <- setdiff(as.character(unique(stats::na.omit(application_status))), approved_raw)

  warning('No `value_map()` supplied for the application status. Inferred ',
          'approved = "', approved_raw, '"',
          if(length(other_raw)) paste0(' and treated ', paste0('"', other_raw, '"', collapse = ', '),
                                       ' as rejected') else '',
          '. Supply `value_map()` to control this -- in particular to mark ',
          'cancelled applications, which are excluded from the approval rate.',
          call. = FALSE)

  args <- list(approved = approved_raw)
  if(length(other_raw)) args$rejected <- other_raw
  do.call(value_map, args)
}

#' Declare a binary outcome
#'
#' Says which raw values of an outcome column mean `bad` (and optionally which
#' mean `good`), so that every function counts the same event. Nothing is
#' guessed: without a declaration the package falls back to inferring the bad
#' class, with a warning.
#'
#' @param column Name of the outcome column. May be `NULL` when the declaration
#'   is used with a vector.
#' @param bad Raw value(s) that mark a bad outcome, e.g. `bad = 1` or
#'   `bad = c("DEFAULTED", "WRITE_OFF")`.
#' @param good Raw value(s) that mark a good outcome. When `NULL` (the default)
#'   every other non-missing value is good; when given, any value listed in
#'   neither `bad` nor `good` is an error.
#' @return A `loan_binary_outcome` (a [value_map()] onto `good` / `bad`).
#' @export
#' @examples
#' binary_outcome("fpd15", bad = 1)
#' binary_outcome("loan_status", bad = c("DEFAULTED", "WRITE_OFF"),
#'                good = c("PAID_OFF", "PERFORMING", "PAST_DUE"))
binary_outcome <- function(column = NULL, bad, good = NULL) {
  if(missing(bad)) {
    stop('`binary_outcome()` needs `bad`: the raw value(s) that mark a bad outcome, ',
         'e.g. `bad = 1`.', call. = FALSE)
  }
  vm <- if(is.null(good)) value_map(column, bad = bad, .default = 'good')
        else value_map(column, bad = bad, good = good)
  vm$implicit_default <- is.null(good)
  class(vm) <- c('loan_binary_outcome', class(vm))
  vm
}

## Map an undeclared two-class outcome onto canonical good / bad. Recognises
## good/paid and bad/default by name; otherwise assumes the majority class is
## good -- and says so, because that is a guess.
detect_binary_outcome <- function(outcome) {
  tabl <- table(outcome, exclude = NA)
  tabl_names <- names(tabl)

  if(length(tabl_names) != 2L) {
    stop('Could not map the outcome onto good / bad: it has ', length(tabl_names),
         ' distinct value(s). Declare it with `binary_outcome()`.', call. = FALSE)
  }

  possible_good <- stringr::str_detect(tolower(tabl_names), 'good|paid')
  possible_bad  <- stringr::str_detect(tolower(tabl_names), 'bad|default')

  if(sum(possible_good) == 2 | sum(possible_bad) == 2) {
    stop('Ambiguous outcome labels: ', paste(tabl_names, collapse = ', '),
         '. Declare the outcome with `binary_outcome()`.', call. = FALSE)
  }
  if(sum(possible_good) == 1) {
    good_name <- tabl_names[possible_good]
  } else if(sum(possible_bad) == 1) {
    good_name <- tabl_names[!possible_bad]
  } else {
    good_name <- names(tabl)[which.max(tabl)]
    warning('No `binary_outcome()` declared: treating the majority value "', good_name,
            '" as good and "', setdiff(tabl_names, good_name), '" as bad. ',
            'Declare it, e.g. `binary_outcome("<column>", bad = ...)`.', call. = FALSE)
  }

  out <- ifelse(is.na(outcome), NA_character_,
                ifelse(as.character(outcome) == good_name, 'good', 'bad'))
  factor(out, levels = c('good', 'bad'))
}
