#' Weight-of-Evidence (WOE) Binning Recipe Step Using scorecard Package
#'
#' `step_woe2` creates a specification of a recipe step that converts numeric
#' predictors into weight-of-evidence (WOE) values using functions provided by the
#' **scorecard** package (i.e. `scorecard::woebin` and `scorecard::woebin_ply`).
#' The step can either replace the original numeric columns with their WOE-transformed
#' values or add new columns (named by appending `_woe` to the original names) to the dataset.
#'
#' @param recipe A recipe object. The step will be added to the sequence of existing steps.
#' @param ... One or more selector functions to choose variables for WOE binning (e.g., `all_numeric()`).
#' @param target A variable in the data used as the outcome for WOE binning. Unquoted.
#' @param positive The level in the target variable that represents the "positive" event. Unquoted.
#' @param replace Logical. If `TRUE` (default), the original variables are replaced by their
#'   WOE-transformed values; if `FALSE`, new columns are created alongside the originals.
#' @param new_role A character string specifying the role to assign to any new columns (when `replace = FALSE`). Defaults to `"predictor"`.
#' @param role A character string for the role of the created variables. Defaults to `"predictor"`.
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated. Default is `FALSE`.
#' @param bins A list of binning objects produced by `scorecard::woebin`. This is `NULL` until computed by `prep()`.
#' @param columns A character vector of column names that will be processed by the step. This is `NULL` until computed by `prep()`.
#' @param new_vars A character vector of new variable names (when `replace = FALSE`). This is `NULL` until computed by `prep()`.
#' @param skip A logical. Should the step be skipped when the recipe is baked by `bake.recipe`? Defaults to `FALSE`.
#' @param id A unique character string to identify the step.
#'
#' @details
#' This step uses functions from the **scorecard** package (namely, `scorecard::woebin` and
#' `scorecard::woebin_ply`) to compute WOE bins for the selected predictors based on a target variable.
#' When `replace = TRUE`, the numeric columns selected are overwritten with their WOE-transformed
#' values. When `replace = FALSE`, new columns (with names suffixed by `_woe`) are added and
#' assigned the role specified by `new_role` (by default, "predictor").
#'
#' The step defaults to assigning the created (or replaced) variables the role `"predictor"`.
#'
#' @return An updated recipe object with the new WOE binning step added.
#'
#' @examples
#' \dontrun{
#' library(recipes)
#' library(caret)
#'
#' # Simulate some data
#' set.seed(123)
#' n <- 1000
#' x1 <- rnorm(n)
#' x2 <- rnorm(n)
#' x3 <- rnorm(n)
#' x4 <- rnorm(n)
#' x5 <- rnorm(n)
#' cat1 <- sample(c("A", "B", "C"), n, replace = TRUE)
#' target_prob <- plogis(0.5 * x1 - 0.3 * x2 + 0.1 * x3)
#' creditability <- factor(ifelse(runif(n) < target_prob, "Good", "Bad"))
#' data_sim <- data.frame(x1, x2, x3, x4, x5, cat1, creditability)
#'
#' # Split data into training and testing sets
#' set.seed(123)
#' train_idx <- caret::createDataPartition(data_sim$creditability, p = 0.7, list = FALSE)
#' train_data <- data_sim[train_idx, ]
#' test_data  <- data_sim[-train_idx, ]
#'
#' # Example 1: Replace original numeric columns with WOE values (using scorecard functions)
#' rec_woe2_replace <- recipe(creditability ~ ., data = train_data) %>%
#'   step_woe2(all_numeric(), target = creditability, positive = Good, replace = TRUE) %>%
#'   step_dummy(all_nominal(), -all_outcomes()) %>%
#'   step_zv(all_predictors())
#'
#' rec_woe2_replace <- prep(rec_woe2_replace, training = train_data)
#' baked_replace <- bake(rec_woe2_replace, new_data = test_data)
#'
#' # Example 2: Append new WOE columns while keeping the original numeric columns.
#' rec_woe2_newcols <- recipe(creditability ~ ., data = train_data) %>%
#'   step_woe2(all_numeric(), target = creditability, positive = Good, replace = FALSE) %>%
#'   step_dummy(all_nominal(), -all_outcomes()) %>%
#'   step_zv(all_predictors())
#'
#' rec_woe2_newcols <- prep(rec_woe2_newcols, training = train_data)
#' baked_newcols <- bake(rec_woe2_newcols, new_data = test_data)
#' }
#'
#' @seealso [recipes::step()], [scorecard::woebin()], [scorecard::woebin_ply()]
#'
#' @export
step_woe2 <- function(recipe, ..., 
                      target, 
                      positive, 
                      replace = TRUE, 
                      new_role = "predictor", 
                      role = "predictor", 
                      trained = FALSE,
                      bins = NULL, 
                      columns = NULL, 
                      new_vars = NULL, 
                      skip = FALSE,
                      id = recipes::rand_id("woe2")) {
  # Capture target and positive as strings using rlang
  target_name <- rlang::as_name(rlang::enquo(target))
  positive_name <- rlang::as_name(rlang::enquo(positive))
  
  recipes::add_step(recipe,
                    step_woe2_new(
                      terms = rlang::enquos(...),
                      target = target_name,
                      positive = positive_name,
                      replace = replace,
                      new_role = new_role,
                      role = role,
                      trained = trained,
                      bins = bins,
                      columns = columns,
                      new_vars = new_vars,
                      skip = skip,
                      id = id
                    )
  )
}

#' @rdname step_woe2
#' @export
prep.step_woe2 <- function(x, training, info = NULL, ...) {
  col_names <- recipes::recipes_eval_select(x$terms, info = info, data = training)
  bins <- scorecard::woebin(training, y = x$target, x = col_names, positive = x$positive)
  bins <- lapply(bins, function(b) {
    b$is_weight <- rep(FALSE, nrow(b))
    b
  })
  new_vars <- if (x$replace) col_names else paste0(col_names, "_woe")
  if (!x$replace) {
    new_info <- tibble::tibble(variable = new_vars,
                               type = rep("numeric", length(new_vars)),
                               role = rep(x$new_role, length(new_vars)))
    info <- dplyr::bind_rows(info, new_info)
  }
  step_woe2_new(
    terms = x$terms,
    target = x$target,
    positive = x$positive,
    replace = x$replace,
    new_role = x$new_role,
    role = x$role,
    trained = TRUE,
    bins = bins,
    columns = col_names,
    new_vars = new_vars,
    skip = x$skip,
    id = x$id
  )
}

#' @rdname step_woe2
#' @export
bake.step_woe2 <- function(object, new_data, ...) {
  predictors <- intersect(object$columns, names(new_data))
  bins_fixed <- object$bins[names(object$bins) %in% predictors]
  bins_fixed <- lapply(bins_fixed, function(b) {
    b$is_weight <- rep(FALSE, nrow(b))
    b
  })
  new_data <- scorecard::woebin_ply(new_data, bins = bins_fixed, replace = object$replace)
  new_data
}

#' @rdname step_woe2
#' @export
print.step_woe2 <- function(x, width = max(20, options()$width - 30), ...) {
  cat("WOE binning using scorecard functions for ")
  recipes::printer(x$columns, x$terms, x$trained, width = width)
  invisible(x)
}

# Internal constructor for step_woe2
step_woe2_new <- function(terms, target, positive, replace, new_role, role, trained, bins, columns, new_vars, skip, id) {
  recipes::step(
    subclass = "woe2",
    terms = terms,
    target = target,
    positive = positive,
    replace = replace,
    new_role = new_role,
    role = role,
    trained = trained,
    bins = bins,
    columns = columns,
    new_vars = new_vars,
    skip = skip,
    id = id
  )
}