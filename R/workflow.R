define_uw_workflow <- function(x, expr) {
  x %>% rlang::enexpr(expr)
}

## Rules description here
#define_uw_workflow(x, )
