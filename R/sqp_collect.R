sqp_collect <- function(...) {
  selected_vars <- as.character(substitute(list(...))[-1])

  load("sqp_data_fin")
}
