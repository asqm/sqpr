sqp_collect <- function(...) {
  selected_vars <- as.character(substitute(list(...))[-1])

  ex <- file.path(find.package("sqpr"), "/data/Rdata.rds")

    sqp_data_fin <-
      dplyr::transmute(
      ex,
      question = tolower(`Question name`),
      quality = as.numeric(`Quality (q2)`),
      reliability = as.numeric(`Reliability (r2)`),
      validity = as.numeric(`Validity (v2)`)
    )

  sqp_data_fin
}
