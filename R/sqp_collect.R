sqp_collect <- function(...) {
  selected_vars <- as.character(substitute(list(...))[-1])

  ex <- suppressMessages(
    suppressWarnings(
      readr::read_csv2("data/SQPexport_20171230_2255.csv")
    )
  )

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
