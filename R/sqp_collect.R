sqp_collect <- function(...) {
  selected_vars <- as.character(substitute(list(...))[-1])

  sqp_data <-
    suppressWarnings(
      suppressMessages(
        read_csv2("SQPexport_20171230_2255.csv")
      )) %>%
    transmute(
      question = tolower(`Question name`),
      quality = as.numeric(`Quality (q2)`),
      reliability = as.numeric(`Reliability (r2)`),
      validity = as.numeric(`Validity (v2)`)
    )

  sqp_data
}
