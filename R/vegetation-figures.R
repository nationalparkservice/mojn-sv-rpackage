#' LPI canopy species count by transect and stratum
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param spring Spring code to generate a plot for, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns Park, SpringCode, SpringName, FieldSeason, TransectNumber, Stratum, SpeciesCount
#' @export
#'
#' @details Omits TBD and UNK species from counts. Only includes data from visits labeled 'Primary.'
#'
#' @importFrom magrittr %>% %<>%
#'
BoxplotSpeciesByStratum <- function(conn, path.to.data, spring, field.season, data.source = "database") {
  if (missing(spring)) {
    stop("Spring code must be specified")
  }

  data <- CountSpeciesByStratum(conn = conn, path.to.data = path.to.data, spring = spring, field.season = field.season, data.source = data.source)
  spring.name <- GetSpringName(conn, path.to.data, spring, data.source)

  if (missing(field.season)) {
    field.season <- unique(data$FieldSeason)
  }
  sample.size <- GetSampleSizes(data)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = factor(Stratum, levels = c("T", "M", "B", "ND")), y = SpeciesCount)) +
    ggplot2::geom_boxplot() +
    ggplot2::xlab("Stratum") +
    ggplot2::ylab("Transect-level species count")
  p <- FormatPlot(p, spring, spring.name, field.season, sample.size, plot.title = "Number of LPI species detected by stratum")

  return(p)
}
