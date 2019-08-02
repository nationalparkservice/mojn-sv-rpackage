#' Disturbance percent cover, broken down by disturbance type.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param spring Spring code to generate a plot for, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param drop.nonexistent Omit disturbances that were not observed at all? Defaults to FALSE.
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#' @param plot.title Optional custom plot title.
#' @param sub.title Optional custom plot subtitle.
#' @param ymax Optional maximum y limit.
#' @param ymin Optional minimum y limit.
#' @param xmax Optional maximum x limit.
#' @param xmin Optional minimum x limit.
#'
#' @return A ggplot object.
#' @export
#'
#' @details Only includes data from visits labeled 'Primary.' Percent cover is calculated only from points at which data were collected - if only three points were recorded for a transect and one of them had bike disturbance, the disturbance type BK would have 33.3% cover for that transect.
#'
#' @importFrom magrittr %>% %<>%
#'
BoxplotDisturbanceCover <- function(conn, path.to.data, spring, field.season, drop.nonexistent = FALSE, data.source = "database", plot.title, sub.title, ymax, ymin, xmax, xmin) {
  if (missing(spring)) {
    stop("Spring code must be specified")
  }

  data <- DisturbancePercentCover(conn, path.to.data, spring = spring, field.season = field.season, drop.zeroes = FALSE, data.source = data.source)
  spring.name <- GetSpringName(conn, path.to.data, spring, data.source)

  if (drop.nonexistent) {
    dist.present <- data %>%
      dplyr::group_by(Park, SpringCode, SpringName, FieldSeason, DisturbanceCode) %>%
      dplyr::summarise(DisturbanceFound = (sum(DisturbanceCover_percent) > 0)) %>%
      dplyr::ungroup()
    data %<>% dplyr::right_join(dist.present, by = c("Park", "SpringCode", "SpringName", "FieldSeason", "DisturbanceCode")) %>%
      dplyr::filter(DisturbanceFound)
  }

  dist.types <- GetDisturbanceTypes()$Code %>%
    intersect(unique(data$DisturbanceCode))

  if (missing(field.season)) {
    field.season <- unique(data$FieldSeason)
  }

  if (missing(plot.title)) {
    plot.title = "Disturbance cover"
  }

  sample.size <- GetSampleSizes(data)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = factor(DisturbanceCode, levels = dist.types), y = DisturbanceCover_percent)) +
    ggplot2::geom_boxplot() +
    ggplot2::xlab("Disturbance type") +
    ggplot2::ylab("Cover (%)")
  p <- FormatPlot(p, spring, spring.name, field.season, sample.size, plot.title = plot.title, sub.title = sub.title, ymax = ymax, ymin = ymin, xmax = xmax, xmin = xmin)

  return(p)
}
