#' LPI canopy species count by transect and stratum
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param spring Spring code to generate a plot for, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#' @param plot.title Optional custom plot title. Leave blank to use a sensible default. Use "" to omit the title.
#' @param sub.title Optional custom plot subtitle.  Leave blank to use a sensible default. Use "" to omit the subtitle.
#' @param x.lab Optional X axis label. Leave blank to use a sensible default. Use "" to omit the axis label.
#' @param y.lab Y axis label. Leave blank to use a sensible default. Use "" to omit the axis label.
#' @param ymax Optional maximum y limit.
#' @param ymin Optional minimum y limit.
#' @param xmax Optional maximum x limit.
#' @param xmin Optional minimum x limit.
#'
#' @return A ggplot object.
#' @export
#'
#' @details Omits TBD and UNK species from counts. Only includes data from visits labeled 'Primary.'
#'
#' @importFrom magrittr %>% %<>%
#'
BoxplotSpeciesByStratum <- function(conn, path.to.data, spring, field.season, data.source = "database", plot.title, sub.title, x.lab, y.lab, ymax, ymin, xmax, xmin) {
  if (missing(spring)) {
    stop("Spring code must be specified")
  }

  data <- CountSpeciesByStratum(conn = conn, path.to.data = path.to.data, spring = spring, field.season = field.season, data.source = data.source)
  spring.name <- GetSpringName(conn, path.to.data, spring, data.source)

  if (missing(field.season)) {
    field.season <- unique(data$FieldSeason)
  }

  if (missing(x.lab)) {
    x.lab <- "Stratum"
  }

  if (missing(y.lab)) {
    y.lab <- "Transect-level species count"
  }

  if (missing(plot.title)) {
    plot.title = "Number of species per stratum"
  }

  sample.size <- GetSampleSizes(data)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = factor(Stratum, levels = c("T", "M", "B", "ND")), y = SpeciesCount)) +
    ggplot2::geom_boxplot()

  p <- FormatPlot(p, spring, spring.name, field.season, sample.size, plot.title = plot.title, sub.title = sub.title, x.lab = x.lab, y.lab = y.lab, ymax = ymax, ymin = ymin, xmax = xmax, xmin = xmin)

  return(p)
}

#' Table of species detected through LPI vs veg inventory
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param spring Optional. Spring code to generate a plot for, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#' @param dt.options Optional. Options to be passed to DT::datatable().
#' @param ... Optional. Additional arguments to be passed to DT::datatable().
#'
#' @return An HTML widget.
#' @export
#'
#' @details Omits TBD and UNK species from counts. Only includes data from visits labeled 'Primary.'
#'
#' @importFrom magrittr %>% %<>%
#'
TableSpeciesPerTransect <- function(conn, path.to.data, park, spring, field.season, dt.options, data.source = "database", ...) {
  data <- CountSpeciesDetected(conn, path.to.data, park, spring, field.season, data.source) %>%
    dplyr::arrange(Park, FieldSeason, SpringCode, TransectNumber)

  col.names <- gsub("([[:upper:]][[:lower:]])", " \\1", names(data)) %>% trimws()
  n.rows <- nrow(data)
  n.cols <- length(col.names)

  if (missing(dt.options)) {
    dt.options <- list(dom = "t",
                       pageLength = n.rows,
                       columnDefs = list(list(className = 'dt-center', targets = 0:n.cols-1)))
  }

  tbl <- DT::datatable(data, colnames = col.names, rownames = FALSE, options = dt.options, ...)

  return(tbl)
}

#' Histogram of LPI canopy cover
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param spring Spring code to generate a plot for, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#' @param plot.title Optional custom plot title. Leave blank to use a sensible default. Use "" to omit the title.
#' @param sub.title Optional custom plot subtitle.  Leave blank to use a sensible default. Use "" to omit the subtitle.
#' @param x.lab Optional X axis label. Leave blank to use a sensible default. Use "" to omit the axis label.
#' @param y.lab Y axis label. Leave blank to use a sensible default. Use "" to omit the axis label.
#' @param breaks Optional numeric vector giving histogram bin boundaries. Defaults to \code{seq(0, 100, by = 10)} to give 10 bins of width 10.
#' @param ymax Optional maximum y limit.
#'
#' @return A ggplot object.
#' @export
#'
#' @details Only includes data from visits labeled 'Primary.' Counts both plant canopy and other canopy (e.g. litter) as cover. Does not take into account the number of canopy layers, only presence/absence of canopy at a point. I.e., a point with a single plant counts the same as a point with multiple plants in multiple strata. Percent cover is calculated only from points at which data were collected - if only three points were recorded for a transect and one of them had canopy cover, that transect would have 33.3% cover.
#'
#' @importFrom magrittr %>% %<>%
#'
HistogramCanopyPercentCover <- function(conn, path.to.data, spring, field.season, data.source = "database", plot.title, sub.title, x.lab, y.lab, breaks = seq(0, 100, by = 10), ymax) {
  if (missing(spring)) {
    stop("Spring code must be specified")
  }

  data <- CanopyPercentCover(conn = conn, path.to.data = path.to.data, spring = spring, field.season = field.season, data.source = data.source)
  spring.name <- GetSpringName(conn, path.to.data, spring, data.source)

  if (missing(field.season)) {
    field.season <- unique(data$FieldSeason)
  }

  if (missing(plot.title)) {
    plot.title = "Canopy cover"
  }

  if (missing(x.lab)) {
    x.lab <- "Canopy cover (%)"
  }

  if (missing(y.lab)) {
    y.lab <- "Percentage of transects"
  }
  sample.size <- GetSampleSizes(data)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = CanopyCover_percent, y = 100 * (..count..)/sum(..count..))) +
    ggplot2::geom_histogram(position = "dodge", breaks = breaks)

  p <- FormatPlot(p, spring, spring.name, field.season, sample.size, plot.title, sub.title, x.lab = x.lab, y.lab = y.lab, ymax = ymax, xmin = 0, xmax = 100)

  return(p)
}

#' Boxplot comparing LPI species richness to inventory species richness
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param spring Spring code to generate a plot for, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#' @param plot.title Optional custom plot title. Leave blank to use a sensible default. Use "" to omit the title.
#' @param sub.title Optional custom plot subtitle.  Leave blank to use a sensible default. Use "" to omit the subtitle.
#' @param x.lab Optional X axis label. Leave blank to use a sensible default. Use "" to omit the axis label.
#' @param y.lab Y axis label. Leave blank to use a sensible default. Use "" to omit the axis label.
#' @param ymax Optional maximum y limit.
#' @param ymin Optional minimum y limit.
#' @param xmax Optional maximum x limit.
#' @param xmin Optional minimum x limit.
#'
#' @return A ggplot object.
#' @export
#'
#' @details Omits TBD and UNK species from counts. Only includes data from visits labeled 'Primary.'
#'
#' @importFrom magrittr %>% %<>%
#'
BoxplotSpeciesRichnessBySOP <- function(conn, path.to.data, spring, field.season, data.source = "database", plot.title, sub.title, x.lab, y.lab, ymax, ymin, xmax, xmin) {
  if (missing(spring)) {
    stop("Spring code must be specified")
  }

  data <- CountSpeciesDetected(conn = conn, path.to.data = path.to.data, spring = spring, field.season = field.season, data.source = data.source) %>%
    tidyr::gather("SOP", "SpeciesRichness", c("LPISpeciesCount", "InventorySpeciesCount"))

  has.inventory <- data %>%
    dplyr::group_by(Park, SpringCode, SpringName, FieldSeason, SOP) %>%
    dplyr::summarise(HasInventory = !all(is.na(SpeciesRichness))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(SOP == "InventorySpeciesCount") %>%
    dplyr::select(-SOP)

  data <- dplyr::left_join(data, has.inventory, by = c("Park", "SpringCode", "SpringName", "FieldSeason")) %>%
    dplyr::filter(HasInventory == TRUE)

  spring.name <- GetSpringName(conn, path.to.data, spring, data.source)

  if (missing(field.season)) {
    field.season <- unique(data$FieldSeason)
  }

  if (missing(plot.title)) {
    plot.title = "Species richness"
  }

  if (missing(x.lab)) {
    x.lab <- "SOP"
  }

  if (missing(y.lab)) {
    y.lab <- "Number of species per transect"
  }
  sample.size <- GetSampleSizes(data)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = factor(SOP, levels = c("LPISpeciesCount", "InventorySpeciesCount"), labels = c("LPI", "Inventory")), y = SpeciesRichness)) +
    ggplot2::geom_boxplot()

  p <- FormatPlot(p, spring, spring.name, field.season, sample.size, plot.title = plot.title, sub.title = sub.title, x.lab = x.lab, y.lab = y.lab, ymax = ymax, ymin = ymin, xmax = xmax, xmin = xmin)

  return(p)
}

#' Boxplot of LPI species richness over time
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param spring Spring code to generate a plot for, e.g. "LAKE_P_BLUE0".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#' @param plot.title Optional custom plot title. Leave blank to use a sensible default. Use "" to omit the title.
#' @param sub.title Optional custom plot subtitle.  Leave blank to use a sensible default. Use "" to omit the subtitle.
#' @param x.lab Optional X axis label. Leave blank to use a sensible default. Use "" to omit the axis label.
#' @param y.lab Y axis label. Leave blank to use a sensible default. Use "" to omit the axis label.
#' @param ymax Optional maximum y limit.
#' @param ymin Optional minimum y limit.
#' @param xmax Optional maximum x limit.
#' @param xmin Optional minimum x limit.
#'
#' @return A ggplot object.
#' @export
#'
#' @details Omits TBD and UNK species from counts. Only includes data from visits labeled 'Primary.'
#'
#' @importFrom magrittr %>% %<>%
#'
BoxplotLPISpeciesRichness <- function(conn, path.to.data, spring, data.source = "database", plot.title, sub.title, x.lab, y.lab, ymax, ymin, xmax, xmin) {
  if (missing(spring)) {
    stop("Spring code must be specified")
  }

  data <- CountLPISpeciesDetected(conn = conn, path.to.data = path.to.data, spring = spring, data.source = data.source)

  spring.name <- GetSpringName(conn, path.to.data, spring, data.source)

  if (missing(plot.title)) {
    plot.title = "LPI species richness"
  }

  if (missing(x.lab)) {
    x.lab <- "Field season"
  }

  if (missing(y.lab)) {
    y.lab <- "Number of species per transect"
  }
  sample.size <- GetSampleSizes(data)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = FieldSeason, y = LPISpeciesCount)) +
    ggplot2::geom_boxplot()

  p <- FormatPlot(p, spring, spring.name, sample.sizes = sample.size, plot.title = plot.title, sub.title = sub.title, x.lab = x.lab, y.lab = y.lab, ymax = ymax, ymin = ymin, xmax = xmax, xmin = xmin)

  return(p)
}

#' Boxplot of inventory species richness over time
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param spring Spring code to generate a plot for, e.g. "LAKE_P_BLUE0".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#' @param plot.title Optional custom plot title. Leave blank to use a sensible default. Use "" to omit the title.
#' @param sub.title Optional custom plot subtitle.  Leave blank to use a sensible default. Use "" to omit the subtitle.
#' @param x.lab Optional X axis label. Leave blank to use a sensible default. Use "" to omit the axis label.
#' @param y.lab Y axis label. Leave blank to use a sensible default. Use "" to omit the axis label.
#' @param ymax Optional maximum y limit.
#' @param ymin Optional minimum y limit.
#' @param xmax Optional maximum x limit.
#' @param xmin Optional minimum x limit.
#'
#' @return A ggplot object.
#' @export
#'
#' @details Omits TBD and UNK species from counts. Only includes data from visits labeled 'Primary.'
#'
#' @importFrom magrittr %>% %<>%
#'
BoxplotInvSpeciesRichness <- function(conn, path.to.data, spring, data.source = "database", plot.title, sub.title, x.lab, y.lab, ymax, ymin, xmax, xmin) {
  if (missing(spring)) {
    stop("Spring code must be specified")
  }

  data <- CountInvSpeciesDetected(conn = conn, path.to.data = path.to.data, spring = spring, data.source = data.source)

  spring.name <- GetSpringName(conn, path.to.data, spring, data.source)

  if (missing(plot.title)) {
    plot.title = "Inventory species richness"
  }

  if (missing(x.lab)) {
    x.lab <- "Field season"
  }

  if (missing(y.lab)) {
    y.lab <- "Number of species per transect"
  }

  sample.size <- GetSampleSizes(data)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = FieldSeason, y = InventorySpeciesCount)) +
    ggplot2::geom_boxplot()

  p <- FormatPlot(p, spring, spring.name, sample.sizes = sample.size, plot.title = plot.title, sub.title = sub.title, x.lab = x.lab, y.lab = y.lab, ymax = ymax, ymin = ymin, xmax = xmax, xmin = xmin)

  return(p)
}

#'Species accumulation curve of one spring in one field season
#'
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param spring Spring code to generate a plot for, e.g. "LAKE_P_BLUE0".
#' @param field.season Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#' @param plot.title Optional custom plot title. Leave blank to use a sensible default. Use "" to omit the title.
#' @param sub.title Optional custom plot subtitle.  Leave blank to use a sensible default. Use "" to omit the subtitle.
#' @param x.lab Optional X axis label. Leave blank to use a sensible default. Use "" to omit the axis label.
#' @param y.lab Y axis label. Leave blank to use a sensible default. Use "" to omit the axis label.
#' @param ymax Optional maximum y limit.
#' @param ymin Optional minimum y limit.
#' @param xmax Optional maximum x limit.
#' @param xmin Optional minimum x limit.
#'
#' @return A ggplot object.
#' @export
#'
#' @details Only includes data from visits labeled 'Primary.' Does not include UNKS but does include To Be Determined unknown species.
#'
#' @importFrom magrittr %>% %<>%
#'
SpeciesAccumulationCurve <- function(conn, path.to.data, spring, field.season, data.source = "database", plot.title, sub.title, x.lab, y.lab, ymax, ymin, xmax, xmin) {
  if (missing(spring)) {
    stop("Spring code must be specified")
  }

  data <- CalculateSpeciesAccumulation(conn = conn, path.to.data = path.to.data, spring = spring, field.season = field.season, data.source = data.source)
  spring.name <- GetSpringName(conn, path.to.data, spring, data.source)

  if (missing(field.season)) {
    stop("Field season must be specified")
  }

  if (missing(x.lab)) {
    x.lab <- "Sampling effort (num of transects)"
  }

  if (missing(y.lab)) {
    y.lab <- "Species richness"
  }

  if (missing(plot.title)) {
    plot.title = "Species accumulation curve"
  }

  sample.size <- tibble::tibble(SpringCode = spring,
                                FieldSeason = field.season,
                                NTransects = max(data$Transects))

  p <- ggplot2::ggplot(data, ggplot2::aes(x = Transects, y = Richness)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_errorbar(ggplot2::aes(x = Transects, ymin = Richness - StDev, ymax = Richness + StDev, width = .2))

  p <- FormatPlot(p, spring, spring.name, field.season, sample.size, plot.title = plot.title, sub.title = sub.title, x.lab = x.lab, y.lab = y.lab, ymax = ymax, ymin = ymin, xmax = xmax, xmin = xmin)

  return(p)
}

#' Table of tree presence/absence by spring and field season
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param spring Optional. Spring code to generate a plot for, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#' @param dt.options Optional. Options to be passed to DT::datatable().
#' @param ... Optional. Additional arguments to be passed to DT::datatable().
#'
#' @return An HTML widget.
#' @export
#'
#' @details Only includes data from visits labeled 'Primary.'
#'
#' @importFrom magrittr %>% %<>%
#'
TableTreePresenceAbsence <- function(conn, path.to.data, park, spring, field.season, dt.options, data.source = "database", ...) {
  data <- TreePresenceAbsence(conn, path.to.data, park, spring, field.season, data.source) %>%
    dplyr::arrange(Park, FieldSeason, SpringCode)

  col.names <- gsub("([[:upper:]][[:lower:]])", " \\1", names(data)) %>% trimws()
  col.names[5:6] <- c("# of Transects With Trees", "# of Transects Without Trees")

  n.rows <- nrow(data)
  n.cols <- length(col.names)

  if (missing(dt.options)) {
    dt.options <- list(dom = "t",
                       pageLength = n.rows,
                       columnDefs = list(list(className = 'dt-center', targets = 0:n.cols-1)))
  }

  tbl <- DT::datatable(data, colnames = col.names, rownames = FALSE, options = dt.options, ...)

  return(tbl)
}

#' Boxplot of water percent cover over time
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param spring Spring code to generate a plot for, e.g. "LAKE_P_BLUE0".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#' @param plot.title Optional custom plot title. Leave blank to use a sensible default. Use "" to omit the title.
#' @param sub.title Optional custom plot subtitle.  Leave blank to use a sensible default. Use "" to omit the subtitle.
#' @param x.lab Optional X axis label. Leave blank to use a sensible default. Use "" to omit the axis label.
#' @param y.lab Y axis label. Leave blank to use a sensible default. Use "" to omit the axis label.
#' @param ymax Optional maximum y limit.
#' @param ymin Optional minimum y limit.
#' @param xmax Optional maximum x limit.
#' @param xmin Optional minimum x limit.
#'
#' @return A ggplot object.
#' @export
#'
#' @details Only includes data from visits labeled 'Primary.' Points with water recorded as "NA" and "ND" are omitted. Omits Blue Point transects 1 - 3 in 2019 since water presence/absence was not yet being recorded consistently.
#'
#' @importFrom magrittr %>% %<>%
#'
BoxplotWaterPercentCover <- function(conn, path.to.data, spring, data.source = "database", plot.title, sub.title, x.lab, y.lab, ymax, ymin, xmax, xmin) {
  if (missing(spring)) {
    stop("Spring code must be specified")
  }

  data <- WaterPercentCover(conn = conn, path.to.data = path.to.data, spring = spring, data.source = data.source)

  spring.name <- GetSpringName(conn, path.to.data, spring, data.source)

  if (missing(plot.title)) {
    plot.title = "Water percent cover"
  }

  if (missing(x.lab)) {
    x.lab <- "Field season"
  }

  if (missing(y.lab)) {
    y.lab <- "Water percent cover per transect"
  }

  sample.size <- GetSampleSizes(data)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = FieldSeason, y = WaterCover_percent)) +
    ggplot2::geom_boxplot()

  p <- FormatPlot(p, spring, spring.name, sample.sizes = sample.size, plot.title = plot.title, sub.title = sub.title, x.lab = x.lab, y.lab = y.lab, ymax = ymax, ymin = ymin, xmax = xmax, xmin = xmin)

  return(p)
}
