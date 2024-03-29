#' Open a Connection to the Spring Veg Database
#'
#' @param use.mojn.default Connect to the MOJN Spring Veg database? MOJN staff should use this option. Defaults to \code{TRUE}.
#' @param drv DBI driver to use. Defaults to \code{odbc::odbc()}.
#' @param ... Additional arguments to \code{\link[pool]{dbPool}}. Ignored if \code{use.mojn.default} is \code{TRUE}.
#'
#' @return A database connection pool object
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#' }
OpenDatabaseConnection <- function(use.mojn.default = TRUE, drv = odbc::odbc(), ...) {

  if (use.mojn.default) {
    params <- readr::read_csv("M:/MONITORING/SV/Data/Database/ConnectFromR/sv-database-conn.csv") %>%
      as.list()
    params$drv <- drv
    my.pool <- do.call(pool::dbPool, params)
  } else {
    my.pool <- pool::dbPool(drv = drv, ...)
  }

  return(my.pool)
}

#' Close a Connection to the Spring Veg Database
#'
#' @param conn A database connection pool object generated from a call to \code{OpenDatabaseConnection()}
#'
#' @return None.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' conn <- OpenDatabaseConnection()
#' CloseDatabaseConnection(conn)
CloseDatabaseConnection <- function(conn) {
  pool::poolClose(conn)
}

#' Save spring vegetation analysis views as a set of .csv files
#'
#' @param conn A database connection pool object generated from a call to \code{OpenDatabaseConnection()}.
#' @param dest.folder The folder in which to save the .csv files.
#' @param create.folders Should \code{dest.folder} be created automatically if it doesn't exist? Defaults to \code{FALSE}.
#' @param overwrite Should existing data be automatically overwritten? Defaults to \code{FALSE}.
#'
#' @return None.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     SaveDataToCsv(conn, "C:/Users/myusername/Documents/R/spring-veg-data", TRUE, TRUE)
#'     CloseDatabaseConnection(conn)
#' }
SaveDataToCsv <- function(conn, dest.folder, create.folders = FALSE, overwrite = FALSE) {
  analysis.views <- c("LPICanopy", "LPIDisturbance", "LPISoilSurface", "LPITransect", "Spring", "SpringVisit", "TreeCount", "TreeCountTransect", "VegetationInventory", "VegetationInventoryTransect")
  dest.folder <- file.path(dirname(dest.folder), basename(dest.folder))  # Get destination directory in a consistent format. Seems like there should be a better way to do this.
  file.paths <- file.path(dest.folder, paste0(analysis.views, ".csv"))

  # Validate inputs
  if (!dir.exists(dest.folder)) {
    if(create.folders) {
      dir.create(dest.folder, recursive = TRUE)
    } else {
      stop("Destination folder does not exist. To create it automatically, set create.folders to TRUE.")
    }
  }

  if (!overwrite & any(file.exists(file.paths))) {
    stop("Saving data in the folder provided would overwrite existing data. To automatically overwrite existing data, set overwrite to TRUE.")
  }

  # Write each analysis view in the database to csv
  for (view.name in analysis.views) {
    df <- dplyr::tbl(conn, dbplyr::in_schema("analysis", view.name)) %>%
      dplyr::collect()
    readr::write_csv(df, file.path(dest.folder, paste0(view.name, ".csv")), na = "", append = FALSE, col_names = TRUE)
  }
}

#' Raw data dump
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param spring Optional. Spring code to filter on, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A list of dataframes containing raw spring vegetation data.
#' @export
#'
#' @importFrom magrittr %>% %<>%
#'
GetRawData <- function(conn, path.to.data, park, spring, field.season, data.source = "database") {
  spring.info <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "Spring")
  spring.visit <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "SpringVisit")
  lpi.tsect <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "LPITransect")
  tree.tsect <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "TreeCountTransect")
  veg.inv.tsect <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "VegetationInventoryTransect")
  lpi.canopy <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "LPICanopy")
  lpi.surface <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "LPISoilSurface")
  lpi.disturb <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "LPIDisturbance")
  tree.count <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "TreeCount")
  veg.inv <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "VegetationInventory")

  data.dump <- list(Spring = spring.info,
                    SpringVisit = spring.visit,
                    LPITransect = lpi.tsect,
                    TreeCountTransect = tree.tsect,
                    VegetationInventoryTransect = veg.inv.tsect,
                    LPICanopy = lpi.canopy,
                    LPISoilSurface = lpi.surface,
                    LPIDisturbance = lpi.disturb,
                    TreeCount = tree.count,
                    VegetationInventory = veg.inv)
  return(data.dump)
}

#' Read spring vegetation data from database or .csv
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param spring Optional. Spring code to filter on, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#' @param data.name The name of the analysis view or the csv file containing the data. E.g. "LPICanopy", "TreeCountTransect". See details for full list of data name options.
#'
#' @return A tibble of filtered data.
#'
#' @details \code{data.name} options are: Spring, SpringVisit, LPICanopy, LPIDisturbance, LPISoilSurface, LPITransect, TreeCount, TreeCountTransect, VegetationInventory, VegetationInventoryTransect.
#'
#' @importFrom magrittr %>% %<>%
#'
ReadAndFilterData <- function(conn, path.to.data, park, spring, field.season, data.source = "database", data.name) {
  col.spec <- list(Spring = readr::cols(TransectLength_m = readr::col_double(),
                                 PointInterceptSpacing_m = readr::col_double(),
                                 TransectBearing = readr::col_integer(),
                                 AccessDirections = readr::col_character(),
                                 Notes = readr::col_character()),
                   SpringVisit = readr::cols(FieldSeason = readr::col_character(),
                                      StartDate = readr::col_character(),
                                      EndDate = readr::col_character(),
                                      Notes = readr::col_character(),
                                      DataProcessingLevelDate = readr::col_datetime(format = ""),
                                      DataProcessingLevelNote = readr::col_character()),
                   LPITransect = readr::cols(FieldSeason = readr::col_integer(),
                                             StartDate = readr::col_character(),
                                             StartTime = readr::col_character(),
                                             EndTime = readr::col_character(),
                                             TransectNumber = readr::col_integer(),
                                             Wind = readr::col_character(),
                                             SkyCondition = readr::col_character(),
                                             Notes = readr::col_character(),
                                             DataProcessingLevelDate = readr::col_datetime(format = ""),
                                             DataProcessingLevelNote = readr::col_character()),
                   TreeCountTransect = readr::cols(FieldSeason = readr::col_integer(),
                                                   StartDate = readr::col_character(),
                                                   StartTime = readr::col_character(),
                                                   EndTime = readr::col_character(),
                                                   TransectNumber = readr::col_integer(),
                                                   Notes = readr::col_character(),
                                                   DataProcessingLevelDate = readr::col_datetime(format = ""),
                                                   DataProcessingLevelNote = readr::col_character()),
                   VegetationInventoryTransect = readr::cols(FieldSeason = readr::col_integer(),
                                                             StartDate = readr::col_character(),
                                                             StartTime = readr::col_character(),
                                                             EndTime = readr::col_character(),
                                                             TransectNumber = readr::col_integer(),
                                                             Notes = readr::col_character(),
                                                             DataProcessingLevelDate = readr::col_datetime(format = ""),
                                                             DataProcessingLevelNote = readr::col_character()),
                   LPICanopy = readr::cols(FieldSeason = readr::col_integer(),
                                           StartDate = readr::col_character(),
                                           TransectNumber = readr::col_integer(),
                                           LocationOnTape_m = readr::col_double(),
                                           WaterPresent = readr::col_character(),
                                           Canopy = readr::col_character(),
                                           Invasive = readr::col_character(),
                                           UnknownPlantCode = readr::col_character(),
                                           IsDead = readr::col_character()
                                           ),
                   LPIDisturbance = readr::cols(FieldSeason = readr::col_integer(),
                                                StartDate = readr::col_character(),
                                                TransectNumber = readr::col_integer(),
                                                LocationOnTape_m = readr::col_double(),
                                                WaterPresent = readr::col_character(),
                                                DisturbanceCode = readr::col_character(),
                                                DisturbanceDescription = readr::col_character()),
                   LPISoilSurface = readr::cols(FieldSeason = readr::col_integer(),
                                                StartDate = readr::col_character(),
                                                TransectNumber = readr::col_integer(),
                                                LocationOnTape_m = readr::col_double(),
                                                WaterPresent = readr::col_character(),
                                                SoilSurfaceCode = readr::col_character(),
                                                SoilSurfaceDescription = readr::col_character(),
                                                SoilSurfacePlantCode = readr::col_character(),
                                                SoilSurfacePlantSpecies = readr::col_character(),
                                                UnknownPlantCode = readr::col_character()),
                   TreeCount = readr::cols(FieldSeason = readr::col_integer(),
                                           StartDate = readr::col_character(),
                                           TransectNumber = readr::col_integer(),
                                           USDAPlantsCode = readr::col_character(),
                                           ScientificName = readr::col_character(),
                                           UnknownPlantCode = readr::col_character(),
                                           LiveAdultCount = readr::col_integer(),
                                           LiveJuvenileCount = readr::col_integer(),
                                           DeadAdultCount = readr::col_integer(),
                                           DeadJuvenileCount = readr::col_integer(),
                                           Notes = readr::col_character()),
                   VegetationInventory = readr::cols(FieldSeason = readr::col_integer(),
                                                     StartDate = readr::col_character(),
                                                     TransectNumber = readr::col_integer(),
                                                     USDAPlantsCode = readr::col_character(),
                                                     ScientificName = readr::col_character(),
                                                     UnknownPlantCode = readr::col_character(),
                                                     Invasive = readr::col_character(),
                                                     Notes = readr::col_character()))

  if (!(data.source %in% c("database", "local"))) {
    stop("Please choose either 'database' or 'local' for data.source")
  } else if (data.source == "database") {
    filtered.data <- dplyr::tbl(conn, dbplyr::in_schema("analysis", data.name))
  } else if (data.source == "local") {
    filtered.data <- readr::read_csv(file.path(path.to.data, paste0(data.name, ".csv")), na = "", col_types = col.spec[[data.name]])
  }

  if(!missing(park)) {
    if (!(park %in% (dplyr::select(filtered.data, Park) %>% dplyr::collect())$Park)) {
      stop("Data are not available for the park specified")
    }
    filtered.data %<>%
      dplyr::filter(Park == park)
  }

  if(!missing(spring)) {
    if (!(spring %in% (dplyr::select(filtered.data, SpringCode) %>% dplyr::collect())$SpringCode)) {
      stop("Data are not available for the spring specified")
    }
    filtered.data %<>%
      dplyr::filter(SpringCode == spring)
  }

  if(!missing(field.season) & ("FieldSeason" %in% colnames(filtered.data))) {
    if (any(!(field.season %in% (dplyr::select(filtered.data, FieldSeason) %>% dplyr::collect())$FieldSeason))) {
      stop("Data are not available for one or more of the field seasons specified")
    } else {
      filtered.data %<>%
        dplyr::filter(FieldSeason %in% field.season)
    }
  }

  filtered.data %<>%
    dplyr::collect() %>%
    dplyr::mutate_if(is.character, trimws)

  if ("FieldSeason" %in% names(filtered.data)) {
    filtered.data %<>% dplyr::mutate(FieldSeason = as.character(FieldSeason))
  }

  return(filtered.data)
}


#' Get the name of a spring from the spring code
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param spring Spring code to get the name for, e.g. "LAKE_P_BLUE0".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return The name of the spring
#' @export
#'
GetSpringName <- function(conn, path.to.data, spring.code, data.source = "database") {

  spring <- ReadAndFilterData(conn, path.to.data, spring = spring.code, data.source = data.source, data.name = "Spring")
  spring %<>% dplyr::select("SpringCode", "SpringName") %>%
    unique() %>%
    dplyr::filter(SpringCode == spring.code)

  return(spring$SpringName)
}

#' Compute sample size by spring and field season
#'
#' @param data A data frame with at least the following columns: SpringCode, FieldSeason, TransectNumber.
#'
#' @return A dataframe with columns for spring code, field season, and number of transects (i.e. sample size).
#'
#' @importFrom magrittr %>% %<>%
#'
GetSampleSizes <- function(data) {

  # Check for valid input
  required.input.cols <- c("SpringCode", "FieldSeason", "TransectNumber")

  if (!all(required.input.cols %in% names(data))) {
    stop("The dataframe provided does not have the correct columns")
  } else if (nrow(data) == 0) {
    stop("The dataframe provided contains no data")
  }

  # Get a list of plots monitored by event group
  n.transects <- data %>%
    dplyr::select(SpringCode, FieldSeason, TransectNumber) %>%
    unique() %>%
    dplyr::group_by(SpringCode, FieldSeason) %>%
    dplyr::summarise(NTransects = dplyr::n()) %>%
    dplyr::ungroup()

  return(n.transects)
}

#' List disturbance types monitored as part of the spring veg protocol.
#'
#' @return A tibble with columns Code, Label, Summary, and SortOrder
#'
#' @details This data is saved as internal data in R/sysdata.rda. If the codes are modified in the database, the data in the package will need to be manually updated.
GetDisturbanceTypes <- function() {

  return(DISTURBANCE.TYPES)

}

#' Create a vector of labels for plots faceted by field season
#'
#' @param field.seasons A vector of field season names.
#' @param sample.sizes A dataframe with columns for spring code, field season, and number of transects.
#'
#' @return A vector of facet labels with field season and sample size information.
#'
#' @importFrom magrittr %>% %<>%
#'
FacetTitle <- function(field.seasons, sample.sizes) {

  if (missing(sample.sizes)) {
    labels <- field.seasons
  } else {
    # Get the number of plots monitored in each season
    labels <- sample.sizes[which(sample.sizes$FieldSeason %in% field.seasons), ]$NTransects

    # Format labels as "YYYY (n = [# plots monitored])"
    labels <- paste0(field.seasons, " (n = ", labels, ")")
  }

  return(labels)
}

#' Apply some standard formatting to a ggplot object.
#'
#' @param p A ggplot object.
#' @param spring The spring code.
#' @param spring.name The spring name.
#' @param field.seasons Either a single field season name, or a vector of field season names.
#' @param sample.sizes Optional dataframe with columns SpringCode, FieldSeason, NTransects (i.e. sample size).
#' @param plot.title The title of the plot.
#' @param sub.title Optional custom plot subtitle.
#' @param x.lab X axis label.
#' @param y.lab Y axis label.
#' @param rotate.x.labs Boolean indicating whether to rotate x axis labels 90 degrees.
#' @param ymax Optional maximum y limit.
#' @param ymin Optional minimum y limit.
#' @param xmax Optional maximum x limit.
#' @param xmin Optional minimum x limit.
#'
#' @return A ggplot object.
#'
FormatPlot <- function(p, spring, spring.name, field.seasons, sample.sizes, plot.title, sub.title, x.lab, y.lab, rotate.x.labs, ymax, ymin, xmax, xmin) {

  # Generate a subtitle from park and event group if subtitle not provided by user
  if (missing(sub.title)) {
    # For multiple seasons of data, just use the spring name since season and sample size will go in the facet titles
    if (missing(field.seasons) || (length(field.seasons) > 1)) {
      sub.title <- spring.name
    # Otherwise, include spring name, season and sample size
    } else if (!missing(sample.sizes)) {
      n <- sample.sizes[(sample.sizes$SpringCode == spring & sample.sizes$FieldSeason == field.seasons), ]$NTransects
      sub.title <- paste0(spring.name, " (", field.seasons, ")", "\n", "n = ", n)
    } else {
      sub.title <- paste0(spring.name, " (", field.seasons, ")")
    }
  }

  # Create facets if >1 event group
  if (!missing(field.seasons) && (length(field.seasons) > 1)) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(FieldSeason), ncol = 2, labeller = ggplot2::as_labeller(function(field.seasons){FacetTitle(field.seasons, sample.sizes)}))
  }

  # Add title and subtitle if not blank
  if (plot.title != "") {
    p <- p + ggplot2::labs(title = plot.title)
  }
  if (sub.title != "") {
    p <- p + ggplot2::labs(subtitle = sub.title)
  }

  # Add x and y axis titles if not blank
  if (x.lab != "") {
    p <- p + ggplot2::xlab(x.lab)
  } else {
    p <- p + ggplot2::theme(axis.title.x = ggplot2::element_blank())
  }

  if (y.lab != "") {
    p <- p + ggplot2::ylab(y.lab)
  } else {
    p <- p + ggplot2::theme(axis.title.y = ggplot2::element_blank())
  }

  # Rotate x labels 90 degrees if rotate.x.labs is TRUE
  if (!missing(rotate.x.labs)) {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))
  }

  # Set ymin and ymax if provided
  if (!missing(ymin) && !missing(ymax)) {
    p <- p + ggplot2::expand_limits(y = c(ymin, ymax))
  } else if (!missing(ymax)) {
    p <- p + ggplot2::expand_limits(y = ymax)
  } else if (!missing(ymin)) {
    p <- p + ggplot2::expand_limits(y = ymin)
  }

  # Set xmin and xmax if provided
  if (!missing(xmin) && !missing(xmax)) {
    p <- p + ggplot2::expand_limits(x = c(xmin, xmax))
  } else if (!missing(xmax)) {
    p <- p + ggplot2::expand_limits(x = xmax)
  } else if (!missing(xmin)) {
    p <- p + ggplot2::expand_limits(x = xmin)
  }

  return(p)
}

#' See data availability by park, spring, field season, and SOP.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param spring Optional. Spring code to filter on, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#' @param data.name The name of the analysis view or the csv file containing the data. E.g. "LPICanopy", "TreeCountTransect"
#'
#' @return A tibble.
#' @export
#'
#' @details Only includes data from visits labeled 'Primary.'
#'
#' @importFrom magrittr %>% %<>%
#'
GetDataAvailability <- function(conn, path.to.data, park, spring, field.season, data.source = "database") {

  lpi <- tibble::tibble()
  inventory <- tibble::tibble()
  tree.count <- tibble::tibble()

  tryCatch({
    lpi <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, data.name = "LPITransect") %>%
      dplyr::filter(VisitType == "Primary") %>%
      dplyr::select(Park, SpringCode, SpringName, FieldSeason) %>%
      unique() %>%
      dplyr::mutate(SOP = "LPI", DataPresent = "Y")
  },
  error = function(e) {
    if (!grepl("^Data are not available", e$message)) {
      stop(e)
    }
  })

  tryCatch({
    inventory <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, data.name = "VegetationInventoryTransect") %>%
      dplyr::filter(VisitType == "Primary") %>%
      dplyr::select(Park, SpringCode, SpringName, FieldSeason) %>%
      unique() %>%
      dplyr::mutate(SOP = "Inventory", DataPresent = "Y")
  },
  error = function(e) {
    if (!grepl("^Data are not available", e$message)) {
      stop(e)
    }
  })

  tryCatch({
    tree.count <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, data.name = "TreeCountTransect") %>%
      dplyr::filter(VisitType == "Primary") %>%
      dplyr::select(Park, SpringCode, SpringName, FieldSeason) %>%
      unique() %>%
      dplyr::mutate(SOP = "TreeCount", DataPresent = "Y")
  },
  error = function(e) {
    if ((nrow(lpi) == 0 && nrow(inventory) == 0) || !grepl("^Data are not available", e$message)) {
      stop(e)
    }
  })

  data.avail <- rbind(lpi, inventory, tree.count) %>%
    dplyr::mutate(SOP = factor(SOP, levels = c("LPI", "Inventory", "TreeCount"))) %>%
    tidyr::spread(key = SOP, value = DataPresent, fill = "N", drop = FALSE) %>%
    dplyr::filter(!(LPI == "N" & Inventory == "N" & TreeCount == "N"))

  return(data.avail)
}
