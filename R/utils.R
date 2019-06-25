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
    params <- readr::read_csv("M:/MONITORING/SLS_Veg/Data/Database/ConnectFromR/sv-database-conn.csv") %>%
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
    readr::write_csv(df, file.path(dest.folder, paste0(view.name, ".csv")), na = "NA", append = FALSE, col_names = TRUE)
  }
}

#' Read spring vegetation data from database or .csv
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param spring Optional. Spring code to filter on, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#' @param data.name The name of the analysis view or the csv file containing the data. E.g. "LPICanopy", "TreeCountTransect"
#'
#' @return A tibble of filtered data.
#'
#' @importFrom magrittr %>% %<>%
#'
ReadAndFilterData <- function(conn, path.to.data, park, spring, field.season, data.source = "database", data.name) {
  if (!(data.source %in% c("database", "local"))) {
    stop("Please choose either 'database' or 'local' for data.source")
  } else if (data.source == "database") {
    filtered.data <- dplyr::tbl(conn, dbplyr::in_schema("analysis", data.name))
  } else if (data.source == "local") {
    filtered.data <- readr::read_csv(file.path(path.to.data, paste0(data.name, ".csv")))
  }

  if(!missing(park)) {
    filtered.data %<>%
      dplyr::filter(Park == park)
  }

  if(!missing(spring)) {
    filtered.data %<>%
      dplyr::filter(SpringCode == spring)
  }

  if(!missing(field.season) && ("FieldSeason" %in% names(filtered.data))) {
    filtered.data %<>%
      dplyr::filter(FieldSeason == field.season)
  }

  filtered.data %<>%
    dplyr::collect()

  if ("FieldSeason" %in% names(filtered.data)) {
    filtered.data %<>% dplyr::mutate(FieldSeason = as.character(FieldSeason))
  }

  return(filtered.data)
}
