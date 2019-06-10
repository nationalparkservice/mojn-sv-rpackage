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
    my.pool <- pool::dbPool(drv, ...)
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
