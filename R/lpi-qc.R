#' Check for missing LPI points
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param spring Optional. Spring code to filter on, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#' @param points Vector representing the expected points on a transect. Defaults to \code{seq(0.5, 15, 0.5)} (a 15m transect sampled every 0.5m and starting at 0.5m).
#'
#' @return None.
#' @export
#'
#' @importFrom magrittr %>% %<>%
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     LpiQcMissingPoints(conn, park = "LAKE")
#'     LpiQcMissingPoints(conn, spring = "LAKE_P_BLUE0", field.season = "2019")
#'     LpiQcMissingPoints(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
LpiQcMissingPoints <- function(conn, path.to.data, park, spring, field.season, data.source = "database", points = seq(0.5, 15, 0.5)) {

  if (!(data.source %in% c("database", "local"))) {
    stop("Please choose either 'database' or 'local' for data.source")
  } else if (data.source == "database") {
    lpi.canopy <- dplyr::tbl(conn, dbplyr::in_schema("analysis", "LPICanopy"))
  } else if (data.source == "local") {
    lpi.canopy <- readr::read_csv(file.path(path.to.data, "LPICanopy.csv"))
  }

  lpi.canopy %<>%
    dplyr::mutate(FieldSeason = as.character(FieldSeason))

  if(!missing(park)) {
    lpi.canopy %<>%
      dplyr::filter(Park == park)
  }

  if(!missing(spring)) {
    lpi.canopy %<>%
      dplyr::filter(SpringCode == spring)
  }

  if(!missing(field.season)) {
    lpi.canopy %<>%
      dplyr::filter(FieldSeason == field.season)
  }

  lpi.canopy %<>%
    dplyr::collect() %>%
    dplyr::select(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, TransectNumber, LocationOnTape_m) %>%
    unique() %>%
    dplyr::group_by(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, TransectNumber) %>%
    dplyr::mutate(LocationOnTape_m = paste(dplyr::setdiff(points, LocationOnTape_m), collapse = ", ")) %>%
    dplyr::filter(LocationOnTape_m != "") %>%
    unique() %>%
    dplyr::ungroup()

  return(lpi.canopy)
}
