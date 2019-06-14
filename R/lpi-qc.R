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

  lpi.canopy <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, data.name = "LPICanopy")

  lpi.canopy %<>%
    dplyr::select(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, TransectNumber, LocationOnTape_m) %>%
    unique() %>%
    dplyr::group_by(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, TransectNumber) %>%
    dplyr::mutate(LocationOnTape_m = paste(dplyr::setdiff(points, LocationOnTape_m), collapse = ", ")) %>%
    dplyr::filter(LocationOnTape_m != "") %>%
    unique() %>%
    dplyr::ungroup()

  return(lpi.canopy)
}

#' List occurrences of "TBD" unknown plant species in LPI data
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param spring Optional. Spring code to filter on, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return None.
#' @export
#'
#' @importFrom magrittr %>% %<>%
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     LpiQcTBDSpecies(conn, park = "LAKE")
#'     LpiQcTBDSpecies(conn, spring = "LAKE_P_BLUE0", field.season = "2019")
#'     LpiQcTBDSpecies(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
LpiQcTBDSpecies <- function(conn, path.to.data, park, spring, field.season, data.source = "database") {

  lpi.canopy <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "LPICanopy")
  lpi.surface <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "LPISoilSurface")

  lpi.canopy %<>%
    dplyr::filter(Canopy == "TBD") %>%
    dplyr::select(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, UnknownPlantCode, TransectNumber, LocationOnTape_m)

  lpi.surface %<>%
    dplyr::filter(SoilSurfacePlantCode == "TBD") %>%
    dplyr::select(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, UnknownPlantCode, TransectNumber, LocationOnTape_m)

  tbd.species <- dplyr::bind_rows(lpi.canopy, lpi.surface) %>%
    unique() %>%
    dplyr::group_by(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, TransectNumber, UnknownPlantCode) %>%
    dplyr::mutate(LocationOnTape_m = paste(LocationOnTape_m, collapse = ", ")) %>%
    dplyr::arrange(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, UnknownPlantCode, TransectNumber) %>%
    unique() %>%
    dplyr::ungroup()

  return(tbd.species)
}

#' List occurrences of "UNK" unknown plant species in LPI data
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param spring Optional. Spring code to filter on, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return None.
#' @export
#'
#' @importFrom magrittr %>% %<>%
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     LpiQcUNKSpecies(conn, park = "LAKE")
#'     LpiQcUNKSpecies(conn, spring = "LAKE_P_BLUE0", field.season = "2019")
#'     LpiQcUNKSpecies(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
LpiQcUNKSpecies <- function(conn, path.to.data, park, spring, field.season, data.source = "database") {

  lpi.canopy <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "LPICanopy")
  lpi.surface <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "LPISoilSurface")

  lpi.canopy %<>%
    dplyr::filter(Canopy == "UNK") %>%
    dplyr::select(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, UnknownPlantCode, TransectNumber, LocationOnTape_m)

  lpi.surface %<>%
    dplyr::filter(SoilSurfacePlantCode == "UNK") %>%
    dplyr::select(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, UnknownPlantCode, TransectNumber, LocationOnTape_m)

  tbd.species <- dplyr::bind_rows(lpi.canopy, lpi.surface) %>%
    unique() %>%
    dplyr::group_by(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, TransectNumber, UnknownPlantCode) %>%
    dplyr::mutate(LocationOnTape_m = paste(LocationOnTape_m, collapse = ", ")) %>%
    dplyr::arrange(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, UnknownPlantCode, TransectNumber) %>%
    unique() %>%
    dplyr::ungroup()

  return(tbd.species)
}
