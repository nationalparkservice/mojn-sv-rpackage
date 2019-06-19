#' List unknown species codes that have been ID'd to more than one species
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param spring Optional. Spring code to filter on, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, UnknownPlantCode, USDAPlantsCode
#' @export
#'
#' @importFrom magrittr %>% %<>%
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     QcUnknownSpeciesID(conn, park = "LAKE")
#'     QcUnknownSpeciesID(conn, spring = "LAKE_P_BLUE0", field.season = "2019")
#'     QcUnknownSpeciesID(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
QcUnknownSpeciesID <- function(conn, path.to.data, park, spring, field.season, data.source = "database") {

  lpi.canopy <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "LPICanopy")
  lpi.surface <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "LPISoilSurface")
  tree.ct <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "TreeCount")
  sp.inv <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "VegetationInventory")

  lpi.canopy %<>%
    dplyr::select(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, UnknownPlantCode, Canopy) %>%
    dplyr::rename(USDAPlantsCode = Canopy)

  lpi.surface %<>%
    dplyr::select(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, UnknownPlantCode, SoilSurfacePlantCode) %>%
    dplyr::rename(USDAPlantsCode = SoilSurfacePlantCode)

  tree.ct %<>%
    dplyr::select(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, UnknownPlantCode, USDAPlantsCode)

  sp.inv %<>%
    dplyr::select(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, UnknownPlantCode, USDAPlantsCode)

  dup.id <- dplyr::bind_rows(lpi.canopy, lpi.surface, tree.ct, sp.inv) %>%
    unique() %>%
    dplyr::group_by(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, UnknownPlantCode) %>%
    dplyr::mutate(DupIDCount = dplyr::n()) %>%
    dplyr::filter(DupIDCount > 1 & !is.na(UnknownPlantCode)) %>%
    dplyr::mutate(USDAPlantsCode = paste(USDAPlantsCode, collapse = ", ")) %>%
    dplyr::select(-DupIDCount) %>%
    dplyr::ungroup() %>%
    unique()

  return(dup.id)
}
