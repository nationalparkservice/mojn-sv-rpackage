#' List occurrences of "TBD" unknown plant species in veg species inventory data
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param spring Optional. Spring code to filter on, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, UnknownPlantCode, TransectNumber.
#' @export
#'
#' @importFrom magrittr %>% %<>%
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     SpInvQcTBDSpecies(conn, park = "LAKE")
#'     SpInvQcTBDSpecies(conn, spring = "LAKE_P_BLUE0", field.season = "2019")
#'     SpInvQcTBDSpecies(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
SpInvQcTBDSpecies <- function(conn, path.to.data, park, spring, field.season, data.source = "database") {

  tbd.species <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "VegetationInventory")

  tbd.species %<>%
    dplyr::filter(USDAPlantsCode == "TBD") %>%
    dplyr::select(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, UnknownPlantCode, TransectNumber) %>%
    dplyr::arrange(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, UnknownPlantCode, TransectNumber) %>%
    unique()

  return(tbd.species)
}

#' List occurrences of "UNK" unknown plant species in veg species inventory data
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param spring Optional. Spring code to filter on, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, UnknownPlantCode, TransectNumber.
#' @export
#'
#' @importFrom magrittr %>% %<>%
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     SpInvQcUNKSpecies(conn, park = "LAKE")
#'     SpInvQcUNKSpecies(conn, spring = "LAKE_P_BLUE0", field.season = "2019")
#'     SpInvQcUNKSpecies(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
SpInvQcUNKSpecies <- function(conn, path.to.data, park, spring, field.season, data.source = "database") {

  unk.species <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "VegetationInventory")

  unk.species %<>%
    dplyr::filter(USDAPlantsCode == "UNK") %>%
    dplyr::select(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, UnknownPlantCode, TransectNumber) %>%
    dplyr::arrange(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, UnknownPlantCode, TransectNumber) %>%
    unique()

  return(unk.species)
}

#' List occurrences of duplicate plant species in species inventory data
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param spring Optional. Spring code to filter on, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, UnknownPlantCode, TransectNumber, USDAPlantsCode
#' @export
#'
#' @importFrom magrittr %>% %<>%
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     SpInvQcDuplicateSpecies(conn, park = "LAKE")
#'     SpInvQcDuplicateSpecies(conn, spring = "LAKE_P_BLUE0", field.season = "2019")
#'     SpInvQcDuplicateSpecies(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
SpInvQcDuplicateSpecies <- function(conn, path.to.data, park, spring, field.season, data.source = "database") {

  sp.inv <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "VegetationInventory")

  dup.species <- sp.inv %>%
    dplyr::select(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, UnknownPlantCode, TransectNumber, USDAPlantsCode) %>%
    dplyr::filter(!(USDAPlantsCode %in% c("UNK", "TBD"))) %>%
    dplyr::group_by(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, TransectNumber, USDAPlantsCode) %>%
    dplyr::mutate(UnknownPlantCode = paste(UnknownPlantCode, collapse = ", ")) %>%
    dplyr::mutate(DupCount = dplyr::n()) %>%
    dplyr::filter(DupCount > 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-DupCount) %>%
    unique()

  return(dup.species)
}
