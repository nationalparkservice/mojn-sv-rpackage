#' List occurrences of "TBD" unknown plant species in tree count data
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
#'     TreeCtQcTBDSpecies(conn, park = "LAKE")
#'     TreeCtQcTBDSpecies(conn, spring = "LAKE_P_BLUE0", field.season = "2019")
#'     TreeCtQcTBDSpecies(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
TreeCtQcTBDSpecies <- function(conn, path.to.data, park, spring, field.season, data.source = "database") {

  tbd.species <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "TreeCount")

  tbd.species %<>%
    dplyr::filter(USDAPlantsCode == "TBD") %>%
    dplyr::select(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, UnknownPlantCode, TransectNumber) %>%
    dplyr::arrange(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, UnknownPlantCode, TransectNumber) %>%
    unique()

  return(tbd.species)
}

#' List occurrences of "UNK" unknown plant species in tree count data
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
#'     TreeCtQcUNKSpecies(conn, park = "LAKE")
#'     TreeCtQcUNKSpecies(conn, spring = "LAKE_P_BLUE0", field.season = "2019")
#'     TreeCtQcUNKSpecies(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
TreeCtQcUNKSpecies <- function(conn, path.to.data, park, spring, field.season, data.source = "database") {

  unk.species <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "TreeCount")

  unk.species %<>%
    dplyr::filter(USDAPlantsCode == "UNK") %>%
    dplyr::select(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, UnknownPlantCode, TransectNumber) %>%
    dplyr::arrange(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, UnknownPlantCode, TransectNumber) %>%
    unique()

  return(unk.species)
}

#' List occurrences of duplicate plant species in tree count data
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
#'     TreeCtQcDuplicateSpecies(conn, park = "LAKE")
#'     TreeCtQcDuplicateSpecies(conn, spring = "LAKE_P_BLUE0", field.season = "2019")
#'     TreeCtQcDuplicateSpecies(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
TreeCtQcDuplicateSpecies <- function(conn, path.to.data, park, spring, field.season, data.source = "database") {

  tree.ct <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "TreeCount")

  dup.species <- tree.ct %>%
    dplyr::select(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, UnknownPlantCode, TransectNumber, USDAPlantsCode) %>%
    dplyr::group_by(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, TransectNumber, USDAPlantsCode) %>%
    dplyr::mutate(UnknownPlantCode = paste(UnknownPlantCode, collapse = ", ")) %>%
    dplyr::mutate(DupCount = dplyr::n()) %>%
    dplyr::filter(DupCount > 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-DupCount) %>%
    unique()

  return(dup.species)
}
