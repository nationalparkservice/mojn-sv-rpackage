#' Comparison of species detected in LPI and vegetation inventory
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param spring Optional. Spring code to filter on, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns Park, SpringCode, SpringName, FieldSeason, TransectNumber, LPISpeciesCount, InventorySpeciesCount
#' @export
#'
#' @details Omits TBD and UNK species from counts. Only includes data from visits labeled 'Primary.' Currently uses LPI canopy data only (plants recorded as soil surface are not included in species counts).
#'
#' @importFrom magrittr %>% %<>%
#'
CountSpeciesDetected <- function(conn, path.to.data, park, spring, field.season, data.source = "database") {
  lpi.canopy <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "LPICanopy")
  sp.inv <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "VegetationInventory")

  lpi.canopy %<>%
    dplyr::filter(CanopyType == "Plant" & VisitType == "Primary" & !(Canopy %in% c("UNK", "TBD"))) %>%
    dplyr::select(Park, SpringCode, SpringName, FieldSeason, TransectNumber, Canopy) %>%
    unique() %>%
    dplyr::group_by(Park, SpringCode, SpringName, FieldSeason, TransectNumber) %>%
    dplyr::summarise(LPISpeciesCount = dplyr::n()) %>%
    dplyr::ungroup()

  sp.inv %<>%
    dplyr::filter(VisitType == "Primary" & !(USDAPlantsCode %in% c("UNK", "TBD"))) %>%
    dplyr::select(Park, SpringCode, SpringName, FieldSeason, TransectNumber, USDAPlantsCode) %>%
    unique() %>%
    dplyr::group_by(Park, SpringCode, SpringName, FieldSeason, TransectNumber) %>%
    dplyr::summarise(InventorySpeciesCount = dplyr::n()) %>%
    dplyr::ungroup()

  sp.count <- dplyr::full_join(lpi.canopy, sp.inv, by = c("Park", "SpringCode", "SpringName", "FieldSeason", "TransectNumber")) %>%
    dplyr::arrange(Park, SpringCode, desc(FieldSeason), TransectNumber)

  return(sp.count)
}

#' LPI canopy species count by transect and stratum
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param spring Optional. Spring code to filter on, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns Park, SpringCode, SpringName, FieldSeason, TransectNumber, Stratum, SpeciesCount
#' @export
#'
#' @details Omits TBD and UNK species from counts. Only includes data from visits labeled 'Primary.'
#'
#' @importFrom magrittr %>% %<>%
#'
CountSpeciesByStratum <- function(conn, path.to.data, park, spring, field.season, data.source = "database") {
  lpi.canopy <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "LPICanopy")

  count.by.stratum <- lpi.canopy %>%
    dplyr::mutate(Stratum = factor(Stratum, levels = c("T", "M", "B", "ND"))) %>%  # Temporarily convert Stratum to factor so that levels with zero species will be included
    dplyr::filter(CanopyType == "Plant" & VisitType == "Primary" & !(Canopy %in% c("UNK", "TBD"))) %>%
    dplyr::select(Park, SpringCode, SpringName, FieldSeason, TransectNumber, Stratum, Canopy) %>%
    unique() %>%
    dplyr::group_by(Park, SpringCode, SpringName, FieldSeason, TransectNumber, Stratum, .drop = FALSE) %>%  # Use .drop = FALSE to keep strata with zero species
    dplyr::summarise(SpeciesCount = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(Park, SpringCode, desc(FieldSeason), TransectNumber, Stratum) %>%  # Sort the data while Stratum is still a factor so it sorts T,M,B instead of alphabetically
    dplyr::mutate(Stratum = as.character(Stratum)) %>%  # Convert Stratum back from a factor to a normal character column
    dplyr::filter(!(Stratum == "ND" & SpeciesCount == 0))  # Get rid of ND (no data) counts of 0

  return(count.by.stratum)
}