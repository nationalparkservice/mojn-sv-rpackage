#' Comparison of LPI and vegetation inventory species richness
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

  lpi.canopy <- tibble::tibble()
  sp.inv <- tibble::tibble()

  tryCatch({
    lpi.canopy <- CountLPISpeciesDetected(conn, path.to.data, park, spring, field.season, data.source)
  },
  error = function(e) {
    if (!grepl("^Data are not available", e$message)) {
      stop(e)
    }
  })

  tryCatch({
    sp.inv <- CountInvSpeciesDetected(conn, path.to.data, park, spring, field.season, data.source)
  },
  error = function(e) {
    if ((nrow(lpi.canopy) == 0) || !grepl("^Data are not available", e$message)) {
      stop(e)
    }
  })

  if (nrow(lpi.canopy) > 0 && nrow(sp.inv) > 0) {
    sp.count <- dplyr::full_join(lpi.canopy, sp.inv, by = c("Park", "SpringCode", "SpringName", "FieldSeason", "TransectNumber")) %>%
      dplyr::arrange(Park, SpringCode, desc(FieldSeason), TransectNumber)
  } else if (nrow(lpi.canopy) > 0) {
    sp.count <- lpi.canopy %>%
      dplyr::mutate(InventorySpeciesCount = NA)
  } else {
    sp.count <- sp.inv %>%
      dplyr::mutate(LPISpeciesCount = NA)
  }

  return(sp.count)
}

#' LPI species richness
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param spring Optional. Spring code to filter on, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns Park, SpringCode, SpringName, FieldSeason, TransectNumber, LPISpeciesCount
#' @export
#'
#' @details Omits TBD and UNK species from counts. Only includes data from visits labeled 'Primary.' Currently uses LPI canopy data only (plants recorded as soil surface are not included in species counts).
#'
#' @importFrom magrittr %>% %<>%
#'
CountLPISpeciesDetected <- function(conn, path.to.data, park, spring, field.season, data.source = "database") {

  lpi.canopy <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "LPICanopy")
  lpi.richness <- lpi.canopy %>%
    dplyr::filter(CanopyType == "Plant" & VisitType == "Primary" & !(Canopy %in% c("UNK", "TBD"))) %>%
    dplyr::select(Park, SpringCode, SpringName, FieldSeason, TransectNumber, Canopy) %>%
    unique() %>%
    dplyr::group_by(Park, SpringCode, SpringName, FieldSeason, TransectNumber) %>%
    dplyr::summarise(LPISpeciesCount = dplyr::n()) %>%
    dplyr::ungroup()

  return(lpi.richness)
}

#' Inventory species richness
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param spring Optional. Spring code to filter on, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns Park, SpringCode, SpringName, FieldSeason, TransectNumber, InventorySpeciesCount
#' @export
#'
#' @details Omits TBD and UNK species from counts. Only includes data from visits labeled 'Primary.' Currently uses LPI canopy data only (plants recorded as soil surface are not included in species counts).
#'
#' @importFrom magrittr %>% %<>%
#'
CountInvSpeciesDetected <- function(conn, path.to.data, park, spring, field.season, data.source = "database") {

  sp.inv <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "VegetationInventory")
  inv.richness <- sp.inv %>%
    dplyr::filter(VisitType == "Primary" & !(USDAPlantsCode %in% c("UNK", "TBD"))) %>%
    dplyr::select(Park, SpringCode, SpringName, FieldSeason, TransectNumber, USDAPlantsCode) %>%
    unique() %>%
    dplyr::group_by(Park, SpringCode, SpringName, FieldSeason, TransectNumber) %>%
    dplyr::summarise(InventorySpeciesCount = dplyr::n()) %>%
    dplyr::ungroup()

  return(inv.richness)
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

#' LPI canopy cover by transect
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param spring Optional. Spring code to filter on, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns Park, SpringCode, SpringName, FieldSeason, TransectNumber, CanopyPercentCover
#' @export
#'
#' @details Only includes data from visits labeled 'Primary.' Counts both plant canopy and other canopy (e.g. litter) as cover. Does not take into account the number of canopy layers, only presence/absence of canopy at a point. I.e., a point with a single plant counts the same as a point with multiple plants in multiple strata. Percent cover is calculated only from points at which data were collected - if only three points were recorded for a transect and one of them had canopy cover, that transect would have 33.3% cover.
#'
#' @importFrom magrittr %>% %<>%
#'
CanopyPercentCover <- function(conn, path.to.data, park, spring, field.season, data.source = "database") {
  lpi.canopy <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "LPICanopy")

  pct.cover <- lpi.canopy %>%
    dplyr::filter(VisitType == "Primary") %>%
    dplyr::select(Park, SpringCode, SpringName, FieldSeason, TransectNumber, LocationOnTape_m, CanopyType) %>%
    dplyr::mutate(HasCanopy = (CanopyType != "None")) %>%  # Create a column indicating presence/absence of canopy
    dplyr::select(-CanopyType) %>%  # We don't care about the type of canopy anymore
    unique() %>%  # Now we have one row per transect point indicating whether or not cover is present
    dplyr::select(-LocationOnTape_m) %>%
    dplyr::group_by(Park, SpringCode, SpringName, FieldSeason, TransectNumber) %>%
    dplyr::summarise(CanopyCover_percent = 100 * mean(HasCanopy)) %>%  # Since HasCanopy is logical (true/false), the mean * 100 should give us pct cover
    dplyr::mutate(CanopyCover_percent = round(CanopyCover_percent, 1)) %>%
    dplyr::ungroup()

  return(pct.cover)
}

#' Species Accumulation Curve by spring
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param spring Spring code to filter on, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns Park, SpringCode, SpringName, FieldSeason, Sites, Richness, StDev
#' @export
#'
#' @details Only includes data from visits labeled 'Primary.' Does not include UNKS but does include To Be Determined unknown species.
#'
#' @importFrom magrittr %>% %<>%
#'
CalculateSpeciesAccumulation<- function(conn, path.to.data, spring, field.season, data.source = "database") {
  veg.inv <- ReadAndFilterData(conn, path.to.data, spring = spring, field.season = field.season, data.source = data.source, data.name = "VegetationInventory", )

  sp.acc <- veg.inv %>%
    dplyr::filter(VisitType == "Primary" & USDAPlantsCode != "UNK") %>%
    dplyr::select(TransectNumber, USDAPlantsCode, UnknownPlantCode)
  # Replace TBD plant codes with the unknown code (i.e. A, B, C, etc.)
  sp.acc[which(sp.acc$USDAPlantsCode == "TBD"), ]$USDAPlantsCode <- sp.acc[which(sp.acc$USDAPlantsCode == "TBD"), ]$UnknownPlantCode

  sp.acc %<>% dplyr::select(-UnknownPlantCode) %>%
    dplyr::mutate(Present = 1) %>%
    tidyr::spread(USDAPlantsCode, Present, fill = 0) %>%
    vegan::specaccum ("rarefaction", permutations=500)

  sp.acc <- tibble::tibble(Park = unique(veg.inv$Park),
                 SpringCode = spring,
                 SpringName = unique(veg.inv$SpringName),
                 FieldSeason = field.season,
                 Transects = sp.acc$sites,
                 StDev = sp.acc$sd,
                 Richness = sp.acc$richness)

  return(sp.acc)

}

#' Number of transects with and without trees, by spring and field season
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param spring Optional. Spring code to filter on, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns Park, SpringCode, SpringName, FieldSeason, NTransectsWithTrees, NTransectsNoTrees
#' @export
#'
#' @details Only includes data from visits labeled 'Primary.'
#'
#' @importFrom magrittr %>% %<>%
#'
TreePresenceAbsence <- function(conn, path.to.data, park, spring, field.season, data.source = "database") {
  trees <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "TreeCount")

  tree.pa <- trees %>%
    dplyr::filter(VisitType == "Primary") %>%
    dplyr::select(Park, SpringCode, SpringName, FieldSeason, TransectNumber, USDAPlantsCode) %>%
    dplyr::mutate(HasTrees = !is.na(USDAPlantsCode), NoTrees = is.na(USDAPlantsCode)) %>%
    dplyr::select(-USDAPlantsCode) %>%
    unique() %>%
    dplyr::select(-TransectNumber) %>%
    dplyr::group_by(Park, SpringCode, SpringName, FieldSeason) %>%
    dplyr::summarise(NTransectsWithTrees = sum(HasTrees), NTransectsNoTrees = sum(NoTrees)) %>%
    dplyr::ungroup()

  return(tree.pa)
}

#' Water percent cover by transect
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param spring Optional. Spring code to filter on, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns Park, SpringCode, SpringName, FieldSeason, TransectNumber, WaterPercentCover
#' @export
#'
#' @details Only includes data from visits labeled 'Primary.' Points with water recorded as "NA" and "ND" are omitted. Omits Blue Point transects 1 - 3 in 2019 since water presence/absence was not yet being recorded consistently.
#'
#' @importFrom magrittr %>% %<>%
#'
WaterPercentCover <- function(conn, path.to.data, park, spring, field.season, data.source = "database") {
  lpi.canopy <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "LPICanopy")

  pct.cover <- lpi.canopy %>%
    dplyr::filter(VisitType == "Primary" & !WaterPresent %in% c("NA", "ND")) %>%
    dplyr::filter(!(SpringCode == "LAKE_P_BLUE0" & TransectNumber %in% c(1:3) & FieldSeason == "2019")) %>%  # Omit Blue Point transects 1 - 3 in 2019 since water presence/absence was not recorded consistently here.
    dplyr::select(Park, SpringCode, SpringName, FieldSeason, TransectNumber, LocationOnTape_m, WaterPresent) %>%
    dplyr::mutate(HasWater = (WaterPresent == "Y")) %>%  # Create a column indicating presence/absence of water
    dplyr::select(-WaterPresent) %>%  # We don't care about the type of canopy anymore
    unique() %>%  # Now we have one row per transect point indicating whether or not water is present
    dplyr::select(-LocationOnTape_m) %>%
    dplyr::group_by(Park, SpringCode, SpringName, FieldSeason, TransectNumber) %>%
    dplyr::summarise(WaterCover_percent = 100 * mean(HasWater)) %>%  # Since HasCanopy is logical (true/false), the mean * 100 should give us pct cover
    dplyr::mutate(WaterCover_percent = round(WaterCover_percent, 1)) %>%
    dplyr::ungroup()

  return(pct.cover)
}
