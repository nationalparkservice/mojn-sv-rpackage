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

#' Report DPL by transect for all SOPs
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param spring Optional. Spring code to filter on, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, TransectNumber, DataProcessingLevel, DataProcessingLevelDate, DataProcessingLevelNote
#' @export
#'
#' @importFrom magrittr %>% %<>%
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     QcReportDPL(conn, park = "LAKE")
#'     QcReportDPL(conn, spring = "LAKE_P_BLUE0", field.season = "2019")
#'     QcReportDPL(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
QcReportDPL <- function(conn, path.to.data, park, spring, field.season, data.source = "database") {
  lpi <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "LPITransect")
  tree <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "TreeCountTransect")
  sp.inv <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "VegetationInventoryTransect")

  lpi %<>%
    dplyr::select(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, TransectNumber, DataProcessingLevel, DataProcessingLevelDate, DataProcessingLevelNote) %>%
    dplyr::mutate(SOP = "LPI")

  tree %<>%
    dplyr::select(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, TransectNumber, DataProcessingLevel, DataProcessingLevelDate, DataProcessingLevelNote) %>%
    dplyr::mutate(SOP = "TreeCount")

  sp.inv %<>%
    dplyr::select(Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, TransectNumber, DataProcessingLevel, DataProcessingLevelDate, DataProcessingLevelNote) %>%
    dplyr::mutate(SOP = "VegetationInventory")

  dpl <- dplyr::bind_rows(lpi, tree, sp.inv) %>%
    unique()

  return(dpl)
}

#' List rows of data where one or more columns is missing data that should have been collected
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param spring Optional. Spring code to filter on, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A list of tibbles, each corresponding to a data analysis view/csv
#' @export
#'
#' @importFrom magrittr %>% %<>%
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     QcListNoData(conn, park = "LAKE")
#'     QcListNoData(conn, spring = "LAKE_P_BLUE0", field.season = "2019")
#'     QcListNoData(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
QcListNoData <- function(conn, path.to.data, park, spring, field.season, data.source = "database") {
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

  nodata.regex <- "(^\\[?(N|n)o\\s?(D|d)ata\\]?$)|^(N|n)(D|d)$"

  spring.info <- dplyr::bind_rows(dplyr::filter_all(spring.info, dplyr::any_vars(grepl(nodata.regex, .))),
                                  dplyr::filter_at(spring.info, dplyr::vars(-Notes), dplyr::any_vars(is.na(.)))) %>%
    unique()
  spring.visit %<>% dplyr::filter_all(dplyr::any_vars(grepl(nodata.regex, .)))
  lpi.tsect %<>% dplyr::filter_all(dplyr::any_vars(grepl(nodata.regex, .)))
  tree.tsect %<>% dplyr::filter_all(dplyr::any_vars(grepl(nodata.regex, .)))
  veg.inv.tsect %<>% dplyr::filter_all(dplyr::any_vars(grepl(nodata.regex, .)))
  lpi.canopy %<>% dplyr::filter_all(dplyr::any_vars(grepl(nodata.regex, .)))
  lpi.surface %<>% dplyr::filter_all(dplyr::any_vars(grepl(nodata.regex, .)))
  lpi.disturb %<>% dplyr::filter_all(dplyr::any_vars(grepl(nodata.regex, .)))
  tree.count %<>% dplyr::filter_all(dplyr::any_vars(grepl(nodata.regex, .)))
  veg.inv %<>% dplyr::filter_all(dplyr::any_vars(grepl(nodata.regex, .)))

  no.data <- list(Spring = spring.info,
                  SpringVisit = spring.visit,
                  LPITransect = lpi.tsect,
                  TreeCountTransect = tree.tsect,
                  VegetationInventoryTransect = veg.inv.tsect,
                  LPICanopy = lpi.canopy,
                  LPISoilSurface = lpi.surface,
                  LPIDisturbance = lpi.disturb,
                  TreeCount = tree.count,
                  VegetationInventory = veg.inv)

  return(no.data)
}
