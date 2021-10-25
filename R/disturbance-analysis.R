#' Disturbance cover by disturbance type and transect
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param spring Optional. Spring code to filter on, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param drop.zeroes Should percent covers of 0 be included in the dataset? Defaults to FALSE.
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns Park, SpringCode, SpringName, FieldSeason, TransectNumber, CanopyPercentCover
#' @export
#'
#' @details Only includes data from visits labeled 'Primary.' Percent cover is calculated only from points at which data were collected - if only three points were recorded for a transect and one of them had bike disturbance, the disturbance type BK would have 33.3% cover for that transect.
#'
#' @importFrom magrittr %>% %<>%
#'
DisturbancePercentCover <- function(conn, path.to.data, park, spring, field.season, drop.zeroes = FALSE, data.source = "database") {
  disturbance <- ReadAndFilterData(conn, path.to.data, park, spring, field.season, data.source, "LPIDisturbance")
  dist.types <- GetDisturbanceTypes()

  pct.cover <- disturbance %>%
    dplyr::filter((VisitType == "Primary") & (is.na(DisturbanceCode) | DisturbanceCode != "NA")) %>%
    dplyr::select(Park, SpringCode, SpringName, FieldSeason, TransectNumber, LocationOnTape_m, DisturbanceCode) %>%
    dplyr::mutate(HasDisturbance = !is.na(DisturbanceCode), DisturbanceCode = factor(DisturbanceCode, levels = dist.types$Code)) %>%
    tidyr::spread(DisturbanceCode, HasDisturbance, fill = FALSE, drop = FALSE) %>%
    dplyr::select(-`<NA>`, -LocationOnTape_m) %>%
    dplyr::group_by(Park, SpringCode, SpringName, FieldSeason, TransectNumber)

  # Get a list of disturbance columns
  dist.cols <- setdiff(names(pct.cover), c("Park", "SpringCode", "SpringName", "FieldSeason", "TransectNumber"))

  pct.cover %<>%
    dplyr::summarise_at(dplyr::vars(dist.cols), ~100*mean(.)) %>%
    dplyr::mutate_at(dplyr::vars(dist.cols), ~round(., 1)) %>%
    dplyr::ungroup() %>%
    tidyr::gather("DisturbanceCode", "DisturbanceCover_percent", dist.cols)

  if (drop.zeroes) {
    pct.cover %<>%
      dplyr::filter(DisturbanceCover_percent > 0)
  }

  return(pct.cover)
}
