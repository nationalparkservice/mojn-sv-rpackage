context("Vegetation analysis")

test_that("CountSpeciesDetected returns a dataframe of species found in LPI canopy vs species inventory", {
  expected <- tibble::tibble(Park = c("LAKE", "LAKE", "LAKE"),
                             SpringCode = c("LAKE_P_BLUE0", "LAKE_P_BLUE0", "LAKE_P_BLUE0"),
                             SpringName = c("Blue Point", "Blue Point", "Blue Point"),
                             FieldSeason = c("2019", "2019", "2019"),
                             TransectNumber = as.integer(c(0, 1, 2)),
                             LPISpeciesCount = as.integer(c(1, 1, 2)),
                             InventorySpeciesCount = as.integer(c(2, 3, 1)))

  expect_mapequal(CountSpeciesDetected(path.to.data = "./dummy-data/veg-analysis", data.source = "local"), expected)
})

test_that("CountSpeciesByStratum returns a dataframe of species counts by stratum", {
  expected <- tibble::tibble(Park = c("LAKE", "LAKE", "LAKE", "LAKE", "LAKE", "LAKE", "LAKE", "LAKE", "LAKE"),
                             SpringCode = c("LAKE_P_BLUE0", "LAKE_P_BLUE0", "LAKE_P_BLUE0", "LAKE_P_BLUE0", "LAKE_P_BLUE0", "LAKE_P_BLUE0", "LAKE_P_BLUE0", "LAKE_P_BLUE0", "LAKE_P_BLUE0"),
                             SpringName = c("Blue Point", "Blue Point", "Blue Point", "Blue Point", "Blue Point", "Blue Point", "Blue Point", "Blue Point", "Blue Point"),
                             FieldSeason = c("2019", "2019", "2019", "2019", "2019", "2019", "2019", "2019", "2019"),
                             TransectNumber = as.integer(c(0, 0, 0, 1, 1, 1, 2, 2, 2)),
                             Stratum = c("T", "M", "B", "T", "M", "B", "T", "M", "B"),
                             SpeciesCount = as.integer(c(0, 1, 1, 0, 1, 1, 0, 0, 2)))

  expect_mapequal(CountSpeciesByStratum(path.to.data = "./dummy-data/veg-analysis", data.source = "local"), expected)
})

test_that("CanopyPercentCover correctly computes the canopy cover for each transect", {
  expected <- tibble::tibble(Park = c("LAKE", "LAKE", "LAKE"),
                             SpringCode = c("LAKE_P_BLUE0", "LAKE_P_BLUE0", "LAKE_P_BLUE0"),
                             SpringName = c("Blue Point", "Blue Point", "Blue Point"),
                             FieldSeason = c("2019", "2019", "2019"),
                             TransectNumber = as.integer(c(0, 1, 2)),
                             CanopyCover_percent = c(66.7, 66.7, 66.7))

  expect_mapequal(CanopyPercentCover(path.to.data = "./dummy-data/veg-analysis", data.source = "local"), expected)
})

test_that("CalculateSpeciesAccumulation correctly creates a table of speccaccum output for use in figure", {
  expected <- tibble::tibble(Park = c("LAKE", "LAKE", "LAKE"),
                             SpringCode = c("LAKE_P_BLUE0", "LAKE_P_BLUE0", "LAKE_P_BLUE0"),
                             SpringName = c("Blue Point", "Blue Point", "Blue Point"),
                             FieldSeason = c("2019", "2019", "2019"),
                             Transects = c(1.09, 1.91, 3),
                             StDev = c(0.58, 0.74, 0),
                             Richness = c(3.48, 5.30, 7))
  actual <- CalculateSpeciesAccumulation(path.to.data = "./dummy-data/veg-analysis", data.source = "local", spring = "LAKE_P_BLUE0", field.season = "2019")
  actual %<>% dplyr::mutate(Transects = round(Transects, 2), StDev = round(StDev, 2), Richness = round(Richness, 2))
  expect_mapequal(actual, expected)
})

test_that("TreePresenceAbsence correctly reports the number of transects with and without trees", {
  expected <- tibble::tibble(Park = "LAKE",
                             SpringCode = c("LAKE_P_HOR0042", "LAKE_P_ROGE0"),
                             SpringName = c("Horsethief Canyon", "Rogers"),
                             FieldSeason = "2019",
                             NTransectsWithTrees = as.integer(c(1, 3)),
                             NTransectsNoTrees = as.integer(c(2, 0)))
  expect_mapequal(TreePresenceAbsence(path.to.data = "./dummy-data/veg-analysis", data.source = "local"), expected)
  expect_mapequal(TreePresenceAbsence(path.to.data = "./dummy-data/veg-analysis", data.source = "local", spring = "LAKE_P_ROGE0"), expected[2,])
})
