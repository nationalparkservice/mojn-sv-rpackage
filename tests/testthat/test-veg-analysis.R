context("Vegetation analysis")

test_that("CountSpeciesDetected returns a dataframe of species found in LPI canopy vs species inventory", {
  expected <- tibble::tibble(Park = c("LAKE", "LAKE", "LAKE"),
                             SpringCode = c("LAKE_P_BLUE0", "LAKE_P_BLUE0", "LAKE_P_BLUE0"),
                             SpringName = c("Blue Point", "Blue Point", "Blue Point"),
                             FieldSeason = c("2019", "2019", "2019"),
                             TransectNumber = as.integer(c(0, 1, 2)),
                             LPISpeciesCount = as.integer(c(1, 1, 1)),
                             InventorySpeciesCount = as.integer(c(2, 3, 1)))

  expect_mapequal(CountSpeciesDetected(path.to.data = "./dummy-data/veg-analysis", data.source = "local"), expected)
})
