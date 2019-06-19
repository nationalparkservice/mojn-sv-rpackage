context("LPI quality control")

test_that("LpiQcMissingPoints returns a blank dataframe when all transects have points 0-15", {
  expect_equal(nrow(LpiQcMissingPoints(path.to.data = "./dummy-data/ok", data.source = "local", points = c(0.5, 1, 1.5))), 0)
})

test_that("LpiQcMissingPoints returns a dataframe of missing points when points are absent from some transects", {
  expected.missing <- tibble::tibble(Park = c("LAKE", "LAKE"),
                                     SpringCode = c("LAKE_P_BLUE0", "LAKE_P_HOR0042"),
                                     SpringName = c("Blue Point", "Horsethief Canyon"),
                                     VisitType = c("Primary", "Primary"),
                                     FieldSeason = c("2019", "2019"),
                                     StartDate = c("4/4/2019", "4/22/2019"),
                                     TransectNumber = c(0, 1),
                                     LocationOnTape_m = c("1, 1.5", "0.5"))

  expect_setequal(LpiQcMissingPoints(path.to.data = "./dummy-data/bad", data.source = "local", points = c(0.5, 1, 1.5)), expected.missing)
})

test_that("LpiQcMissingPoints does not fail when using data from the database", {
  conn <- OpenDatabaseConnection()

  expect_error(LpiQcMissingPoints(conn, field.season = 2019), NA)

  CloseDatabaseConnection(conn)
})

test_that("LpiQcTBDSpecies returns a dataframe of unknown species codes that are marked as TBD", {
  expected.tbd <- tibble::tibble(Park = c("LAKE", "LAKE", "LAKE"),
                                 SpringCode = c("LAKE_P_BLUE0", "LAKE_P_HOR0042", "LAKE_P_HOR0042"),
                                 SpringName = c("Blue Point", "Horsethief Canyon", "Horsethief Canyon"),
                                 VisitType = c("Primary", "Primary", "Primary"),
                                 FieldSeason = c("2019", "2019", "2019"),
                                 StartDate = c("4/4/2019", "4/22/2019", "4/22/2019"),
                                 UnknownPlantCode = c("UNK4", "UNK2", "UNK5"),
                                 TransectNumber = c(2, 0, 2),
                                 LocationOnTape_m = c("1, 1.5", "1", "1, 1.5"))
  expect_setequal(LpiQcTBDSpecies(path.to.data = "./dummy-data/ok", data.source = "local"), expected.tbd)
})

test_that("LpiQcTBDSpecies does not fail when using data from the database", {
  conn <- OpenDatabaseConnection()

  expect_error(LpiQcTBDSpecies(conn, field.season = 2019), NA)

  CloseDatabaseConnection(conn)
})

test_that("LpiQcDuplicateSpecies returns a dataframe of points with unknowns that resolved to duplicate species", {
  expected <- tibble::tibble(Park = c("LAKE"),
                                 SpringCode = c("LAKE_P_BLUE0"),
                                 SpringName = c("Blue Point"),
                                 VisitType = c("Primary"),
                                 FieldSeason = c("2019"),
                                 StartDate = c("4/4/2019"),
                                 UnknownPlantCode = c("NA, UNK4"),
                                 TransectNumber = c(2),
                                 LocationOnTape_m = c(1),
                                 Stratum = "B",
                                 Canopy = "PLANT1")
  expect_mapequal(LpiQcDuplicateSpecies(path.to.data = "./dummy-data/bad", data.source = "local"), expected)
})

test_that("LpiQcDuplicateSpecies returns an empty dataframe when no duplicate species are present", {
  expect_equal(nrow(LpiQcDuplicateSpecies(path.to.data = "./dummy-data/ok", data.source = "local")), 0)
})
