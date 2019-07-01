context("LPI quality control")

test_that("LpiQcMissingPoints returns a blank dataframe when all transects have points 0-15", {
  expect_equal(nrow(LpiQcMissingPoints(path.to.data = "./dummy-data/ok", data.source = "local", points = c(0.5, 1, 1.5))), 0)
})

test_that("LpiQcMissingPoints returns a dataframe of missing points when points are absent from some transects", {
  expected.missing <- tibble::tibble(Park = c("LAKE", "LAKE"),
                                     SpringCode = c("LAKE_P_BLUE0", "LAKE_P_BLUE0"),
                                     SpringName = c("Blue Point", "Blue Point"),
                                     VisitType = c("Primary", "Primary"),
                                     FieldSeason = c("2019", "2019"),
                                     StartDate = c("4/4/2019", "4/4/2019"),
                                     TransectNumber = as.integer(c(0, 1)),
                                     LocationOnTape_m = c("1, 1.5", "0.5"))

  expect_mapequal(LpiQcMissingPoints(path.to.data = "./dummy-data/lpi-qc/bad", data.source = "local", points = c(0.5, 1, 1.5)), expected.missing)
})

test_that("LpiQcMissingPoints does not fail when using data from the database", {
  conn <- OpenDatabaseConnection()

  expect_error(LpiQcMissingPoints(conn, field.season = 2019), NA)

  CloseDatabaseConnection(conn)
})

test_that("LpiQcTBDSpecies returns a dataframe of unknown species codes that are marked as TBD", {
  expected.tbd <- tibble::tibble(Park = c("LAKE", "LAKE"),
                                 SpringCode = c("LAKE_P_BLUE0", "LAKE_P_BLUE0"),
                                 SpringName = c("Blue Point", "Blue Point"),
                                 VisitType = c("Primary", "Primary"),
                                 FieldSeason = c("2019", "2019"),
                                 StartDate = c("4/4/2019", "4/4/2019"),
                                 UnknownPlantCode = c("A", "Q"),
                                 TransectNumber = as.integer(c(0, 0)),
                                 LocationOnTape_m = c("1, 1.5", "0.5"))
  expect_mapequal(LpiQcTBDSpecies(path.to.data = "./dummy-data/lpi-qc/ok", data.source = "local"), expected.tbd)
})

test_that("LpiQcTBDSpecies does not fail when using data from the database", {
  conn <- OpenDatabaseConnection()

  expect_error(LpiQcTBDSpecies(conn, field.season = 2019), NA)

  CloseDatabaseConnection(conn)
})

test_that("LpiQcDuplicateSpecies returns a dataframe of points with unknowns that resolved to duplicate species", {
  expected <- tibble::tibble(Park = c("LAKE"),
                                 SpringCode = "LAKE_P_BLUE0",
                                 SpringName = "Blue Point",
                                 VisitType = "Primary",
                                 FieldSeason = "2019",
                                 StartDate = "4/4/2019",
                                 UnknownPlantCode = "A, NA",
                                 TransectNumber = as.integer(1),
                                 LocationOnTape_m = 1,
                                 Stratum = "B",
                                 Canopy = "TYAN")
  expect_mapequal(LpiQcDuplicateSpecies(path.to.data = "./dummy-data/lpi-qc/bad", data.source = "local"), expected)
})

test_that("LpiQcDuplicateSpecies returns an empty dataframe when no duplicate species are present", {
  expect_equal(nrow(LpiQcDuplicateSpecies(path.to.data = "./dummy-data/lpi-qc/ok", data.source = "local")), 0)
})
