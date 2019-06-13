context("LPI quality control")

test_that("LpiQcMissingPoints returns a blank dataframe when all transects have points 0-15", {
  expect_equal(nrow(LpiQcMissingPoints(path.to.data = "./dummy-data/ok", data.source = "local", points = c(0.5, 1, 1.5))), 0)
  expect_equal(nrow(LpiQcMissingPoints(park = "MOJA", path.to.data = "./dummy-data/ok", data.source = "local", points = c(0.5, 1, 1.5))), 0)
  expect_equal(nrow(LpiQcMissingPoints(field.season = "2019", path.to.data = "./dummy-data/ok", data.source = "local", points = c(0.5, 1, 1.5))), 0)
  expect_equal(nrow(LpiQcMissingPoints(spring = "LAKE_P_BLUE0", path.to.data = "./dummy-data/ok", data.source = "local", points = c(0.5, 1, 1.5))), 0)
})

test_that("LpiQcMissingPoints returns a dataframe of missing points when points are absent from some transects", {
  expected.missing <- tibble::tibble(Park = c("LAKE", "LAKE", "PARA", "MOJA"),
                                     SpringCode = c("LAKE_P_BLUE0", "LAKE_P_HOR0042", "PARA_P_PAKO0", "MOJA_P_MCSP0"),
                                     SpringName = c("Blue Point", "Horsethief Canyon", "Pakoon", "MC Spring"),
                                     VisitType = c("Primary", "Primary", "Primary", "Primary"),
                                     FieldSeason = c("2019", "2019", "2020", "2020"),
                                     StartDate = c("4/4/2019", "4/22/2019", "4/4/2020", "5/6/2020"),
                                     TransectNumber = c(0, 1, 0, 3),
                                     LocationOnTape_m = c("1, 1.5", "0.5", "0.5, 1.5", "1.5"))

  expect_setequal(LpiQcMissingPoints(path.to.data = "./dummy-data/bad", data.source = "local", points = c(0.5, 1, 1.5)), expected.missing)
  expect_setequal(LpiQcMissingPoints(park = "PARA", path.to.data = "./dummy-data/bad", data.source = "local", points = c(0.5, 1, 1.5)), dplyr::filter(expected.missing, Park == "PARA"))
  expect_setequal(LpiQcMissingPoints(spring = "MOJA_P_MCSP0", path.to.data = "./dummy-data/bad", data.source = "local", points = c(0.5, 1, 1.5)), dplyr::filter(expected.missing, SpringCode == "MOJA_P_MCSP0"))
  expect_setequal(LpiQcMissingPoints(field.season = "2020", path.to.data = "./dummy-data/bad", data.source = "local", points = c(0.5, 1, 1.5)), dplyr::filter(expected.missing, FieldSeason == "2020"))
})

