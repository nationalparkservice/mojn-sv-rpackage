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

