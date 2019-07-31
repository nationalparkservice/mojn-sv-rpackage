context("Utility functions")

test_that("GetSampleSizes correctly computes sample size", {
  test.data <- tibble::tibble(SpringCode = rep("LAKE_P_BLUE0", 8),
                              FieldSeason = c(rep("2018", 4), rep("2019", 4)),
                              TransectNumber = as.integer(c(1, 2, 2, 3, 1, 2, 3, 4)),
                              Stratum = rep(c("T", "M"), 4)
  )
  expected <- tibble::tibble(SpringCode = rep("LAKE_P_BLUE0", 2),
                             FieldSeason = c("2018", "2019"),
                             NTransects = as.integer(c(3, 4)))
  expect_mapequal(GetSampleSizes(test.data), expected)
})

test_that("FacetTitle correctly generates titles for plot facets", {
  test.data <- tibble::tibble(SpringCode = rep("LAKE_P_BLUE0", 2),
                             FieldSeason = c("2018", "2019"),
                             NTransects = as.integer(c(3, 4)))
  expected <- c("2018 (n = 3)", "2019 (n = 4)")
  expect_equal(FacetTitle(c("2018", "2019"), test.data), expected)
  expect_equal(FacetTitle("2019", test.data), expected[2])
})

test_that("GetSpringName retrieves the correct spring name for the spring code provided", {
  expect_equal(GetSpringName(path.to.data = "./dummy-data/ok", spring.code = "LAKE_P_BLUE0", data.source = "local"), "Blue Point")
  expect_equal(GetSpringName(path.to.data = "./dummy-data/ok", spring.code = "JOTR_P_FORT0", data.source = "local"), "Fortynine Palms")
  expect_error(GetSpringName(path.to.data = "./dummy-data/ok", spring.code = "asdf", data.source = "local"), "Data are not available for the spring specified")
})

test_that("ReadAndFilterData correctly filters data", {
  park <- ReadAndFilterData(path.to.data = "./dummy-data/read-and-filter", park = "JOTR", data.source = "local", data.name = "LPICanopy")
  spring <- ReadAndFilterData(path.to.data = "./dummy-data/read-and-filter", spring = "LAKE_P_BLUE0", data.source = "local", data.name = "LPICanopy")
  field.season <- ReadAndFilterData(path.to.data = "./dummy-data/read-and-filter", field.season = "2020", data.source = "local", data.name = "LPICanopy")
  field.season.mult <- ReadAndFilterData(path.to.data = "./dummy-data/read-and-filter", field.season = c("2019", "2020"), data.source = "local", data.name = "LPICanopy")

  expect_equal(unique(park$Park), "JOTR")
  expect_equal(unique(spring$SpringCode), "LAKE_P_BLUE0")
  expect_equal(unique(field.season$FieldSeason), "2020")
  expect_equal(unique(field.season.mult$FieldSeason), c("2019", "2020"))

  expect_error(ReadAndFilterData(path.to.data = "./dummy-data/read-and-filter", park = "MOJA", spring = "LAKE_P_BLUE0", data.source = "local", data.name = "LPICanopy"), "Data are not available for the park specified")
  expect_error(ReadAndFilterData(path.to.data = "./dummy-data/read-and-filter", spring = "MOJA_P_MCSP0", data.source = "local", data.name = "LPICanopy"), "Data are not available for the spring specified")
  expect_error(ReadAndFilterData(path.to.data = "./dummy-data/read-and-filter", field.season = "2018", data.source = "local", data.name = "LPICanopy"), "Data are not available for one or more of the field seasons specified")
  expect_error(ReadAndFilterData(path.to.data = "./dummy-data/read-and-filter", spring = "MOJA_P_MCSP0", data.source = "local", data.name = "LPICanopy"), "Data are not available for the spring specified")
  expect_error(ReadAndFilterData(path.to.data = "./dummy-data/read-and-filter", field.season = c("2019", "2018"), data.source = "local", data.name = "LPICanopy"), "Data are not available for one or more of the field seasons specified")
})

