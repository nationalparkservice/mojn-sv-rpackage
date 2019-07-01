context("General quality control")

test_that("QcUnknownSpeciesID returns a dataframe of unknowns that were resolved to more than one species", {
  expected <- tibble::tibble(Park = "LAKE",
                             SpringCode = "LAKE_P_BLUE0",
                             SpringName = "Blue Point",
                             VisitType = "Primary",
                             FieldSeason = "2019",
                             StartDate = "4/4/2019",
                             UnknownPlantCode = "A",
                             USDAPlantsCode = "TYAN, TBD, WAFI, BRRU2")
  expect_mapequal(QcUnknownSpeciesID(path.to.data = "./dummy-data/general-qc/bad", data.source = "local"), expected)
})

test_that("QcUnknownSpeciesID returns an empty dataframe when no duplicate species are present", {
  expect_equal(nrow(QcUnknownSpeciesID(path.to.data = "./dummy-data/general-qc/ok", data.source = "local")), 0)
})

test_that("QcReportDPL returns a dataframe of that lists DPL by spring, SOP, date, and transect", {
  dpl <- QcReportDPL(path.to.data = "./dummy-data/general-qc/ok", data.source = "local")
  load("./known-output/QcReportDPL.Rdata")

  expect_mapequal(dpl, expected)
})

test_that("QcListNoData returns a list of dataframes containing rows that are missing data", {
  no.data <- QcListNoData(path.to.data = "./dummy-data/general-qc/bad", data.source = "local")
  load("./known-output/QcListNoData.Rdata")

  expect_mapequal(no.data, expected)
})

