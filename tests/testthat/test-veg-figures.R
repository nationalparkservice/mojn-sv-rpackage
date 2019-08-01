context("Vegetation figures")

test_that("BoxplotSpeciesByStratum produces the expected plot", {
  expect_known_value(BoxplotSpeciesByStratum(path.to.data = "./dummy-data/veg-figures", spring = "LAKE_P_BLUE0", field.season = "2019", data.source = "local"), file = "./known-output/BoxplotSpeciesByStratum-1.Rdata", update = FALSE)
  expect_known_value(BoxplotSpeciesByStratum(path.to.data = "./dummy-data/veg-figures", spring = "LAKE_P_BLUE0", data.source = "local"), file = "./known-output/BoxplotSpeciesByStratum-2.Rdata", update = FALSE)
})

test_that("TableSpeciesDetected produces the expected output", {
  expect_known_value(TableSpeciesPerTransect(path.to.data = "./dummy-data/veg-figures", spring = "LAKE_P_BLUE0", field.season = "2019", data.source = "local"), file = "./known-output/TableSpeciesPerTransect-1.Rdata", update = FALSE)
  expect_known_value(TableSpeciesPerTransect(path.to.data = "./dummy-data/veg-figures", data.source = "local"), file = "./known-output/TableSpeciesPerTransect-2.Rdata", update = FALSE)
})

test_that("HistogramCanopyPercentCover produces the expected output", {
  expect_known_value(HistogramCanopyPercentCover(path.to.data = "./dummy-data/veg-figures", spring = "LAKE_P_BLUE0", field.season = "2019", data.source = "local"), file = "./known-output/HistogramCanopyPercentCover-1.Rdata", update = FALSE)
  expect_known_value(HistogramCanopyPercentCover(path.to.data = "./dummy-data/veg-figures", spring = "LAKE_P_BLUE0", data.source = "local"), file = "./known-output/HistogramCanopyPercentCover-2.Rdata", update = FALSE)
})
