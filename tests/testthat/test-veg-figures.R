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

test_that("BoxplotSpeciesRichnessBySOP produces the expected plot", {
  expect_known_value(BoxplotSpeciesRichnessBySOP(path.to.data = "./dummy-data/veg-figures", spring = "LAKE_P_BLUE0", field.season = "2019", data.source = "local"), file = "./known-output/BoxplotSpeciesRichnessBySOP-1.Rdata", update = FALSE)
  expect_known_value(BoxplotSpeciesRichnessBySOP(path.to.data = "./dummy-data/veg-figures", spring = "LAKE_P_BLUE0", data.source = "local"), file = "./known-output/BoxplotSpeciesRichnessBySOP-2.Rdata", update = FALSE)
})

test_that("BoxplotLPISpeciesRichness produces the expected plot", {
  expect_known_value(BoxplotLPISpeciesRichness(path.to.data = "./dummy-data/veg-figures", spring = "LAKE_P_HOR0042", data.source = "local"), file = "./known-output/BoxplotLPISpeciesRichness-1.Rdata", update = FALSE)
  expect_known_value(BoxplotLPISpeciesRichness(path.to.data = "./dummy-data/veg-figures", spring = "LAKE_P_BLUE0", data.source = "local"), file = "./known-output/BoxplotLPISpeciesRichness-2.Rdata", update = FALSE)
  expect_error(BoxplotLPISpeciesRichness(path.to.data = "./dummy-data/veg-figures", spring = "LAKE_P_ROGE0", data.source = "local"), regexp = "Data are not available for the spring specified")
})

test_that("BoxplotInvSpeciesRichness produces the expected plot", {
  expect_error(BoxplotInvSpeciesRichness(path.to.data = "./dummy-data/veg-figures", spring = "LAKE_P_HOR0042", data.source = "local"), regexp = "Data are not available for the spring specified")
  expect_known_value(BoxplotInvSpeciesRichness(path.to.data = "./dummy-data/veg-figures", spring = "LAKE_P_BLUE0", data.source = "local"), file = "./known-output/BoxplotInvSpeciesRichness-2.Rdata", update = FALSE)
})

test_that("SpeciesAccumulationCurve produces the expected plot", {
  expect_error(SpeciesAccumulationCurve(path.to.data = "./dummy-data/veg-figures", spring = "LAKE_P_BLUE0", data.source = "local"))
  expect_known_value(SpeciesAccumulationCurve(path.to.data = "./dummy-data/veg-figures", spring = "LAKE_P_BLUE0", field.season = "2019", data.source = "local"), file = "./known-output/SpeciesAccumulationCurve-1.Rdata", update = FALSE)
})
