context("Vegetation figures")

test_that("BoxplotSpeciesByStratum produces the same plot", {
  expect_known_value(BoxplotSpeciesByStratum(path.to.data = "./dummy-data/veg-figures", spring = "LAKE_P_BLUE0", field.season = "2019", data.source = "local"), file = "./known-output/boxplot-species-by-stratum-1.Rdata")
  expect_known_value(BoxplotSpeciesByStratum(path.to.data = "./dummy-data/veg-figures", spring = "LAKE_P_BLUE0", data.source = "local"), file = "./known-output/boxplot-species-by-stratum-2.Rdata")
})
