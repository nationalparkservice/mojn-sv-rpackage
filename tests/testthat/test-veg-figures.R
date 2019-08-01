context("Vegetation figures")

test_that("BoxplotSpeciesByStratum produces the expected plot", {
  expect_known_value(BoxplotSpeciesByStratum(path.to.data = "./dummy-data/veg-figures", spring = "LAKE_P_BLUE0", field.season = "2019", data.source = "local"), file = "./known-output/BoxplotSpeciesByStratum-1.Rdata")
  expect_known_value(BoxplotSpeciesByStratum(path.to.data = "./dummy-data/veg-figures", spring = "LAKE_P_BLUE0", data.source = "local"), file = "./known-output/BoxplotSpeciesByStratum-2.Rdata")
})
