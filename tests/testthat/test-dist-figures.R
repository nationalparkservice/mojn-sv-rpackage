context("Disturbance figures")

test_that("BoxplotDisturbanceCover produces the expected plot", {
  expect_known_value(BoxplotDisturbanceCover(path.to.data = "./dummy-data/disturbance-figures", spring = "LAKE_P_BLUE0", drop.nonexistent = TRUE, field.season = "2019", data.source = "local"), file = "./known-output/BoxplotDisturbanceCover-1.Rdata", update = FALSE)
  expect_known_value(BoxplotDisturbanceCover(path.to.data = "./dummy-data/disturbance-figures", spring = "LAKE_P_BLUE0", data.source = "local"), file = "./known-output/BoxplotDisturbanceCover-2.Rdata", update = FALSE)
})
