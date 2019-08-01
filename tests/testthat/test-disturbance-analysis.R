context("Disturbance analysis")

test_that("DisturbancePercentCover correctly computes percent cover for each disturbance type", {
  dist.list <- GetDisturbanceTypes()
  dist.list <- setdiff(dist.list$Code, c("ANT", "TR"))

  expected.no.zero <- tibble::tibble(Park = "LAKE",
                             SpringCode = "LAKE_P_BLUE0",
                             SpringName = "Blue Point",
                             FieldSeason = "2019",
                             TransectNumber = as.integer(c(0, 0, 1, 1, 2)),
                             DisturbanceCode = c("ANT", "TR", "WSH", "TR", "ANT"),
                             DisturbanceCover_percent = c(33.3, 66.7, 33.3, 33.3, 66.7))
  expected.zero <- tibble::tibble(Park = "LAKE",
                                  SpringCode = "LAKE_P_BLUE0",
                                  SpringName = "Blue Point",
                                  FieldSeason = "2019",
                                  TransectNumber = as.integer(0),
                                  DisturbanceCode = c("ANT", "TR", dist.list),
                                  DisturbanceCover_percent = c(33.3, 66.7, rep(0, length(dist.list))))

  dist.w.zeroes <- DisturbancePercentCover(path.to.data = "./dummy-data/disturbance-analysis", data.source = "local") %>%
    dplyr::filter(TransectNumber == 0)

  expect_mapequal(DisturbancePercentCover(drop.zeroes = TRUE, path.to.data = "./dummy-data/disturbance-analysis", data.source = "local"), expected.no.zero)
  expect_mapequal(dist.w.zeroes, expected.zero)
})
