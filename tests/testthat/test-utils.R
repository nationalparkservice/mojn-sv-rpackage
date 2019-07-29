context("Utility functions")

test_that("GetSampleSizes correctly computes sample size", {
  test.data <- tibble::tibble(SpringCode = rep("LAKE_P_BLUE0", 8),
                              SpringName = rep("Blue Point", 8),
                              FieldSeason = c(rep("2018", 4), rep("2019", 4)),
                              TransectNumber = rep(c(1, 2, 2, 3), 2),
                              Stratum = rep(c("T", "M"), 4)
  )
  expected <- tibble::tibble(SpringCode = rep("LAKE_P_BLUE0", 2),
                             SpringName = rep("Blue Point", 2),
                             FieldSeason = c("2018", "2019"),
                             NTransects = as.integer(rep(3, 2)))
  expect_mapequal(GetSampleSizes(test.data), expected)
})
