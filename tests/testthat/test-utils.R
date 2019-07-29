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
