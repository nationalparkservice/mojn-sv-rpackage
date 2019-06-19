context("Tree count quality control")

test_that("TreeCtQcDuplicateSpecies returns a dataframe of transects with unknowns that resolved to duplicate species", {
  expected <- tibble::tibble(Park = c("LAKE"),
                             SpringCode = c("LAKE_P_ROGE0"),
                             SpringName = c("Rogers"),
                             VisitType = c("Primary"),
                             FieldSeason = c("2019"),
                             StartDate = c("5/6/2019"),
                             UnknownPlantCode = c("NA, UNK6"),
                             TransectNumber = c(0),
                             USDAPlantsCode = "TREE1")
  expect_mapequal(TreeCtQcDuplicateSpecies(path.to.data = "./dummy-data/bad", data.source = "local"), expected)
})

test_that("TreeCtQcDuplicateSpecies returns an empty dataframe when no duplicate species are present", {
  expect_equal(nrow(TreeCtQcDuplicateSpecies(path.to.data = "./dummy-data/ok", data.source = "local")), 0)
})
