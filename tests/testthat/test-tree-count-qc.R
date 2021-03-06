context("Tree count quality control")

test_that("TreeCtQcDuplicateSpecies returns a dataframe of transects with unknowns that resolved to duplicate species", {
  expected <- tibble::tibble(Park = "LAKE",
                             SpringCode = "LAKE_P_BLUE0",
                             SpringName = "Blue Point",
                             VisitType = "Primary",
                             FieldSeason = "2019",
                             StartDate = "4/4/2019",
                             UnknownPlantCode = "NA, A",
                             TransectNumber = as.integer(1),
                             USDAPlantsCode = "WAFI")
  expect_mapequal(TreeCtQcDuplicateSpecies(path.to.data = "./dummy-data/tree-ct-qc/bad", data.source = "local"), expected)
})

test_that("TreeCtQcDuplicateSpecies returns an empty dataframe when no duplicate species are present", {
  expect_equal(nrow(TreeCtQcDuplicateSpecies(path.to.data = "./dummy-data/tree-ct-qc/ok", data.source = "local")), 0)
})
