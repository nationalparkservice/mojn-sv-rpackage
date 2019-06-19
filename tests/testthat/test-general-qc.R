context("General quality control")

test_that("QcUnknownSpeciesID returns a dataframe of unknowns that were resolved to more than one species", {
  expected <- tibble::tibble(Park = c("LAKE", "LAKE"),
                             SpringCode = c("LAKE_P_ROGE0", "LAKE_P_HOR0042"),
                             SpringName = c("Rogers", "Horsethief Canyon"),
                             VisitType = c("Primary", "Primary"),
                             FieldSeason = c("2019", "2019"),
                             StartDate = c("5/6/2019", "4/22/2019"),
                             UnknownPlantCode = c("UNK2", "UNK2"),
                             USDAPlantsCode = c("TREE1, TREE2", "UNK, TBD, PLANT12"))
  expect_mapequal(QcUnknownSpeciesID(path.to.data = "./dummy-data/bad", data.source = "local"), expected)
})

test_that("QcUnknownSpeciesID returns an empty dataframe when no duplicate species are present", {
  expect_equal(nrow(QcUnknownSpeciesID(path.to.data = "./dummy-data/ok", data.source = "local")), 0)
})
