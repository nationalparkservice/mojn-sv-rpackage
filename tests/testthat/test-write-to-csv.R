context("Write data to csv")

test_that("SaveDataToCsv successfully writes data to csv", {
  conn <- OpenDatabaseConnection()
  dir <- "temp_test-csv"
  expected.files <- c("LPICanopy.csv", "LPIDisturbance.csv", "LPISoilSurface.csv", "LPITransect.csv", "Spring.csv", "SpringVisit.csv", "TreeCount.csv", "TreeCountTransect.csv", "VegetationInventory.csv", "VegetationInventoryTransect.csv")

  SaveDataToCsv(conn, dir, create.folders = TRUE, overwrite = FALSE)
  files <- list.files(dir)

  expect_setequal(files, expected.files)
  expect_error(SaveDataToCsv(conn, dir, create.folders = TRUE, overwrite = TRUE), NA)
  expect_error(SaveDataToCsv(conn, dir, create.folders = FALSE, overwrite = TRUE), NA)

  unlink(dir, recursive = TRUE)
  dir.create(dir)
  SaveDataToCsv(conn, dir, create.folders = FALSE, overwrite = FALSE)

  expect_setequal(files, expected.files)

  unlink(dir, recursive = TRUE)
  CloseDatabaseConnection(conn)
})

test_that("SaveDataToCsv doesn't create a new folder when create.folders = FALSE", {
  conn <- OpenDatabaseConnection()
  dir <- "temp_test-csv"

  expect_error(SaveDataToCsv(conn, dir, create.folders = FALSE, overwrite = TRUE))
  expect_error(SaveDataToCsv(conn, dir, create.folders = FALSE, overwrite = FALSE))
  expect_false(dir.exists(dir))

  CloseDatabaseConnection(conn)
})

test_that("SaveDataToCsv doesn't overwrite existing files when overwrite = FALSE", {
  conn <- OpenDatabaseConnection()
  dir <- "temp_test-csv"
  SaveDataToCsv(conn, dir, create.folders = TRUE, overwrite = FALSE)

  expect_error(SaveDataToCsv(conn, dir, create.folders = TRUE, overwrite = FALSE))
  expect_error(SaveDataToCsv(conn, dir, create.folders = FALSE, overwrite = FALSE))

  unlink(dir, recursive = TRUE)
  CloseDatabaseConnection(conn)
})
