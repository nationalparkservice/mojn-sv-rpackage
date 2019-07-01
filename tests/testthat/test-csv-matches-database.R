context("Reading from database and csv")

# Write temporary csv files
conn <- OpenDatabaseConnection()
dir <- "temp_test-csv"
SaveDataToCsv(conn, dir, create.folders = TRUE, overwrite = FALSE)
CloseDatabaseConnection(conn)

test_that("Spring.csv matches data read from database", {
  c <- OpenDatabaseConnection()
  db <- ReadAndFilterData(c, data.name = "Spring")
  CloseDatabaseConnection(c)
  csv <- ReadAndFilterData(path.to.data = dir, data.source = "local", data.name = "Spring")

  expect_mapequal(db, csv)
})

test_that("SpringVisit.csv matches data read from database", {
  c <- OpenDatabaseConnection()
  db <- ReadAndFilterData(c, data.name = "SpringVisit")
  CloseDatabaseConnection(c)
  csv <- ReadAndFilterData(path.to.data = dir, data.source = "local", data.name = "SpringVisit")

  expect_mapequal(db, csv)
})

test_that("LPITransect.csv matches data read from database", {
  c <- OpenDatabaseConnection()
  db <- ReadAndFilterData(c, data.name = "LPITransect")
  CloseDatabaseConnection(c)
  csv <- ReadAndFilterData(path.to.data = dir, data.source = "local", data.name = "LPITransect")

  expect_mapequal(db, csv)
})

test_that("TreeCountTransect.csv matches data read from database", {
  c <- OpenDatabaseConnection()
  db <- ReadAndFilterData(c, data.name = "TreeCountTransect")
  CloseDatabaseConnection(c)
  csv <- ReadAndFilterData(path.to.data = dir, data.source = "local", data.name = "TreeCountTransect")

  expect_mapequal(db, csv)
})

test_that("VegetationInventoryTransect.csv matches data read from database", {
  c <- OpenDatabaseConnection()
  db <- ReadAndFilterData(c, data.name = "VegetationInventoryTransect")
  CloseDatabaseConnection(c)
  csv <- ReadAndFilterData(path.to.data = dir, data.source = "local", data.name = "VegetationInventoryTransect")

  expect_mapequal(db, csv)
})

test_that("LPICanopy.csv matches data read from database", {
  c <- OpenDatabaseConnection()
  db <- ReadAndFilterData(c, data.name = "LPICanopy")
  CloseDatabaseConnection(c)
  csv <- ReadAndFilterData(path.to.data = dir, data.source = "local", data.name = "LPICanopy")

  expect_mapequal(db, csv)
})

test_that("LPIDisturbance.csv matches data read from database", {
  c <- OpenDatabaseConnection()
  db <- ReadAndFilterData(c, data.name = "LPIDisturbance")
  CloseDatabaseConnection(c)
  csv <- ReadAndFilterData(path.to.data = dir, data.source = "local", data.name = "LPIDisturbance")

  expect_mapequal(db, csv)
})

test_that("LPISoilSurface.csv matches data read from database", {
  c <- OpenDatabaseConnection()
  db <- ReadAndFilterData(c, data.name = "LPISoilSurface")
  CloseDatabaseConnection(c)
  csv <- ReadAndFilterData(path.to.data = dir, data.source = "local", data.name = "LPISoilSurface")

  expect_mapequal(db, csv)
})

test_that("TreeCount.csv matches data read from database", {
  c <- OpenDatabaseConnection()
  db <- ReadAndFilterData(c, data.name = "TreeCount")
  CloseDatabaseConnection(c)
  csv <- ReadAndFilterData(path.to.data = dir, data.source = "local", data.name = "TreeCount")

  expect_mapequal(db, csv)
})

test_that("VegetationInventory.csv matches data read from database", {
  c <- OpenDatabaseConnection()
  db <- ReadAndFilterData(c, data.name = "VegetationInventory")
  CloseDatabaseConnection(c)
  csv <- ReadAndFilterData(path.to.data = dir, data.source = "local", data.name = "VegetationInventory")

  expect_mapequal(db, csv)
})

# Remove temporary csv's
unlink(dir, recursive = TRUE)
