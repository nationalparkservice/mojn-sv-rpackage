context("Database connection")

test_that("Connection to MOJN SV database is successful", {
  conn <- OpenDatabaseConnection()
  result <- pool::dbGetQuery(conn, "SELECT TOP 1 Park FROM analysis.Spring WHERE Park = 'DEVA'")
  CloseDatabaseConnection(conn)

  expect_equal(result$Park, "DEVA")
})
