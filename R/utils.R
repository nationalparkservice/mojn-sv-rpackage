#' Open a Connection to the Spring Veg Database
#'
#' @param use.mojn.default Connect to the MOJN Spring Veg database? MOJN staff should use this option. Defaults to \code{TRUE}.
#' @param drv DBI driver to use. Defaults to \code{odbc::odbc()}.
#' @param ... Additional arguments to \code{\link[pool]{dbPool}}. Ignored if \code{use.mojn.default} is \code{TRUE}.
#'
#' @return A database connection pool object
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' conn <- OpenDatabaseConnection()
#'
OpenDatabaseConnection <- function(use.mojn.default = TRUE, drv = odbc::odbc(), ...) {

  if (use.mojn.default) {
    params <- readr::read_csv("M:/MONITORING/SLS_Veg/Data/Database/ConnectFromR/sv-database-conn.csv") %>%
      as.list()
    params$drv <- drv
    my.pool <- do.call(pool::dbPool, params)
  } else {
    my.pool <- pool::dbPool(drv, ...)
  }

  return(my.pool)

}

#' Close a Connection to the Spring Veg Database
#'
#' @param conn A database connection pool object generated from a call to \code{OpenDatabaseConnection()}
#'
#' @return None.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' conn <- OpenDatabaseConnection()
#' CloseDatabaseConnection(conn)
#'
CloseDatabaseConnection <- function(conn) {
  pool::poolClose(conn)
}
