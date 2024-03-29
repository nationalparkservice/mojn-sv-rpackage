% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vegetation-figures.R
\name{BoxplotWaterPercentCover}
\alias{BoxplotWaterPercentCover}
\title{Boxplot of water percent cover over time}
\usage{
BoxplotWaterPercentCover(conn, path.to.data, spring,
  data.source = "database", plot.title, sub.title, x.lab, y.lab, ymax,
  ymin, xmax, xmin)
}
\arguments{
\item{conn}{Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.}

\item{path.to.data}{The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.}

\item{spring}{Spring code to generate a plot for, e.g. "LAKE_P_BLUE0".}

\item{data.source}{Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.}

\item{plot.title}{Optional custom plot title. Leave blank to use a sensible default. Use "" to omit the title.}

\item{sub.title}{Optional custom plot subtitle.  Leave blank to use a sensible default. Use "" to omit the subtitle.}

\item{x.lab}{Optional X axis label. Leave blank to use a sensible default. Use "" to omit the axis label.}

\item{y.lab}{Y axis label. Leave blank to use a sensible default. Use "" to omit the axis label.}

\item{ymax}{Optional maximum y limit.}

\item{ymin}{Optional minimum y limit.}

\item{xmax}{Optional maximum x limit.}

\item{xmin}{Optional minimum x limit.}
}
\value{
A ggplot object.
}
\description{
Boxplot of water percent cover over time
}
\details{
Only includes data from visits labeled 'Primary.' Points with water recorded as "NA" and "ND" are omitted. Omits Blue Point transects 1 - 3 in 2019 since water presence/absence was not yet being recorded consistently.
}
