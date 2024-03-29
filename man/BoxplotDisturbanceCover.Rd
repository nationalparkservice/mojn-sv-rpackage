% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/disturbance-figures.R
\name{BoxplotDisturbanceCover}
\alias{BoxplotDisturbanceCover}
\title{Disturbance percent cover, broken down by disturbance type.}
\usage{
BoxplotDisturbanceCover(conn, path.to.data, spring, field.season,
  drop.nonexistent = FALSE, data.source = "database", plot.title,
  sub.title, x.lab, y.lab, ymax, ymin, xmax, xmin)
}
\arguments{
\item{conn}{Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.}

\item{path.to.data}{The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.}

\item{spring}{Spring code to generate a plot for, e.g. "LAKE_P_BLUE0".}

\item{field.season}{Optional. Field season name to filter on, e.g. "2019".}

\item{drop.nonexistent}{Omit disturbances that were not observed at all? Defaults to FALSE.}

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
Disturbance percent cover, broken down by disturbance type.
}
\details{
Only includes data from visits labeled 'Primary.' Percent cover is calculated only from points at which data were collected - if only three points were recorded for a transect and one of them had bike disturbance, the disturbance type BK would have 33.3% cover for that transect.
}
