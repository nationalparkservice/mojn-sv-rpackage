% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vegetation-figures.R
\name{SpeciesAccumulationCurve}
\alias{SpeciesAccumulationCurve}
\title{Species accumulation curve of one spring in one field season}
\usage{
SpeciesAccumulationCurve(conn, path.to.data, spring, field.season,
  data.source = "database", plot.title, sub.title, x.lab, y.lab, ymax,
  ymin, xmax, xmin)
}
\arguments{
\item{conn}{Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.}

\item{path.to.data}{The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.}

\item{spring}{Spring code to generate a plot for, e.g. "LAKE_P_BLUE0".}

\item{field.season}{Field season name to filter on, e.g. "2019".}

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
Species accumulation curve of one spring in one field season
}
\details{
Only includes data from visits labeled 'Primary.' Does not include UNKS but does include To Be Determined unknown species.
}
