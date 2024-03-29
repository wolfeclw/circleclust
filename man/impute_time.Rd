% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impute_time.R
\name{impute_time}
\alias{impute_time}
\title{Impute datetime for gaps in time series data}
\usage{
impute_time(
  df,
  dt_field = NULL,
  fill_cols = NULL,
  force = FALSE,
  force_interval = 1
)
}
\arguments{
\item{df}{a data frame with a datetime field.}

\item{dt_field}{character; name of datetime field.}

\item{fill_cols}{character; names of columns that should have values carried forward.}

\item{force}{logical; force the function to fill time between sampling intervals
even if no lapses are detected. For example, if location is recorded at a 5 second
sampling interval, extra rows can be inserted to convert the sampling resolution
to 1 measurement per second (\verb{force = TRUE, force_interval = 1}). Default = FALSE.}

\item{force_interval}{numeric; the new sampling frequency in seconds to be
forced upon the input data frame. \code{force_interval} must be less than the
sampling interval of the input data frame.}
}
\description{
\code{impute_time()} fills datetime gaps in time series data. Specifically,
this function was designed to impute gaps in GPS data that may result
from signal loss or because the GPS logger goes into 'sleep' mode (i.e. Columbus GPS logger).
Datetime rows are added to the input data frame under the specified datetime
column (\code{dt_field}) to fill gaps between time stamps. The sampling interval is
automatically calculated.
}
\details{
Columns that have one unique value and are unaffected by imputation  (i.e.
participant or sample ID) can be specified under '\code{fill_cols}. Values for these
columns will be carried forward from the last observation.  If NULL (default),
only the datetime column (\code{dt_field}) will be assigned values for the imputed rows.

\code{impute_coords()} can be used in conjunction with \code{impute_time()} to assign
latitude and longitude to the added timestamps/rows.
}
\examples{
\dontrun{
impute_time(df, dt_field = 'Date_Time', fill_cols = 'ID') \%>\%
  impute_coords(dt_field = 'Date_Time')
}

}
