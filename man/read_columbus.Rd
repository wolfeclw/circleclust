% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_columbus.R
\name{read_columbus}
\alias{read_columbus}
\title{Read Columbus gps logger .csv file}
\usage{
read_columbus(path, tzone_gps = "UTC", tzone_out = "America/New_York")
}
\arguments{
\item{path}{string; filepath of input .csv}

\item{tzone_gps}{character; time zone of the Columbus logger. Default = 'UTC'}

\item{tzone_out}{character; time zone of the output file. Default = 'America/New_York.'}
}
\value{

}
\description{
\code{read_columbus()} imports latitude and longitude from a Columbus gps logger file.
}
\examples{
\dontrun{

read_columbus(path, tzone_gps = 'UTC', tzone_out = 'America/New_York.'
)
}
}