% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Eq.r
\name{eq_read_data}
\alias{eq_read_data}
\title{Read earthquake data text file}
\usage{
eq_read_data(filename)
}
\arguments{
\item{filename}{A character string giving the path, name, and extension of a file to read in. Only the name and extension needs to be provided if the file is in the working directory.}
}
\value{
This function returns a data frame table containing data for earthquakes.
}
\description{
This function (eq_read_data) reads the text file with earthquake data and creates a data frame.
The function assumes that the text file is stored in the working directory.
The file is read using the read_delim function from the readr package (\code{\link[readr]{read_delim}}).
}
\examples{
\dontrun{
eq_data <- eq_read_data("signif.txt")
eq_data <- eq_read_data(filename= "signif.txt")
}

}
