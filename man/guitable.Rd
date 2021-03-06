% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/guitable.R
\name{guitable}
\alias{guitable}
\title{guitable}
\usage{
guitable(..., out_dir = NULL)
}
\arguments{
\item{...}{Matrix or data frame}

\item{out_dir}{The storage path of the output picture, recommend 'out_dir=getwb()'}
}
\value{
png and pdf of plot
}
\description{
guitable
}
\examples{
\donttest{
guitable()
}
\donttest{
guitable(PK)
}
\donttest{
guitable(PK,out_dir=getwb())
}
}
