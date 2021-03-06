% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bearpower.R
\name{bearpower}
\alias{bearpower}
\title{bear power technical analysis function}
\usage{
bearpower(h,down,day)
}
\arguments{
\item{h}{an stock data}

\item{down}{an rsi down horizon value}

\item{day}{Days of data shown}
}
\value{
an analysis of stock data for bear power technical analysis indicators
}
\description{
bear power technical analysis function is to analyze the reversal pattern conform to the downward trend and bear market of stock data
}
\details{
use RSI analysis of the strength of the stock market trend, analyze trends conform to bear power, and RSI function need library 'TTR'
}
\examples{
\dontrun{
library(quantmod)
aapl<-getSymbols("AAPL",src="yahoo",auto.assign=FALSE)
bearpower(aapl,40,5)
}
}
\author{
Chun-Yu Liu <john401528@gmail.com>
}
