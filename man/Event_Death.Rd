% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Event_Death.R
\name{Event_Death}
\alias{Event_Death}
\title{Event - Death}
\usage{
Event_Death(input_name, input)
}
\arguments{
\item{input_name}{currently only 'Data_none' and 'Data_per_disability' are supported. should include bool_war in the future}

\item{input}{e.g. per_disability}
}
\value{
bool_dead
}
\description{
calculates the probability of death.
}
