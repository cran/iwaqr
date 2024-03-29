% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_DoneenL.R
\name{plot_DoneenL}
\alias{plot_DoneenL}
\title{Plot Doneen diagram (Low permeability) for all rows}
\usage{
plot_DoneenL(
  df,
  tc_column,
  PI_column,
  label_column = NULL,
  grp_column = NULL,
  convert_units = FALSE
)
}
\arguments{
\item{df}{Data frame containing the necessary columns.}

\item{tc_column}{Column name for total concentration (tc).}

\item{PI_column}{Column name for PI (optional).}

\item{label_column}{Column name for labels (optional).}

\item{grp_column}{Column name for grouping (optional).}

\item{convert_units}{Logical, whether to convert units.}
}
\value{
A ggplot object representing the USSL diagram.
#'@examples
df <- data.frame(tc = c(80, 65, 70),
                 PI = c(30, 65, 150),
                 Color = c("red", "green", "blue"))
plot_DoneenL(df, tc_column = "tc", PI_column = "PI", label_column = NULL, grp_column = NULL, convert_units = FALSE)
}
\description{
This function plots the USSL diagram for the given data frame.
}
