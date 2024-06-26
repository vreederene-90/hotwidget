% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hotwidget_update.R
\name{hotwidget_update}
\alias{hotwidget_update}
\title{hotwidget_update}
\usage{
hotwidget_update(input, hotwidget_data, hotwidget_data_updated)
}
\arguments{
\item{input}{the list of input from the server function}

\item{hotwidget_data}{the originally initiated data set}

\item{hotwidget_data_updated}{a reactiveVal(), initiated with the original
data set. This object will contain the data which has been updated by the
user.}
}
\value{
Doesnt return anything, but as a side effect updates
hotwidget_data_updated.
}
\description{
Updates the data set with the changes made by the user in the hotwidget.
It works through collecting the changes made by the user via different
hooks in the handsontable.js library.
\itemize{
\item afterChange
\item afterCreateRow
\item afterRemoveRow
\item afterCreateCol
\item afterRemoveCol
}

Data types are inferred via the original data set. This leads to the
following behaviour
\itemize{
\item non-numerics in numeric columns become NA
\item invalid dates are NOT updated
\item factors where the value is not in the levels become NA
}
}
