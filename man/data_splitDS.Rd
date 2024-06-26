% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_splitDS.R
\name{data_splitDS}
\alias{data_splitDS}
\title{Reduce the data to the last added leaves}
\usage{
data_splitDS(
  training_data,
  bounds_and_levels,
  current_tree,
  data_classes,
  split_goal,
  split_row
)
}
\arguments{
\item{training_data}{All the training data per features.}

\item{bounds_and_levels}{List of maximum and minimum value for numeric and
levels for factor features.}

\item{current_tree}{The currently trained tree.}

\item{data_classes}{Data class for all features.}

\item{split_goal}{Whether we want the data after one specific split or after
a whole binary tree.}

\item{split_row}{If we split a leaf this is the row number of the final split
we are interested in, if its a binary tree its the amount of splits in the
tree.}
}
\value{
The two last added leafs of the tree.
}
\description{
Reduce the data to the last added leaves
}
