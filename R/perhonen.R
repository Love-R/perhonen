#' perhonen: Functions for data manipulation & connections to Vowpal Wabbit
#'
#' Package provides various functions for transformations csv to
#' Vowpal Wabbit format and functions that make feature engineering easier -
#' calculating mean response for a factor variable, creating matrix of
#' indicators for each level of a factor variable, etc.
#'
#' @section Data manipulation:
#'
#' Function regarding data manipulation are inspired by
#' frequently performed tasks at Kaggle competitions and advanced analytics
#' in general. Function add.factormean calculates mean response for each
#' observation of a factor variable. Function add.factorfun allows to
#' calculate other function than mean. Function add.indicatiors adds
#' Boolean matrix representing levels of a factor variable.
#'
#' @section Transformation to Vowpal wabbit format:
#'
#' Vowpal Wabbit is an efficient tool for building models for data stored
#' at hard drive. Goal of perhonen functions is to transform a flat csv
#' file into Vowpal Wabbit format. This is done via function csv2vw.
#' csv2vw uses fread to read a csv file in chunks and  calls two internal
#' functions: change_NA and dt2vw.
#'
#' change_NA is a function that creates a new vector of indicators stating
#' whether an observation is NA or not for all columns that have at least one NA.
#' dt2vw transform a data.table to a format for Vowpal Wabbit.
#' It loops over data.table columns, each type of column data type it treats
#' differently. Character and factor variables are outputed as
#' "columnname_value", numeric variables are outputed as "columnname: value".
#'
#' An advantage of the approach is that it loops over columns, not over
#' each row. The author believes that this is quick in R.
#' However, a downside of the approach is that it is not efficient in
#' the size of output: one would not like to print variables that have 0 value;
#' and to print only the variable name when its value is 1.
#'
#' A downside of reading by fread is that it takes a long time to skip lines.
#' Hence, choosing a low size of chunk n results in that fread has to skip lines
#' many times. On the other hand, choosing the size n too high results in
#' memory problems. The optimal value of n is such that maximum RAM is not
#' fully occupied or just for a short time. An improvement of this issue is in
#' fread top priority list.
#'
#' Vowpal Wabbit option of namespaces is not used. Functions could be
#' further extended to allow to create namespaces of each categorial
#' variable.
#'
#' @section Benchmarks for Vowpal Wabbit:
#'
#' *** 18 minutes to read a 1.5 GB file with 6 millions rows and 40 columns
#'     chunk size 2 000 000 fread in total about 1.5 minute
#'     Size of producted file: 3.3 GB
#'     Altering the chunk size did not affect the speed
#'
#' *** 235 minutes to read a 11.3 GB file with 45.8 millions rows and 40 columns,
#'     chunk size 2 000 000, fread spent in total 110 minutes
#'     Size of produced file: 25 GB
#'
#'Apart from fread, the speed is about 11 minutes per GB.
#'We conclude that functions other than fread do not depend on scale of
#'the problem as long a reasonable chunk size is chosen.
#'
#' @docType package
#' @name perhonen
NULL
