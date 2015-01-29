#' Calculates a mean (or other function) of response of a factor variable.
#'
#' Calculates a mean (or other function) of response for each observation
#' of a factor variable target_data given that factor variable source_data
#' had response source_response. For training data source_data and
#' target_data coincide. Function allows not to consider levels with
#' little occurence in the train set. Category "other" is created for all
#' such observations and function of overall response in this category is provided.
#'
#' @param source_data Column with data to be interpreted as factor.
#' @param source_response Column with target values {0,1}.
#' @param FUN Function for aggregation, default is mean.
#' @param target_data Column with reference data for output.
#' @param min_count Threshold of minimal occurence of a level.
#' @examples
#' admission <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
#' gre_mean_success <- add.factorfun(source_data = admission$gre,
#'      source_response = admission$admit, min_count = 10)
#' table(gre_mean_success)
#' admission1 <- admission[1:200,]
#' admission2 <- admission[201:400,]
#' table(as.factor(admission1$gre))
#' table(as.factor(admission2$gre))
#' gre_mean_success2 <- add.factorfun(source_data = admission1$gre,
#'     source_response = admission1$admit, target_data = admission2$gre,
#'     min_count = 5)
#' gre_mean_success2[105] #level 220 not included in admission1, thus we get NA
add.factorfun <- function(source_data, source_response, FUN = mean,
                          target_data = NULL, min_count = 1) {
  source_factorized <- as.factor(source_data) #make source_data a factor variable
  if (min_count > 1) {    #for min_count = 1 we do not need other category
    other <- names(which(table(source_factorized) < min_count))
        #names of columns to be replaced
    levels(source_factorized) <- c(levels(source_factorized), "other")
        #adding other among the levels
    source_factorized[source_factorized %in% other] <- "other"
        #replacement of values that scarce
  }
  if (is.null(target_data)) {
    target_factorized <- source_factorized
        #if not specified we use source_data as target_data
  }
  else {  #we need to change the target for which source has not enough
          #observations and relevel the target so that levels that are in
          #source_factorized are the first
    target_factorized <- as.factor(target_data)
    missing_levels <- levels(source_factorized)[! levels(source_factorized) %in%
                                                  levels(target_factorized)]
        #names of columns that are in source_factorized but not in
        #target_factorize
    levels(target_factorized) <- c(levels(target_factorized), missing_levels)
        #adding these levels
    target_factorized[target_factorized %in% other] <- "other"
        #replacing values that were scarse in the training set
    leftover_levels <- levels(target_factorized)[! levels(target_factorized) %in%
                                                   levels(source_factorized)]
        #levels that are missing in source_factorized
    target_factorized <- factor(target_factorized, levels =
                                c(levels(source_factorized), leftover_levels))
        #releveling so that levels thar are missing in source factorized are
        #the last
  }
  source_levels <- tapply(source_response, source_factorized, FUN)
  return(source_levels[target_factorized])
      #when a sample from leftover_levels is reach, NA is produced
}

#' Calculates mean response leaving an observation out.
#'
#' The function calculates mean response for each level of factor variable
#' while leaving out the current observation. It outputs mean response
#' for each observation. As the level depends on the output, it cannot be
#' used in classification trees.
#'
#' @param factor Factor is the variable we want to calculate factor mean for.
#' @param response Response variable with values {0,1}.
#'
#' @examples
#' admission <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
#' add.factormean(factor= admission$rank, response = admission$admit)
add.factormean <- function(factor, response) {
  print("Do not use in classification trees!")
  factorized <- as.factor(factor)
      #if source_data is not a factor, we make it so
  tab <- table(factorized, response)
  success_by_factor <- tab[,2]    #number of positive responses for each factor
  suma <- apply(tab, 1, sum)    #number of all responses for each factor
  success <- (success_by_factor[factorized] - response) / (suma[factorized] - 1)
      #mean response calculation leaving an observation out
  names(success) <- paste(colnames(factor), "mean_success", sep="_")
  return(success)
}

#' Adding Boolean matrix representing factor variable.
#'
#' The function outputs a Boolean matrix of indicators that represents levels of
#' a factor variable.
#'
#' @param data Name of the data set.
#' @param factor Column name of variable we want to create indicators for
#' @examples
#' admission <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
#' add.indicators(data = admission, factor = "rank")
add.indicators <- function(data, factor) {
  factorized <- factor(data[[factor]], exclude = NULL) #NA is a separate levele
  matrix <- model.matrix(~factorized-1) #creation of the Boolign matrix
  colnames(matrix) <- paste(factor, levels(factorized), sep="_")
      #naming the matrix colummns
  return(matrix)
}