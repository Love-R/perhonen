#' Cultivates NA's handling.
#'
#' Function that creates an is.na() vector for each column that has at least one NA
#' and it then replaces all NA's with 0. The function is primarily used via a call
#' from csv2vw.
#'
#' @param dataset Name of the dataset.
change_NA <- function(dataset) {
  cols_NA <- colnames(dataset)[which(apply(dataset, 2, function(x) {sum(is.na(x))})>0)]
  for (i in cols_NA) {
    dataset[[as.character(paste0(i,"_","NA"))]] <- as.numeric( is.na (dataset[[i]]))
    data.table::set(dataset,which(is.na(dataset[[as.character(paste0(i))]])),i,0)
        #Setting NA to zero
  }
}

#' Function converts data.table to Vowpal Wabbit format.
#'
#' Internal function called by csv2vw that converts data.table to Vowpal Wabbit format.
#'
#' @param dataset Name of the dataset.
#' @param id Name of id variable column in data.
#' @param target Name of target variable column in data; it should be specified for
#' training, left out for testing
#' @param weights Optional vector of weight of each observation, it is a single vector,
#' not a part of the dataset.
#' @param char_factor,int_numeric Optional partion of all column names into
#' two groups.
dt2vw <- function(dataset, id = NULL, target = NULL, weights = NULL, char_factor, int_numeric) {
  if (is.null(char_factor) | is.null(int_numeric)) {
    cols <- colnames(dataset)[which(!colnames(dataset) %in% c(target,id))]
    classes <- sapply(dataset[,cols, with = F], class)
    intnumeric <- names(classes)[which((classes == "integer")|(classes == "numeric"))]
    charfactor <- names(classes)[which((classes == "factor")|(classes == "character"))]
  }
  else{
    charfactor = char_factor[which(!char_factor %in% c(target,id))]
    intnumeric = int_numeric[which(!int_numeric %in% c(target,id))]
  }

  #Misspecification handling:
  missing_weights <- ((missing(weights))|is.null(weights))
  missing_target <- ((missing(target))|is.null(target))
  missing_id <- ((missing(id))|is.null(id))
  if (nrow(dataset) != length(weights) & !missing_weights) {
    stop("Length of vector weights not corresponding to nrows of data")
  }
  else if (missing_target & !missing_weights) {
    stop("Weights can be specified only if the target variable is present")
  }
  else { #id and target variable may or may not be present which effects how
         #variable "output" (containing the part before |) will look like
    if(missing_target & !missing_id ) { #for testing the target variable is absent
      output <- cbind(weights, paste0("'", dataset[[id]]), "|" )
    }
    else if (!missing_target & !missing_id ) { #for training the target variable is present
      output <- cbind(dataset[[target]],weights, paste0("'", dataset[[id]]), "|" )
    }
    else if (missing_target & missing_id) { #for testing the target variable is absent
      if(missing_weights) {
        output <- rep("|", nrow(dataset))
      }
      else {
        output <- cbind(weights,"|")
      }
    }
    else if (!missing_target & missing_id ) { #for training the target variable is present
      output <- cbind(dataset[[target]], weights, "|")
    }
    ### Bind features based on their type
    for (i in intnumeric) {
      output <- cbind(output, i, ":", dataset[,i, with = F] )
    }
    for (i in charfactor) {
      output <- cbind(output, paste0(i, "_", as.matrix(dataset[,i, with = F])) )
    }
  }
  #Output now has several columns, make just one of it:
  return(apply(output, 1, function(x) {paste(x, collapse = " ")}))
}

#' Function reads a dataset in chunks and transforms it into Vowpal Wabbit format.
#'
#' Function reads a dataset in chunks and transforms it into Vowpal Wabbit format
#' calling functions change_NA and dt2vw.
#' It loops over columns, each type of column data type it treats differently.
#' Character and factor variables are outputed as "columnname_value",
#' numeric variables are outputed as "columnname: value". It is possible to
#' specify how to treat columns (e.g. in order to treat numeric variable as factor)
#' but it is not necessary.
#'
#' @param infile Name of the dataset in csv format to transform.
#' @param outfile Name of the output file in Vowpal Wabbit format.
#' @param n Size of the chunk (number of lines) that is read at once.
#' @param id Name of the id column
#' @param target Name of the target variable column
#' @param weights External vector of weights to be specified.
#' @param start How many lines to skip.
#' @param char_factor,int_numeric Optional partion of all column names into
#' two groups.
#' @param ... Additional parameters to be passed to data.table fread function.
#' @examples
#' \dontrun{
#' csv2vw(infile= "admission.csv", outfile = "admissionvw.txt", target = "admit",
#'      weights = c(0,1) ) #weights need to as long as the data
#' csv2vw(infile= "admission.csv", outfile = "admissionvw.txt", id = "id",  weights= weights)
#' #weights ar only for training, i.e. target variable must be present
#' }
csv2vw <- function(infile, outfile, n = 1000000, id = NULL, target = NULL, weights = NULL, start = 0,
                   int_numeric = NULL, char_factor = NULL,...) {
  all_read <- F
  i = 1
  first = T
  while (!all_read) {
    if (first) {
      part <- data.table::fread(input = infile, nrows = n, skip = start + (i-1) * n, header = T,  ...)
      header <- names(part)
    }
    else {
      part <- data.table::fread(input = infile, nrows = n, skip = start + (i-1) * n, header = F,  ...)
      cat(nrow(part), "rows read\n")
      data.table::setnames(part, old = colnames(part), new = header)
    }
    change_NA(part)
    if (missing(weights)) {
      vwpart <- dt2vw(part, id = id, target = target, int_numeric = int_numeric, char_factor = char_factor)
    }
    else {
      vwpart <- dt2vw(part, id = id, target = target, weights = weights[((i-1)*n):(i*n)],
                      int_numeric = int_numeric, char_factor = char_factor)
    }
    if (first) {
      write.table(vwpart, file=outfile, append = FALSE, quote = F, row.names = F, col.names = F )
    }
    else {
      write.table(vwpart, file=outfile, append = TRUE, quote = F, row.names = F, col.names = F )
    }
    if(nrow(part) < n) {all_read = T} #If less then n rows was read, eof was reached
    first = F
    cat(nrow(part), "rows written, in total", nrow(part) + n * (i-1), "rows written \n")
    i =  i+1
    part <- NULL #Prepare for garbage collection
    vw <- NULL
    gc() #Clean after Yourself
  }
}
