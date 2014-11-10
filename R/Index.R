#' Index
#'
#' This function computes a mean-index or a sum-index for use in social sciences
#' @param items The items you want to compute the index from. Has to be a data.frame.
#' type mean will give you a mean-index, sum a sum-index
#' maximum the maximum of allowed missings in a row
#' minimum the minimum of non-missings in a row for which to compute the scale
#' @keywords mean-index
#' @export
#' @examples
#' examples to insert

Index <- function(items, type=c("mean", "sum"), maximum, minimum) {
  if(!is.data.frame(items))
    stop("Data must be a data frame")
  if(missing(maximum) && missing(minimum))
    stop("Either a maximum value of NAs or a minimum value of Items must be given")
  
  if(missing(maximum)){
    max <- dim(items)[2]-minimum
  } 
  if(missing(minimum)){
    max <- maximum
  }
  
  df <- items # give new name to dataframe, in order to change it
  if (!is.numeric(df[,1:ncol(df)])) { # coerce to numeric if not numeric
    df <- sapply(df, function(x) as.numeric(x) )
    df <- data.frame(df)
  }
  df[rowSums(is.na(df)) > max,]<-NA # set all lines to NA where the number of NAs is above the maximum of allowed NAs
  
  if(type=="mean") {
    skala <- rowMeans(df, na.rm=T) # compute the meanIndex
  }
  if(type=="sum") {
    skala <- rowSums(df) # compute the sumIndex
  }
  return(skala)
}