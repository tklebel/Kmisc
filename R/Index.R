#' Index
#'
#' This function computes a mean-index or a sum-index for use in social sciences
#' @param items The items you want to compute the index from. Has to be a data.frame.
#' @param type mean will give you a mean-index, sum a sum-index
#' @param maximum the maximum of allowed missings in a row
#' @param minimum the minimum of non-missings in a row for which to compute the scale
#' @keywords mean-index
#' @export
#' @examples
#' # generate data
#' set.seed(1234)
#' item1 <- rnorm(10, 2.5, .5)
#' item1[sample.int(length(item1), 2)] <- NA_real_
#' item2 <- rnorm(10, 3, .5)
#' item2[sample.int(length(item2), 3)] <- NA_real_
#' item3 <- rnorm(10, 2.2, .5)
#' item4 <- rnorm(10, 2, .5)
#' df1 <- data.frame(item1, item2, item3, item4)
#' df1
#' # compute different Indizes
#' Index(df1, type="mean", min=3)
#' Index(df1, type="mean", min=2)
#' Index(df1, type="sum", max=0)
 


Index <- function(items, type, max, min) {
  if(!is.data.frame(items))
    stop("Data must be a data frame")
  if(missing(max) && missing(min)) {
    m <- 0
    print("minimum value of items was set to 0")
  }
  else if(missing(max)){
    m <- dim(items)[2]-min
  } 
  else {m <- max}
  
  df <- items # give new name to dataframe, in order to change it
  if (!is.numeric(df[,1:ncol(df)])) { # coerce to numeric if not numeric
    df <- sapply(df, function(x) as.numeric(x) )
    df <- data.frame(df)
  }
  df[rowSums(is.na(df)) > m,]<-NA # set all lines to NA where the number of NAs is above the maximum of allowed NAs
  
  
  switch(type,
         mean = {
           skala <- rowMeans(df, na.rm=T) # compute the meanIndex
         },
         sum = {
           skala <- rowSums(df) # compute the meanIndex
         })
  return(skala)
}