# load required libraries
# -----------------------------------------------------------------------------#
library(ggplot2)
# -----------------------------------------------------------------------------#


# functions
# -----------------------------------------------------------------------------#
CountNA <- function(df){
  count <- sapply(df, FUN = function(x) sum(is.na(x)))
  count <- data.frame(variable = names(count), count.na = count,
                         row.names=NULL)
  count <- count[count$count.na>0,]
  index <- order(count$count.na, decreasing=T)
  return(count[index,])
}
# -----------------------------------------------------------------------------#




# Dimensions of dataset - number of observations x variables
# -----------------------------------------------------------------------------#
dimensions <- dim(data)
names(dimensions) <- c("observations","variables")
print(dimensions)
# -----------------------------------------------------------------------------#




# types of variables
# -----------------------------------------------------------------------------#
variables <- sapply(data, class)
numerical.variables <- variables[variables %in% c("integer","numeric")]
categorical.variables <- variables[variables %in% c("factor","character",
                                                    "logical")]
print(numerical.variables)
print(categorical.variables)
# -----------------------------------------------------------------------------#




# Missing Values
# -----------------------------------------------------------------------------#
count.na <- CountNA(data)
print(data)
# -----------------------------------------------------------------------------#




# NA Barchart
# -----------------------------------------------------------------------------#
source("./functions/naBar.R")
naBar(data)
# -----------------------------------------------------------------------------#



