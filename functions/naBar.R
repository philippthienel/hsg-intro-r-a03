naBar <- function(x, exclude = TRUE, compare = FALSE){
    
    #count NAs per variable and stroe in named vector
    vectorNAs <- apply(x, 2, FUN = function(y) sum(is.na(y)))
    
    #cast as dataframe
    NAs <- as.data.frame(vectorNAs)
    NAs <- cbind(row.names(NAs),NAs)
    row.names(NAs) <- NULL
    names(NAs) <-c("variables","numNAs")
    
    #exclude  variables with numNAs = 0, is TRUE by default
    if(exclude == TRUE){
      indexNAs <- NAs$numNAs > 0
      NAs <- NAs[indexNAs,]
    }
    
    #sort factor levels of variables to order barplot by
    NAs <- within(NAs, variables <- factor(
      variables, levels = variables[order(numNAs, decreasing = TRUE)]))
    
    require(ggplot2)
    plotNA <- ggplot(data = NAs, aes(x = variables, y = numNAs))
    plotNA <- plotNA + geom_bar(stat = "identity", alpha = 0.6, fill = "red")
    plotNA <- plotNA + theme(axis.text.x=element_text(angle=45, hjust = 1))
    
    if(compare == TRUE){
      plotNA <- plotNA + geom_hline(yintercept = nrow(x), linetype = "dashed")
    }
    plotNA
}