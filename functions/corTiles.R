
corTile <- function(x, ...){
    #creating the correlation matrix
    colClasses <- sapply(x,class)
    colNames <- names(x)
    colSelection <- colNames[colClasses == "numeric" | colClasses == "integer"]
    corMatrix <- cor(x[,sort(colSelection)], ...)
    corMatrix[lower.tri(corMatrix)] <- NA 
    
    #reshaping the matrix for use in plotting
    require(reshape2)
    corMatrix <- melt(corMatrix)
    corMatrix$Var1 <- as.character(corMatrix$Var1)
    corMatrix$Var2 <- as.character(corMatrix$Var2)
    corMatrix <- na.omit(corMatrix)
    
    #creating the plot
    require(ggplot2)
    tilePlot <- ggplot(corMatrix, aes(Var2, Var1)) 
    tilePlot <- tilePlot + geom_tile(data=corMatrix, aes(fill=value),
                                     color="white")
    tilePlot <- tilePlot + scale_fill_gradient2(low="blue",
                            high="red", mid="white",midpoint=0, 
                            limit=c(-1,1), name="Correlation\n(Pearson)")
    tilePlot <- tilePlot + theme(axis.text.x = element_text(angle=45, 
                                vjust=1, size=11, hjust=1))
    tilePlot <- tilePlot + theme(axis.title = element_blank())
    tilePlot <- tilePlot + coord_equal()
    return(tilePlot)
}