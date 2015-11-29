# load required libraries
# -----------------------------------------------------------------------------#
library(ggplot2)
library(reshape2)
# -----------------------------------------------------------------------------#



# functions
# -----------------------------------------------------------------------------#
ReScaling <- function(x){
  x.min <- min(x, na.rm = TRUE)
  x.max <- max(x, na.rm = TRUE)  
  y <- (x - x.min)/(x.max - x.min)
  return(y)
}
# -----------------------------------------------------------------------------#



# numeric covariates - dispersion
# -----------------------------------------------------------------------------#
# select numeric covariates
numeric.covariates <- names(data)[sapply(data, class) %in% c("integer","numeric")]

#rescale numeric covariates
dataScaled <- data.frame(sapply(data[,numeric.covariates],FUN=ReScaling))

# plot distribution of rescales variables to compare
dataScaledMelt <- melt(dataScaled)
plt <- ggplot(data=dataScaledMelt, aes(x=variable, y= value))
plt + geom_boxplot(fill="red", alpha=0.6) + coord_flip()
# -----------------------------------------------------------------------------#



# numeric covariates - correlation
# -----------------------------------------------------------------------------#
# create correlation matrix
cor.matrix <- cor(data[,numeric.covariates], use="pairwise.complete.obs")
print(cor.matrix)

# create correlation tiles plot
source("./functions/corTiles.R")
corTile(data[,numeric.covariates], use="pairwise.complete.obs")
# -----------------------------------------------------------------------------#




# categorical / factor variables
# -----------------------------------------------------------------------------#


# -----------------------------------------------------------------------------#








ggplot(data, aes(x=age,y=preis)) + geom_point() + geom_smooth(method="lm", se=FALSE, fullrange=TRUE)
plt <- ggplot(data, aes(x=age,y=preis, colour=is.na(energieeffizienz))) + geom_point(alpha=0.5)
plt
plt + geom_smooth(method="lm", se=FALSE, fullrange=TRUE)


df <- filter(data, !(plattform %in% c("LT","Multivan","VAR","Variant")))
df$plattform <- factor(df$plattform)

plt <- ggplot(df, aes(x=preis)) + geom_histogram() + facet_grid(.~plattform)
plt

plt <- ggplot(df, aes(x=kilometer)) + geom_histogram() + facet_grid(.~plattform)
plt

plt <- ggplot(df, aes(x=age)) + geom_histogram() + facet_grid(.~plattform)
plt

plt <- ggplot(df, aes(x=preis, fill = as.factor(round(age/12)))) + geom_histogram(alpha=0.7)
plt





index <- with(df, which(energieeffizienz %in% c("A","B","C","D") & age<=36))
fit <- lm(preis ~ kilometer + log(age/12+1) + dummyEnergie, df)



