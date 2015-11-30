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


# preis
# -----------------------------------------------------------------------------#
# numerical distribution measures
summary(data$preis)

# graphical distribution
plot <- ggplot(data, aes(x=preis))
plot <- plot + geom_histogram(binwidth=1000, fill="red", alpha=0.6, color="black")
plot

# dependency of 'preis' on 'age'
plot <- ggplot(data, aes(x=preis, y=age, colour = age)) + geom_point()
plot

# add binary variable 'young' if age < 60 months (5years)
data$young <- ifelse(data$age <60, 1, 0)

# graphical distribution differenciated by 'young'
plot <- ggplot(data, aes(x=preis, fill=as.factor(young)))
plot <- plot + geom_histogram(binwidth=1000, alpha=0.6, colour="black")
plot + scale_fill_manual(values=c("red","blue"))
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
# select relevant categorical variables (logical & factor)
categoric.variables <- c('fahrzeugart', 'aussenfarbe', 'getriebeart', 'antrieb',
                         'treibstoff', 'tueren', 'sitze', 'zylinder',
                         'plattform', 'abmfk')


# frequency tables
with(data, table(fahrzeugart))
with(data, table(aussenfarbe))
with(data, table(getriebeart))
with(data, table(antrieb))
with(data, table(treibstoff))
with(data, table(tueren))
with(data, table(sitze))
with(data, table(zylinder))
with(data, table(plattform))
with(data, table(abmfk))
# -----------------------------------------------------------------------------#




library(dplyr)
df <- filter(data, !(plattform %in% c("LT","Multivan","VAR","Variant")))
df$plattform <- factor(df$plattform)

plt <- ggplot(df, aes(x=preis)) + geom_histogram() + facet_grid(.~plattform)
plt

plt <- ggplot(df, aes(x=kilometer)) + geom_histogram() + facet_grid(.~plattform)
plt

plt <- ggplot(df, aes(x=age)) + geom_histogram() + facet_grid(.~plattform)
plt



