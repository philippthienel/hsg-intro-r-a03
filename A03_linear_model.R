# load required libraries
# -----------------------------------------------------------------------------#
library(ggplot2)
library(reshape2)
# -----------------------------------------------------------------------------#



# functions
# -----------------------------------------------------------------------------#
source("./functions/RMSEPredict.R")
# -----------------------------------------------------------------------------#




# creating dummy variables
# -----------------------------------------------------------------------------#
# create dummy for 'antrieb' == 'allrad' (four-wheel-drive)
data$by4 <- ifelse(data$antrieb == "Allrad", 1, 0)

# create dummy for 'treibstoff' == 'diesel'
data$diesel <- ifelse(data$treibstoff == "Diesel", 1, 0)

# create dummy for 'getriebeart' == 'Automat' (automatic gear shift) 
data$automatic <- ifelse(data$getriebeart == "Automat", 1, 0)
# -----------------------------------------------------------------------------#


# fit linear models
# -----------------------------------------------------------------------------#
# bivariate linear model
formula1 <- 'preis ~ age + kilometer'
fit1 <- lm(formula1 , data)
summary(fit1)$coefficients

# linear model with level effects in age (decreasing)
formula2 <- 'preis ~ sqrt(age) + kilometer'
fit2 <- lm(formula2 ,data)
summary(fit2)$coefficients

# linear model with level effects in age (decreasing) including dummies
formula3 <- 'preis ~ sqrt(age) + kilometer + diesel + by4 + automatic'
fit3 <- lm(formula3, data)
summary(fit3)$coefficients

# linear model maximum fit (with significant coefficients)
formula4 <- 'preis ~ sqrt(age) + kilometer + leistunginps + garantie + 
             leergewicht + diesel + by4 + automatic'
fit4 <- lm(formula4 , data)
summary(fit4)$coefficients
# -----------------------------------------------------------------------------#




# goodness of fit
# -----------------------------------------------------------------------------#
# get r squared for all models
r.sq <- data.frame(name = c("fit1","fit2","fit3","fit4"),
                   r.squared = c(summary(fit1)$r.squared, summary(fit2)$r.squared,
                                 summary(fit3)$r.squared, summary(fit4)$r.squared))
print(r.sq)

# calculate Root Mean Squared Error
## predict 100 times the model using 80% of dataset to train, rest to test
## and calculate the RMSE
RMSE1 <- replicate(300, RMSEPredict(formula1, data, alpha=0.8))
RMSE2 <- replicate(300, RMSEPredict(formula2, data, alpha=0.8))
RMSE3 <- replicate(300, RMSEPredict(formula3, data, alpha=0.8))
RMSE4 <- replicate(300, RMSEPredict(formula4, data, alpha=0.8))

# create data frame
RMSE.compare <- data.frame(RMSE1, RMSE2, RMSE3, RMSE4)
RMSE.compare <- melt(RMSE.compare)

# plot density plot of RMSE
plot <- ggplot(RMSE.compare, aes(x=value, colour=variable))
plot <- plot + geom_density()
plot + labs(title="Density of distribution of Root Mean Squared Error",
            colour="model")
# -----------------------------------------------------------------------------#