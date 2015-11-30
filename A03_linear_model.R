# load required libraries
# -----------------------------------------------------------------------------#


# -----------------------------------------------------------------------------#



# functions
# -----------------------------------------------------------------------------#

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
fit1 <- lm(preis ~ age + kilometer, data)
summary(fit1)
summary(fit1)$coefficients

# linear model with level effects in age (decreasing)
fit2 <- lm(preis ~ sqrt(age) + kilometer ,data)
summary(fit2)
summary(fit2)$coefficients

# linear model with level effects in age (decreasing) including dummies
fit3 <- lm(preis ~ sqrt(age) + kilometer + diesel + by4 + automatic , data)
summary(fit3)
summary(fit3)$coefficients

# linear model maximum fit (with significant coefficients)
fit4 <- lm(preis ~ sqrt(age) + kilometer + leistunginps + garantie + leergewicht + diesel + by4 + automatic , data)
summary(fit4)
summary(fit4)$coefficients
# -----------------------------------------------------------------------------#

# goodness of fit
# -----------------------------------------------------------------------------#
