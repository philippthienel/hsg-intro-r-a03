# load required libraries
# -----------------------------------------------------------------------------#
library(stringr)
library(reshape2)
# -----------------------------------------------------------------------------#

# functions
# -----------------------------------------------------------------------------#
GetValue <- function(x) {
  require(stringr)
  x <- gsub("'","",x)
  x <- str_extract(x, '[0-9]+\\.*[0-9]*')
  return(as.numeric(x))
}

GetAge <- function(x, month=7, year=2011) {
  require(reshape2)
  df <- colsplit(x,"-",names=c("month","year"))
  age <- (year - df$year)*12 + (month - df$month)
  return(age)
}

GetPlattform <- function(x) {
  require(stringr)
  x <- gsub('VW','',x)
  plattform <- str_extract(x, '[a-zA-Z]+')
  return(plattform)
}
# -----------------------------------------------------------------------------#




# -----------------------------------------------------------------------------#
# 1. read data
# -----------------------------------------------------------------------------#
path.data <- './data/vw_station_wagons.csv'
data <- read.csv(path.data, sep=',')
# -----------------------------------------------------------------------------#




# -----------------------------------------------------------------------------#
# 2. clean data
# -----------------------------------------------------------------------------#

# extract values (strip units)
# -----------------------------------------------------------------------------#
data <- within(data,{
  kilometer <- GetValue(kilometer)
  leergewicht <- GetValue(leergewicht)
  verbrauch <- GetValue(verbrauch)
  co2.emission <- GetValue(co2.emission)
  garantie <- GetValue(garantie)
  preis <- GetValue(preis)
})
# -----------------------------------------------------------------------------#

# cast as factor
# -----------------------------------------------------------------------------#
data <- within(data,{
  tueren <- as.factor(tueren)
  sitze <- as.factor(sitze)
  zylinder <- as.factor(zylinder)
})
# -----------------------------------------------------------------------------#




# -----------------------------------------------------------------------------#
# 3. add features to data
# -----------------------------------------------------------------------------#

# add age in months
data$age <- GetAge(data$inverkehrssetzung)

# add plattform variable
data$plattform <- as.factor(GetPlattform(data$modell))

# add hubraum in liters
data$hubraum.liter <- round(data$hubraum/1000,1)
# -----------------------------------------------------------------------------#

