set.seed(2018)

library(XML)
library(lubridate)
library(timeDate)
library(tis)
library(RMySQL)
library(dbConnect)
library(httr)
library(ggplot2)
library(dplyr)
library(stringr)
library(caret)

Sys.setenv(TZ='EST')
setwd("~/Desktop/test")
source("normalizeVar.R")

#####NOTE IT IS EITHER DATABASE OR CSV FILE
## read database file
# open database connection
# db1 <- dbConnect(MySQL(), user= "root", host = "localhost", db = "dbRates", password = "Newyork@1996")
# historicalRates <- dbReadTable(db1, name = "rates")
# dbDisconnect(db1)

## read csv file
historicalRates <- read.csv("historicalRates.csv", stringsAsFactors = FALSE)


numberRows <- nrow(historicalRates)
historicalRates$date <- as.Date(historicalRates$date)


## read other files
dealerOffers <- read.csv("dealerOffers.csv", stringsAsFactors = FALSE)
stateCode <- read.csv("State_Code.csv", stringsAsFactors = FALSE)

# # # only looking at certain files
dealerOffers <- dealerOffers %>% filter(CPN >= 4)

# need to process date fields
dealerOffers$mty <- as.Date(dealerOffers$mty)
dealerOffers$nxtCall <- as.Date(dealerOffers$nxtCall)
dealerOffers$date <- as.Date(dealerOffers$date)

# add years to maturity and next call based of date 
dealerOffers <- dealerOffers %>% mutate(yearsToMat = as.numeric(mty - date)/365)
dealerOffers <- dealerOffers %>% mutate(yearsToNxtCall = as.numeric(nxtCall - date)/365)

# It is assumed that bonds with no next call are noncallable
dealerOffers$yearsToNxtCall[which(is.na(dealerOffers$yearsToNxtCall))] <- 0

# This model will only look at bonds that are longer than 2 years to maturity
dealerOffers <- dealerOffers %>% filter(yearsToMat > 2)

# for simplicity the model excludes calls less than 8 years 
# future enhancements can included better test to remove kicker bonds.
dealerOffers <- dealerOffers %>% filter(!yearsToNxtCall < 8 | yearsToNxtCall == 0)


# There is some data that has incorrect yield; filtering out all yield > 8%
dealerOffers <- dealerOffers %>% filter(askYTC < 8)

# change ratings to numeric value so Aaa = 1, Aa1 = 2, Aa2 = 3 etc
# Note there is no D rating for Moodys -- 
# So S&P rating scale needs to be adjusted to have 21 (C&D are same)
moodysDF <- data.frame(rating = c("Aaa", "Aa1", "Aa2", "Aa3",
                                  "A1", "A2", "A3",
                                  "Baa1", "Baa2", "Baa3",
                                  "Ba1", "Ba2", "Ba3",
                                  "B1", "B2", "B3",
                                  "Caa1", "Caa2", "Caa3",
                                  "Ca", "C"), 
                       ratingValue = c(1:21))

spDF <- data.frame(rating = c("AAA", "AA+", "AA", "AA-",
                              "A+", "A", "A-",
                              "BBB+", "BBB", "BBB-",
                              "BB+", "BB", "BB-",
                              "B+", "B", "B-",
                              "CCC+", "CCC", "CCC-",
                              "CC", "C","D"), 
                   ratingValue = c(1:21,21))

# Null ratings
nullRatings <- c("N.A.", "NA", "NR")

# Change nullRatings to NA 
x <- which(dealerOffers$moody %in% nullRatings)
dealerOffers$moody[x] <- NA
x <- which(dealerOffers$sp %in% nullRatings)
dealerOffers$sp[x] <- NA

# # We are going to operate on model that must have moodys rating
# dealerOffers <- dealerOffers %>% filter(!is.na(moody))

# provide a number for each rating
dealerOffers <- dealerOffers %>% mutate(moodyNum = NA, spNum = NA, combinedRatingNum = NA,spreadToAAAi = NA)

for (i in 1:nrow(dealerOffers)) {
    if (dealerOffers$moody[i] %in% moodysDF$rating) {
        x <- which(moodysDF$rating == dealerOffers$moody[i])
        dealerOffers$moodyNum[i] <- moodysDF$ratingValue[x]
    }
    if (dealerOffers$sp[i] %in% spDF$rating) {
        y <- which(spDF$rating == dealerOffers$sp[i])
        dealerOffers$spNum[i] <- spDF$ratingValue[y]
    }
    # Get a combined rating if both are available -- otherwise use moodys rating
    if (!is.na(dealerOffers$moodyNum[i]) & !is.na(dealerOffers$spNum[i]))  {
        dealerOffers$combinedRatingNum[i] <- (dealerOffers$moodyNum[i] + dealerOffers$spNum[i])/2
    } else {
        dealerOffers$combinedRatingNum[i] <- dealerOffers$moodyNum[i]
    }
    
    # the rate is end of day -- the offer sheets are usually beginning of day
    # taking previous day AAA
    
    lookupDate <- as.Date(previousBusinessDay(dealerOffers$date[i], holidays = holidays(year(dealerOffers$date[1]))))
    if (lookupDate %in% historicalRates$date) {
        z <- which(historicalRates$date == lookupDate)
        # take first element because data has some duplicate entries
        z <- z[1]
        ## Calculate spline curve from current yield curve
        muniCurve <- spline(c(1,2,5,10,30), 
                            c(historicalRates$muniYield1Y[z], 
                              historicalRates$muniYield2Y[z], 
                              historicalRates$muniYield5Y[z], 
                              historicalRates$muniYield10Y[z], 
                              historicalRates$muniYield30Y[z]), 
                            n=30, method = "natural")
        muniCurve <- as.data.frame(muniCurve)
        names(muniCurve) <- c("Maturity", "AAA_Yield")
        muniCurve$AAA_Yield <- round(muniCurve$AAA_Yield, digits = 2)
        findAAA <- dealerOffers$yearsToMat[i]
        if (floor(findAAA) >= 30) {
            findAAA <- 30
        }
        floorAAA <- muniCurve$AAA_Yield[floor(findAAA)]
        if (findAAA %% 1 > 0) {
            ceilingAAA <- muniCurve$AAA_Yield[ceiling(findAAA)]
            interpolatedAAA <- ((findAAA %% 1) * (ceilingAAA - floorAAA)) + floorAAA
        } else {
            interpolatedAAA <- floorAAA
        }
        dealerOffers$spreadToAAAi[i] <- dealerOffers$askYTC[i] - interpolatedAAA
    }
}

# some cleanup
dealerOffers$spreadToAAAi <- round(dealerOffers$spreadToAAAi,2) * 100
dealerOffers$yearsToMat <- round(dealerOffers$yearsToMat, 2)
dealerOffers$yearsToNxtCall <- round(dealerOffers$yearsToNxtCall, 2)

# drop records for which we cant calc a spread (possible no AAA for that day)
dealerOffers <- dealerOffers %>% filter(!is.na(spreadToAAAi))

# create a flag that catches GO, Airports, and Hospitals
x <- grep("[A-Z]+ ST$", str_trim(dealerOffers$issuer))
dealerOffers <- dealerOffers %>% mutate(goFlag = 0)
dealerOffers$goFlag[x] <- 1

x <- grep(" ARPT | ARPTS | AIRPORT | AIRPORTS | ARPTSR", str_trim(dealerOffers$issuer))
dealerOffers <- dealerOffers %>% mutate(airportFlag = 0)
dealerOffers$airportFlag[x] <- 1

x <- grep(" HLT | HLTHS | HEALTH | HOSP | HOSPITAL", str_trim(dealerOffers$issuer))
dealerOffers <- dealerOffers %>% mutate(hospitalFlag = 0)
dealerOffers$hospitalFlag[x] <- 1

x <- grep("PWR| PWRS | POWER | POWERS", str_trim(dealerOffers$issuer))
dealerOffers <- dealerOffers %>% mutate(powerFlag = 0)
dealerOffers$powerFlag[x] <- 1


pData <- dealerOffers[,-c(1,2,3,6,7,8,9,10,11,12,13,18)]

dummy.vars <- dummyVars(~., data = pData)
train.dummy <- predict(dummy.vars, pData)

pre.process <- preProcess(train.dummy, method = "bagImpute")
imputed.data <- predict(pre.process, train.dummy)

imputed.data <- as.data.frame(imputed.data)
dealerOffers$combinedRatingNum <- (imputed.data$moodyNum + imputed.data$spNum)/2
dealerOffers <- dealerOffers[,-c(1,16,17)]

# process state
dealerOffers <- dealerOffers %>% mutate(stateNum = NA)
for (i in 1:nrow(stateCode)) {
  x <- which(dealerOffers$state == stateCode$Abbreviation[i])
  dealerOffers$stateNum[x] <- stateCode$State_Code[i]
}

x <- nrow(dealerOffers)
y <- x * .80
training <- dealerOffers[1:y,]
testing <- dealerOffers[(y+1):x,]


##########################################################################
# Prediction section
##########################################################################

train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 5,
                              search = "grid")


modelFit <- train(spreadToAAAi ~ stateNum + CPN + combinedRatingNum + yearsToMat + yearsToNxtCall + 
                    goFlag + hospitalFlag + airportFlag + powerFlag,
                   data = training,
                   preProcess = c("center", "scale"),
                   method = "xgbTree",
                   trControl = train.control)


predictions <- predict(modelFit, newdata = testing)
predictions

results <- data.frame(actual = testing$spreadToAAAi, estimate = predictions)
results <- results %>% mutate(diff = actual - estimate)
results$estimate <- round(results$estimate, 1)
results$diff <- round(results$diff, 1)
mean(results$diff)
median(results$diff)
max(results$diff)
min(results$diff)
plot(results$diff)
sum(results$diff < 20 & results$diff > -20)/nrow(results)
sum(results$diff < 10 & results$diff > -10)/nrow(results)
sum(results$diff < 5 & results$diff > -5)/nrow(results)

testing <- cbind(testing, results)