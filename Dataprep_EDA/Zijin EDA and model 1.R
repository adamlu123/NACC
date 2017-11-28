library(haven)
setwd("~/Desktop/Zijin/Stats 275/Project")
data <- read_sas("grill11162017.sas7bdat")
covariates <- read.csv("covariates.csv")
covariates <- colnames(covariates)[2:17]
covariates <- append(covariates, colnames(data)[135:158])


## 1. Use probAD = 1
data <- data[data["PROBAD"] == 1,]

## 2. Delete the INRACE = -4
data <- data[data[, "INRACE"] != -4, ]

## 3. select variable
data <- data[, covariates]

## 4. Remove NPI with -4, change 8 to 0, 9 to NA
data[, 17:40][data[,17:40] == -4] <- NA
data[, 17:40][data[, 17:40] == 8] <- 0
data[, 17:40][data[,17:40] == 9] <- NA
data <- na.omit(data)
## 5. 1st visit
data <- data[data[, "NACCVNUM"] == 1, ]

## 6. calculate the sum of NPI
npi_sum <- rowSums(data[, seq(18, 40, 2)])
data <- cbind(data, npi_sum)






##sum(data[data["NACCALZD"] != 8,"NACCALZD"]) ## AD:28687
##sum(data[data["PROBAD"] != 8,"PROBAD"]) ## Prob AD: 26320
##sum(data[data["POSSAD"] != 8,"POSSAD"]) ## Poss AD: 2367

##asian <- probAD[probAD[,"INRACE"] == 5,]
##non_asian <- probAD[probAD[, "INRACE"] != 5, ]

##asian <- asian[, c(colnames(covariates))]
##non_asian <- non_asian[, c(colnames(covariates))]

##################### linear model 1, add all the NPI  ############################
fit1 <- lm(npi_sum ~ 1 + as.factor(INRACE == 5) + as.factor(INRELTO == 1) + as.factor(INRELTO == 2)
           + NACCMMSE, data = data)
summary(fit1)




