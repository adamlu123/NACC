library(haven)
setwd("~/Desktop/Zijin/Stats 275/Project")
data <- read_sas("grill11162017.sas7bdat")
covariates <- read.csv("covariates.csv")

covariates <- colnames(covariates)[2:17]
npi_cov <- colnames(data)[135:158]

### 1.  Identify subjects with consistent diagnosis of PROBAD and DEMENTED
data$NONDECREASE <- 0
data$NONDECREASE.AD <- 0
id.index = unique(data$NACCID)

for(i in id.index){
  df.temp = data[data$NACCID == i,]
  mlength = dim(df.temp)[1]
  
  # Flag for DEMENTED Consistency
  if(mlength==1){if(df.temp$DEMENTED==0){data$NONDECREASE[data$NACCID==i]=1}}
  
  if(mlength>1){
    for(j in 2:mlength){
      if(df.temp$DEMENTED[j] < df.temp$DEMENTED[j-1]) data$NONDECREASE[data$NACCID == i] = 1
    }
  }
  
  # Flag for ProbAD Consistency
  if(mlength==1){if(df.temp$PROBAD==0){data$NONDECREASE.AD[data$NACCID==i]=1}}
  
  if(mlength>1){
    for(j in 2:mlength){
      if(df.temp$PROBAD[j] < df.temp$PROBAD[j-1]) data$NONDECREASE.AD[data$NACCID == i] = 1
    }
  }
}

### 2.  Convert NPI scores of 8s to 0
data[,133:158][data[,133:158] == 8] <- 0

### 3.  Convert NPI scores of -4s and 9s to NA
data[,133:158][data[,133:158] == 9] <- NA
data[,133:158][data[,133:158] == -4] <- NA

# data <- na.omit(data)

### 4.  Define total NPI score (sum)
npi_score <- rowSums(data[, seq(136, 158, 2)])
summary(npi_score)
data$NPITOTALSEV <- npi_score

### 5.  Convert NACCMMSE greater than 30 to NA
data[,"NACCMMSE"][data[,"NACCMMSE"] > 30] <- NA
data[,"NACCMMSE"][data[,"NACCMMSE"] == -4] <- NA

### 6.  Convert INEDUC greater than 20 to NA
data[,"INEDUC"][data[,"INEDUC"] > 20] <- NA
data[,"INEDUC"][data[,"INEDUC"] == -4] <- NA

data[,"INRELTO"][data[,"INRELTO"] == -4] <- NA
data[,"INRACE"][data[,"INRACE"] == -4] <- NA

# data <- na.omit(data)

### 7.  Define age of patient using VISITYR - BIRTHYR
data[,"BIRTHYR"][data[,"BIRTHYR"] == 9999] <- NA
data[,"INBIRYR"][data[,"INBIRYR"] == 9999] <- NA
# data <- na.omit(data)

### 8.  Define age of informant using VISITYR - INBIRYR
data$AGE <- data$VISITYR - data$BIRTHYR
data$INFAGE <- data$VISITYR - data$INBIRYR

data[,"INHISP"][data[,"INHISP"] == 9] <- NA
data[,"INRACE"][data[,"INRACE"] == 99] <- NA

### 9.  Create subset with:

data1 <- data[, c("NACCID", "NACCVNUM",
"AGE", "SEX",
"DEMENTED", "PROBAD",
"NPITOTALSEV", "NACCMMSE",
"INFAGE", "INRELTO", "INRACE", "INEDUC", "INHISP",
"NONDECREASE", "NONDECREASE.AD")]

### 10.  Remove NA from (9) subset data
data1 <- na.omit(data1)

### 11.  Select subjects who are consistent from (9) subset data (NONDECREASE==0 & NONDECREASE.AD==0)
data1 <- data1[data1$NONDECREASE == 0 & data1$NONDECREASE.AD == 0,]

### 12.  Select visits with PROBAD and DEMENTED diagnosis (PROBAD==1 & DEMENTED==1)
data1 <- data1[data1$PROBAD == 1 & data1$DEMENTED == 1,]
write.csv(data1, "sub-sample.csv")


