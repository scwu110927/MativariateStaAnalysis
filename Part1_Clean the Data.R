Origdata <- read.csv("MultivariateFinalProject/data108.csv", h = T)
#str(Origdata)

# Dummy Data.
Tidydata <- Origdata

# Function of Multichoice Adjustment.
MulAdj <- function(var, n.mui){
  dim.Origdata <- dim(Origdata)
  site <- which(names(Origdata) == paste(tolower(var), "_1", sep = ""))
  result <- index <- matrix(1:n.mui, nrow = dim.Origdata[1], n.mui, byrow = T)
  mui <- Origdata[, site:(site+n.mui-1)]
  for (i in 1:dim.Origdata[1]) {
    result[i, ] <- (index[i, ] %in% mui[i, ]) * 1
  }
  print(paste("Tidydata[, ", site, ":", site+n.mui-1, "] <-", sep = ""))
  return(result)
}

# V7_1~V7_8 Multichoice Adjustment.
dummy <- MulAdj("v7", 8)
Tidydata[, 9:16] <- MulAdj("v7", 8)

# V8 No Internet code 0, and o.w code NA.
Tidydata$v8[Origdata$v8 == 30000] <- NA
Tidydata$v8[Origdata$v8 == 99998] <- NA
Tidydata$v8[Origdata$v8 == 99999] <- 0
#hist(Tidydata$v8)

# V9 No Internet code 0, and o.w code NA.
Tidydata$v9_1[Origdata$v9_2 == 98] <- NA
Tidydata$v9_1[Origdata$v9_2 == 99] <- 0
Tidydata$v9_2[Origdata$v9_2 == 98] <- NA
Tidydata$v9_2[Origdata$v9_2 == 99] <- 0
Tidydata$v9_3 = Tidydata$v9_1*60 + Tidydata$v9_2

# V10_1~V10_12 Multichoice Adjustment.
dummy <- MulAdj("v10", 12)
Tidydata[, 20:31] <- MulAdj("v10", 12)

# V11~V21 code 0 if V10_1~V10_12 == 0, and o.w code NA, and reverse.
site <- which(names(Origdata) == "v11")
for (i in 0:10) {
  Tidydata[Origdata[, site + i] == 8, site + i] <- NA
  Tidydata[Origdata[, site + i] == 9, site + i] <- 6
  Tidydata[, site + i] <- 6 - Tidydata[, site + i]
}

# V22_1~V22_6 Multichoice Adjustment.
dummy <- MulAdj("v22", 6)
Tidydata[, 43:48] <- MulAdj("v22", 6)

# V23 o.w. codes NA, and reverse.
Tidydata$v23[Origdata$v23 > 4] <- NA
Tidydata$v23 <- 5 - Tidydata$v23

# V24 No Internet code 0, and o.w code NA, and code to 1=Yes 0=No. 
Tidydata$v24[Origdata$v24 == 8] <- NA
Tidydata$v24[Origdata$v24 == 9] <- 2
Tidydata$v24 <- - Tidydata$v24 + 2

# V25 o.w. codes NA, and reverse.
Tidydata$v25[Origdata$v25 > 4] <- NA
Tidydata$v25 <- 5 - Tidydata$v25

# V28 o.w. codes NA, and code to 1=Yes 0=No. 
Tidydata$v28[Origdata$v28 == 8] <- NA
Tidydata$v28 <- - Tidydata$v28 + 2

# V29 o.w. codes NA, and code to 1=Yes 0=No. 
Tidydata$v29[Origdata$v29 == 8] <- NA
Tidydata$v29 <- - Tidydata$v29 + 2

# V31 o.w. codes NA. 
Tidydata$v31[Origdata$v31 == 8] <- NA

# V32 o.w. codes NA, and code to 1=Yes 0=No.  
Tidydata$v32[Origdata$v32 == 8] <- NA
Tidydata$v32 <- - Tidydata$v32 + 2

# V33 o.w. codes NA. 
Tidydata$v33[Origdata$v33 == 98] <- NA

# V35 o.w. codes NA. 
Tidydata$v35[Origdata$v35 == 98] <- NA

# V36 o.w. codes NA, and code to 1=Yes 0=No.  
Tidydata$v36[Origdata$v36 == 8] <- NA
Tidydata$v36 <- - Tidydata$v36 + 2

#delete useless variable
drop <- c(paste("v6_", 1:3, sep = ""), "v26", paste("v27_", 1:3, sep = ""), 
          "v30", "v34")
Tidydata <- Tidydata[, !(names(Origdata) %in% drop)]

#Export csv file.
write.csv(Tidydata, file = "MultivariateFinalProject/Tidydata.csv", 
          row.names = FALSE)

