install.packages('adabag')
install.packages('randomForest')
library(adabag)
library(randomForest)

## Readind Data
mydata <- read.csv("MultivariateFinalProject/Tidydata.csv", h = T)
keep <- c("v23", "v8", "v9_3", "v37", "v1", "v31", "v32", "v33", "v36", "v24")
Int.addic <- mydata[, (names(mydata) %in% keep)]
# v23: 上網品質
# v8: 上網費用
# v9_3: 上網時間
# v37: 性別
# v1: 年齡
# v31: 教育程度
# v32: 身心障礙
# v33: 居住地區
# v36: 跟家人住
# v24: 網路成癮

## Missing 
miss.pct <- apply(is.na(Int.addic), 2, sum)/1517
Int.addic <- Int.addic[, !(names(Int.addic) == "v23")]
chisq.test(Int.addic$v23, Int.addic$v24)
Int.addic <- na.omit(Int.addic)

## Response table
table(Int.addic$v24)/length(Int.addic$v24)
  
## Dealing with site variable: v33
site <- table(Int.addic$v33, Int.addic$v24)/as.vector(table(Int.addic$v33))
site.diff <- site[, 2]
km <- kmeans(site.diff, 3, 20)
plot(site.diff, col = km$cluster)
points(km$centers, col = 1:3, pch = 8)
km$cluster[order(km$cluster)]
v33_1 <- Int.addic$v33
v33_1[(v33_1 %in% c(4, 11, 18))] <- 101
v33_1[(v33_1 %in% c(1, 2, 8, 9, 10, 14, 17, 19, 21))] <- 102
v33_1[(v33_1 %in% c(3, 5, 6, 7, 12, 13, 15, 16, 20))] <- 103
Int.addic$v33 <- v33_1 - 100
chisq.test(Int.addic$v33, Int.addic$v24)
table(Int.addic$v33)

# Define factor
Int.addic$v24 <- factor(Int.addic$v24)
Int.addic$v23 <- factor(Int.addic$v23)
Int.addic$v37 <- factor(Int.addic$v37)
Int.addic$v1  <- factor(Int.addic$v1)
Int.addic$v31 <- factor(Int.addic$v31)
Int.addic$v32 <- factor(Int.addic$v32)
Int.addic$v33 <- factor(Int.addic$v33)
Int.addic$v36 <- factor(Int.addic$v36)

# Bagging
fit1 <- bagging(v24 ~ ., data = Int.addic,
                control = rpart.control(minsplit = 1, minbucket = 1, cp = 0))
fit1.pred <- predict.bagging(fit1, Int.addic)
fit1.pred$confusion
fit1.pred$error

fit2 <- bagging.cv(v24 ~ ., data = Int.addic, v = 10, mfinal = 20, 
                   control = rpart.control(minsplit = 1, minbucket = 1, cp = 0))
fit2[-1]

# Random Forest
fit3 <- randomForest(v24 ~ ., data = Int.addic, importance = TRUE, 
                     nodesize = 3, proximity = TRUE)
print(fit3)

fit5 <- rfcv(Int.addic[, !(names(Int.addic) == "v24")], Int.addic$v24, 
             cv.fold = 10, step = 0.8)
fit5$error.cv

#Boosting
fit6 <- boosting(v24 ~ ., data = Int.addic, boos = F, mfinal = 20,
                 control = rpart.control(minsplit = 1, minbucket = 1, cp = 0))
fit6.pred <- predict.boosting(fit6, Int.addic)  
fit6.pred$confusion
  
fit7 <- boosting.cv(v24 ~ ., data = Int.addic, v = 10, mfinal = 20, boos = F,
                   control = rpart.control(minsplit = 1, minbucket = 1, cp = 0))
fit7[-1] 

fit8 <- boosting.cv(v24 ~ ., data = Int.addic, v = 10, mfinal = 20, boos = T,
                    control = rpart.control(minsplit = 1, minbucket = 1, cp = 0))
fit8[-1] 





