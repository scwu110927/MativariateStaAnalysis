#### Multiple Correspondence Analysis (MCA)
library(ca)
library(ggplot2)
library(dplyr)
library(kableExtra)
library(showtext)
showtext.auto(enable = TRUE)

### prepare the data for conducting MCA
Tidy.MCA.data <- Tidydata[, c(1, 29:39, 51, 53, 56)]

## calculate the NA proportion in each variable
NA.ratio <- NULL
for (i in 1:15) {
  temp = sum(is.na(Tidy.MCA.data[, i]))/1517
  NA.ratio = round(c(NA.ratio, temp), 4)
}
NA.ratio
  
## delete all the observations with NA
tidy <- na.omit(Tidy.MCA.data)

## turn the levels into factors
tidy$v1 <- as.factor(tidy$v1)
tidy$v11 <- as.factor(tidy$v11)
tidy$v12 <- as.factor(tidy$v12)
tidy$v13 <- as.factor(tidy$v13)
tidy$v14 <- as.factor(tidy$v14)
tidy$v15 <- as.factor(tidy$v15)
tidy$v16 <- as.factor(tidy$v16)
tidy$v17 <- as.factor(tidy$v17)
tidy$v18 <- as.factor(tidy$v18)
tidy$v19 <- as.factor(tidy$v19)
tidy$v20 <- as.factor(tidy$v20)
tidy$v21 <- as.factor(tidy$v21)
tidy$v31 <- as.factor(tidy$v31)
tidy$v33 <- as.factor(tidy$v33)
tidy$v37 <- as.factor(tidy$v37)

### data exploration : draw bar charts for each variable

## v1 : 年齡
ggplot(data = tidy, aes(x = v1, fill = v1)) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count") +
  scale_fill_discrete(name = "年齡",
                      breaks = c(1, 2, 3, 4, 5, 6, 7, 8),
                      labels = c("12-14歲", "15-19歲", "20-29歲", "30-39歲",
                                 "40-49歲", "50-59歲", "60-64歲", "65歲以上")) +
  scale_x_discrete(labels = c("12-14歲", "15-19歲", "20-29歲", "30-39歲",
                             "40-49歲", "50-59歲", "60-64歲", "65歲以上")) +
  theme_bw() +
  xlab("") +
  ylab("人數") +
  ggtitle("年齡") +
  theme(plot.title = element_text(hjust = 0.5))

## v11 : 影音服務
ggplot(data = tidy, aes(x = v11, fill = v11)) +
  geom_bar(width = 0.7) +
  geom_text(aes(label = ..count..), stat = "count") +
  scale_fill_discrete(name = "使用頻率",
                      breaks = c(0, 1, 2, 3, 4, 5),
                      labels = c("從不", "一個月不到一次", "一個月至少一次",
                                 "一週至少一次", "每天一次", "一天好幾次")) +
  scale_x_discrete(labels = c("從不", "一個月不到一次", "一個月至少一次",
                             "一週至少一次", "每天一次", "一天好幾次")) +
  theme_bw() +
  xlab("") +
  ylab("人數") +
  ggtitle("影音服務") +
  theme(plot.title = element_text(hjust = 0.5))

## v12 : 社群
ggplot(data = tidy, aes(x = v12, fill = v12)) +
  geom_bar(width = 0.7) +
  geom_text(aes(label = ..count..), stat = "count") +
  scale_fill_discrete(name = "使用頻率",
                      breaks = c(0, 1, 2, 3, 4, 5),
                      labels = c("從不", "一個月不到一次", "一個月至少一次",
                                 "一週至少一次", "每天一次", "一天好幾次")) +
  scale_x_discrete(labels = c("從不", "一個月不到一次", "一個月至少一次",
                             "一週至少一次", "每天一次", "一天好幾次")) +
  theme_bw() +
  xlab("") +
  ylab("人數") +
  ggtitle("社群服務") +
  theme(plot.title = element_text(hjust = 0.5))

## v13 : 資訊
ggplot(data = tidy, aes(x = v13, fill = v13)) +
  geom_bar(width = 0.7) +
  geom_text(aes(label = ..count..), stat = "count") +
  scale_fill_discrete(name = "使用頻率",
                      breaks = c(0, 1, 2, 3, 4, 5),
                      labels = c("從不", "一個月不到一次", "一個月至少一次",
                                 "一週至少一次", "每天一次", "一天好幾次")) +
  scale_x_discrete(labels = c("從不", "一個月不到一次", "一個月至少一次",
                             "一週至少一次", "每天一次", "一天好幾次")) +
  theme_bw() +
  xlab("") +
  ylab("人數") +
  ggtitle("資訊服務") +
  theme(plot.title = element_text(hjust = 0.5))

## v14 : 娛樂
ggplot(data = tidy, aes(x = v14, fill = v14)) +
  geom_bar(width = 0.7) +
  geom_text(aes(label = ..count..), stat = "count") +
  scale_fill_discrete(name = "使用頻率",
                      breaks = c(0, 1, 2, 3, 4, 5),
                      labels = c("從不", "一個月不到一次", "一個月至少一次",
                                 "一週至少一次", "每天一次", "一天好幾次")) +
  scale_x_discrete(labels = c("從不", "一個月不到一次", "一個月至少一次",
                             "一週至少一次", "每天一次", "一天好幾次")) +
  theme_bw() +
  xlab("") +
  ylab("人數") +
  ggtitle("娛樂服務") +
  theme(plot.title = element_text(hjust = 0.5))

## v15 : 購物
ggplot(data = tidy, aes(x = v15, fill = v15)) +
  geom_bar(width = 0.7) +
  geom_text(aes(label = ..count..), stat = "count") +
  scale_fill_discrete(name = "使用頻率",
                      breaks = c(0, 1, 2, 3, 4, 5),
                      labels = c("從不", "一個月不到一次", "一個月至少一次",
                                 "一週至少一次", "每天一次", "一天好幾次")) +
  scale_x_discrete(labels = c("從不", "一個月不到一次", "一個月至少一次",
                             "一週至少一次", "每天一次", "一天好幾次")) +
  theme_bw() +
  xlab("") +
  ylab("人數") +
  ggtitle("購物服務") +
  theme(plot.title = element_text(hjust = 0.5))

## v16 : 金融
ggplot(data = tidy, aes(x = v16, fill = v16)) +
  geom_bar(width = 0.7) +
  geom_text(aes(label = ..count..), stat = "count") +
  scale_fill_discrete(name = "使用頻率",
                      breaks = c(0, 1, 2, 3, 4, 5),
                      labels = c("從不", "一個月不到一次", "一個月至少一次",
                                 "一週至少一次", "每天一次", "一天好幾次")) +
  scale_x_discrete(labels = c("從不", "一個月不到一次", "一個月至少一次",
                             "一週至少一次", "每天一次", "一天好幾次")) +
  theme_bw() +
  xlab("") +
  ylab("人數") +
  ggtitle("金融服務") +
  theme(plot.title = element_text(hjust = 0.5))

## v17 : 學習
ggplot(data = tidy, aes(x = v17, fill = v17)) +
  geom_bar(width = 0.7) +
  geom_text(aes(label = ..count..), stat = "count") +
  scale_fill_discrete(name = "使用頻率",
                      breaks = c(0, 1, 2, 3, 4, 5),
                      labels = c("從不", "一個月不到一次", "一個月至少一次",
                                 "一週至少一次", "每天一次", "一天好幾次")) +
  scale_x_discrete(labels = c("從不", "一個月不到一次", "一個月至少一次",
                             "一週至少一次", "每天一次", "一天好幾次")) +
  theme_bw() +
  xlab("") +
  ylab("人數") +
  ggtitle("學習服務") +
  theme(plot.title = element_text(hjust = 0.5))

## v18 : 健康
ggplot(data = tidy, aes(x = v18, fill = v18)) +
  geom_bar(width = 0.7) +
  geom_text(aes(label = ..count..), stat = "count") +
  scale_fill_discrete(name = "使用頻率",
                      breaks = c(0, 1, 2, 3, 4, 5),
                      labels = c("從不", "一個月不到一次", "一個月至少一次",
                                 "一週至少一次", "每天一次", "一天好幾次")) +
  scale_x_discrete(labels = c("從不", "一個月不到一次", "一個月至少一次",
                             "一週至少一次", "每天一次", "一天好幾次")) +
  theme_bw() +
  xlab("") +
  ylab("人數") +
  ggtitle("健康服務") +
  theme(plot.title = element_text(hjust = 0.5))

## v19 : 政府
ggplot(data = tidy, aes(x = v19, fill = v19)) +
  geom_bar(width = 0.7) +
  geom_text(aes(label = ..count..), stat = "count") +
  scale_fill_discrete(name = "使用頻率",
                      breaks = c(0, 1, 2, 3, 4, 5),
                      labels = c("從不", "一個月不到一次", "一個月至少一次",
                                 "一週至少一次", "每天一次", "一天好幾次")) +
  scale_x_discrete(labels = c("從不", "一個月不到一次", "一個月至少一次",
                             "一週至少一次", "每天一次", "一天好幾次")) +
  theme_bw() +
  xlab("") +
  ylab("人數") +
  ggtitle("政府服務") +
  theme(plot.title = element_text(hjust = 0.5))

## v20 : 工具
ggplot(data = tidy, aes(x = v20, fill = v20)) +
  geom_bar(width = 0.7) +
  geom_text(aes(label = ..count..), stat = "count") +
  scale_fill_discrete(name = "使用頻率",
                      breaks = c(0, 1, 2, 3, 4, 5),
                      labels = c("從不", "一個月不到一次", "一個月至少一次",
                                 "一週至少一次", "每天一次", "一天好幾次")) +
  scale_x_discrete(labels = c("從不", "一個月不到一次", "一個月至少一次",
                             "一週至少一次", "每天一次", "一天好幾次")) +
  theme_bw() +
  xlab("") +
  ylab("人數") +
  ggtitle("工具服務") +
  theme(plot.title = element_text(hjust = 0.5))

## v21 : 行銷
ggplot(data = tidy, aes(x = v21, fill = v21)) +
  geom_bar(width = 0.7) +
  geom_text(aes(label = ..count..), stat = "count") +
  scale_fill_discrete(name = "使用頻率",
                      breaks = c(0, 1, 2, 3, 4, 5),
                      labels = c("從不", "一個月不到一次", "一個月至少一次",
                                 "一週至少一次", "每天一次", "一天好幾次")) +
  scale_x_discrete(labels = c("從不", "一個月不到一次", "一個月至少一次",
                             "一週至少一次", "每天一次", "一天好幾次")) +
  theme_bw() +
  xlab("") +
  ylab("人數") +
  ggtitle("行銷服務") +
  theme(plot.title = element_text(hjust = 0.5))

## v31 : 教育程度
ggplot(data = tidy, aes(x = v31, fill = v31)) +
  geom_bar(width = 0.7) +
  geom_text(aes(label = ..count..), stat = "count") +
  scale_fill_discrete(name = "教育程度",
                      breaks = c(1, 2, 3, 4, 5, 6, 7),
                      labels = c("不識字或自修", "小學", "國中或初中",
                                 "高中或高職", "專科", "大學", "研究所及以上")) +
  scale_x_discrete(labels = c("不識字或自修", "小學", "國中或初中",
                              "高中或高職", "專科", "大學", "研究所及以上")) +
  theme_bw() +
  xlab("") +
  ylab("人數") +
  ggtitle("教育程度 (包含目前正在就讀的)") +
  theme(plot.title = element_text(hjust = 0.5))

## v33 : 居住縣市
ggplot(data = tidy, aes(x = v33, fill = v33)) +
  geom_bar(width = 0.7) +
  geom_text(aes(label = ..count..), stat = "count") +
  scale_fill_discrete(name = "居住縣市",
                      breaks = c(1:22),
                      labels = c("臺北市", "新北市", "基隆市", "宜蘭縣",
                                 "桃園市", "新竹縣", "新竹市", "苗栗縣",
                                 "臺中市", "彰化縣", "南投縣", "雲林縣",
                                 "嘉義縣", "嘉義市", "臺南市", "高雄市",
                                 "屏東縣", "澎湖縣", "花蓮縣", "臺東縣",
                                 "金門縣", "連江縣")) +
  scale_x_discrete(labels = c("臺北市", "新北市", "基隆市", "宜蘭縣",
                              "桃園市", "新竹縣", "新竹市", "苗栗縣",
                              "臺中市", "彰化縣", "南投縣", "雲林縣",
                              "嘉義縣", "嘉義市", "臺南市", "高雄市",
                              "屏東縣", "澎湖縣", "花蓮縣", "臺東縣",
                              "金門縣", "連江縣")) +
  theme_bw() +
  xlab("") +
  ylab("人數") +
  ggtitle("居住縣市") +
  theme(plot.title = element_text(hjust = 0.5))

## v37 : 性別
ggplot(data = tidy, aes(x = v37, fill = v37)) +
  geom_bar(width = 0.4) +
  geom_text(aes(label = ..count..), stat = "count") +
  scale_fill_discrete(name = "性別",
                      breaks = c(1, 2),
                      labels = c("男性", "女性")) +
  scale_x_discrete(labels = c("男性", "女性")) +
  theme_bw() +
  xlab("") +
  ylab("人數") +
  ggtitle("性別") +
  theme(plot.title = element_text(hjust = 0.5))

### conduct MCA
tidy.mca <- mjca(Tidy.MCA.data, nd = 2, lambda = "JCA")
summary(tidy.mca)
plot(tidy.mca,
     main = "Multiple Correspondence Analysis")
cats <- apply(Tidy.MCA.data, 2, function(x) nlevels(as.factor(x)))
vars.df <- data.frame(tidy.mca$colcoord,
                      Variable = rep(names(cats), cats))
rownames(vars.df) <- tidy.mca$levelnames
ggplot(data = vars.df,
       aes(x = X1, y = X2, label = rownames(vars.df))) + 
  geom_vline(xintercept = 0, colour = "gray70") + 
  geom_hline(yintercept = 0, colour = "gray70") + 
  geom_text(aes(colour = Variable)) + 
  xlab("Dimension 1") +
  ylab("Dimension 2") +
  theme_bw() +
  ggtitle("Multiple Correspondence Analysis (MCA)") +
  theme(plot.title = element_text(hjust = 0.5))