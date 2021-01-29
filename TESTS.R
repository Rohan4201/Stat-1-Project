library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(randomForest)
library(rpart)


### Importing the data set *** remember you need to change the path of file *** 

Data<- read_excel("D:/Maths/2nd Year Sem-3/Project/Data set EN.xlsx",sheet = "data_MAproFli_2018-10-11_09-46")

### Removing the first unwanted columns

df <- as.data.frame(Data[-1,-c(1:6,94:116,13,16)])


#####
### Market Perspective dataframes alongwith LoA
#####

mp <- as.data.frame(select(df,MS02_01:MS05_08))

mp1 <- as.data.frame(select(df,MS02_01:MS05_08))


for(i1 in 1:ncol(mp)){
  mp[,i1] <- sapply(mp[,i1],as.numeric)
  mp[,i1] <- replace(mp[,i1],is.na(mp[,i1]),median(mp[,i1],na.rm=T))
}

for(i2 in 1:ncol(mp1)){
  mp1[,i2] <- sapply(mp1[,i2],as.numeric)
  mp1[,i2] <- replace(mp1[,i2],is.na(mp1[,i2]),round(mean(mp1[,i2],na.rm=T),2))
}

mp2 <- data.frame(row.names = 1:47)[1:47,]
mp2 <- cbind(mp2,Market.competitors = rowMeans(mp[,c("MS02_01","MS02_02","MS02_09","MS02_10","MS02_11")]))
mp2 <- cbind(mp2,Own.company = round(rowMeans(mp[,c("MS03_12","MS03_13","MS03_14","MS03_15","MS03_16","MS03_17")]),digits=1))
mp2 <- cbind(mp2,Personnel = rowMeans(mp[,c("MS04_20","MS04_19","MS04_18","MS04_21","MS04_22")]))
mp2 <- cbind(mp2,Customers = rowMeans(mp[,c("MS05_03","MS05_05","MS05_06","MS05_07","MS05_08")]))


mp3 <- data.frame(row.names = 1:47)[1:47,]
mp3 <- cbind(mp3,Market.competitors = round(rowMeans(mp1[,c("MS02_01","MS02_02","MS02_09","MS02_10","MS02_11")]),2))
mp3 <- cbind(mp3,Own.company = round(rowMeans(mp1[,c("MS03_12","MS03_13","MS03_14","MS03_15","MS03_16","MS03_17")]),1))
mp3 <- cbind(mp3,Personnel = round(rowMeans(mp1[,c("MS04_20","MS04_19","MS04_18","MS04_21","MS04_22")]),2))
mp3 <- cbind(mp3,Customers = round(rowMeans(mp1[,c("MS05_03","MS05_05","MS05_06","MS05_07","MS05_08")]),2))


mp <- drop_na(cbind(LoA=df$DF02_01,mp),LoA)
mp1 <- drop_na(cbind(LoA=df$DF02_01,mp1),LoA)
mp2 <- drop_na(cbind(LoA=df$DF02_01,mp2),LoA)
mp3 <- drop_na(cbind(LoA=df$DF02_01,mp3),LoA)

mp$LoA <- sapply(mp$LoA,as.numeric)
mp2$LoA <- sapply(mp2$LoA,as.numeric)


#####
### Technology perspective dataframes alongwith LoA
#####


tp <- as.data.frame(select(df,TS04_19:TS02_18))

tp1 <- as.data.frame(select(df,TS04_19:TS02_18))

for(k1 in 1:ncol(tp)){
  tp[,k1] <- sapply(tp[,k1],as.numeric)
  tp[,k1] <- replace(tp[,k1],is.na(tp[,k1]),median(tp[,k1],na.rm=T))
}

for(k2 in 1:ncol(tp1)){
  tp1[,k2] <- sapply(tp1[,k2],as.numeric)
  tp1[,k2] <- replace(tp1[,k2],is.na(tp1[,k2]),round(mean(tp1[,k2],na.rm=T),2))
}


tp2 <- data.frame(row.names = 1:47)[1:47,]
tp2 <- cbind(tp2,Construction = round(rowMeans(tp[colnames(select(tp,TS04_19:TS04_32))]),2))
tp2 <- cbind(tp2,Product = round(rowMeans(tp[colnames(select(tp,TS03_05:TS03_14))]),2))
tp2 <- cbind(tp2,Technology.development = round(rowMeans(tp[colnames(select(tp,TS02_01:TS02_18))]),2))

tp3 <- data.frame(row.names = 1:47)[1:47,]
tp3 <- cbind(tp3,Construction = round(rowMeans(tp1[colnames(select(tp,TS04_19:TS04_32))]),2))
tp3 <- cbind(tp3,Product = round(rowMeans(tp1[colnames(select(tp,TS03_05:TS03_14))]),2))
tp3 <- cbind(tp3,Technology.development = round(rowMeans(tp1[colnames(select(tp,TS02_01:TS02_18))]),2))


tp <- drop_na(cbind(LoA=df$DF02_01,tp),LoA)
tp1 <- drop_na(cbind(LoA=df$DF02_01,tp1),LoA)
tp2 <- drop_na(cbind(LoA=df$DF02_01,tp2),LoA)
tp3 <- drop_na(cbind(LoA=df$DF02_01,tp3),LoA)



tp$LoA <- sapply(tp$LoA,as.numeric)
tp2$LoA <- sapply(tp2$LoA,as.numeric)


#####
### ANOVA tests
#####

mp.anova <- aov(LoA~.,mp)
mp1.anova <- aov(LoA~.,mp1)
mp2.anova <- aov(LoA~.,mp2)
mp3.anova <- aov(LoA~.,mp3)

tp.anova <- aov(LoA~.,tp)
tp1.anova <- aov(LoA~.,tp1)
tp2.anova <- aov(LoA~.,tp2)
tp3.anova <- aov(LoA~.,tp3)

#####
### Linear models
#####

mp.lm <- lm(LoA~., mp)
mp.final.lm <- step(mp.lm)
summary(mp.final.lm)

mp1.lm <- lm(LoA~., mp1)

mp2.lm <- lm(LoA~., mp2)
mp2.final.lm <- step(mp2.lm)
summary(mp2.final.lm)

mp3.lm <- lm(LoA~., mp3)

tp.lm <- lm(LoA~., tp)
tp.final.lm <- step(tp.lm)
summary(tp.final.lm)

tp1.lm <- lm(LoA~., tp1)

tp2.lm <- lm(LoA~., tp2)
tp2.final.lm <- step(tp2.lm)
summary(tp2.final.lm)

tp3.lm <- lm(LoA~., tp3)



#####
### Chi squared association tests
#####

#1

df$DF02_01 <- as.factor(df$DF02_01)
df$DF03_01 <- as.factor(df$DF03_01)
tbl1 <- table(df$DF02_01,df$DF03_01)
chisq.test(tbl1)


#2

df$MH04_01 <- as.factor(df$MH04_01)
df$MH05_01 <- as.factor(df$MH05_01)
tbl2 <- table(df$MH04_01,df$MH05_01)
chisq.test(tbl2)

#3

df$DF07_01 <- as.factor(df$DF07_01)
tbl3 <- table(df$DF02_01,df$DF07_01)
chisq.test(tbl3)


#4

df$DF05_01 <- as.factor(df$DF05_01)
tbl4 <- table(df$DF02_01,df$DF05_01)
chisq.test(tbl4)

#5

df$DF06 <- as.factor(df$DF06)
tbl5 <- table(df$DF02_01,df$DF06)
chisq.test(tbl5)

#6

df$MF01_11 <- as.factor(df$MF01_11)
tbl6 <- table(df$DF02_01,df$MF01_11)
chisq.test(tbl6)







