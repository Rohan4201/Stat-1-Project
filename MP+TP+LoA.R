### Make sure you have the required packages installed

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(ggpubr)
library(ggthemes)
library(plot3D)
library(randomForest)

### Importing the data set *** remember you need to change the path of file ***

Data<- read_excel("D:/Maths/2nd Year Sem-3/Project/Data set EN.xlsx")

### Removing the first unwanted columns

df <- as.data.frame(Data[-1,-c(1:6,94:116,13,16)])

# Creating  data frame with the LoA column alongwith Market perspective and Technology 
# perspective columns

alpha <- data.frame(df$DF02_01,select(df,MS02_01:TS02_18))

alpha <- filter(alpha,!is.na(df.DF02_01))

# Replacing NA values if any with median of column

for(i in 2:ncol(alpha)){
  alpha[,i] <- sapply(alpha[,i],as.numeric)
  alpha[,i] <- replace(alpha[,i],is.na(alpha[,i]),median(alpha[,i],na.rm=T))
}


# Rewriting the data frame in terms of subcategories of Market Perspective and Technology perspective


alpha <- alpha %>% transmute(LoA=df.DF02_01,Construction = round(rowMeans(alpha[colnames(select(alpha,TS04_19:TS04_32))]),3),
                             Product = round(rowMeans(alpha[colnames(select(alpha,TS03_05:TS03_14))]),3),
                             Technology.development = round(rowMeans(alpha[colnames(select(alpha,TS02_01:TS02_18))]),3),
                             Market.Competitors = round(rowMeans(alpha[colnames(select(alpha,MS02_01:MS02_11))]),3),
                             Own.company = round(rowMeans(alpha[colnames(select(alpha,MS03_12:MS03_17))]),3),
                             Personnel = round(rowMeans(alpha[colnames(select(alpha,MS04_20:MS04_22))]),3),
                             Customers = round(rowMeans(alpha[colnames(select(alpha,MS05_03:MS05_08))]),3))

# Converting the LoA entries to numeric

alpha$LoA <- sapply(alpha$LoA,as.numeric)

####
## Linear Model (LoA vs other factors)
####
model <- lm(LoA ~ . ,alpha)
summary(model)
# Say print(summary(model)) to print the summary. Say plot(model) to view various models
# in the list "model"

####
## Random Forest
####

model2 <- randomForest(LoA ~ ., data=alpha,method="class")

# You can search "randomForest"inR documentation help if you want particular value
# in the list of model1 eg. print(model1$importance), print(moddel1), print(model1$mse),etc.


####
## Some realted plts
####

# Boxplot



pl1 <- ggplot(alpha,aes(x=factor(LoA),y=Construction)) + geom_boxplot() + theme_bw()
pl2 <- ggplot(alpha,aes(x=factor(LoA),y=Product)) + geom_boxplot()+  theme_bw()
pl3 <- ggplot(alpha,aes(x=factor(LoA),y=Technology.development)) + geom_boxplot()+  theme_bw()
pl4 <- ggplot(alpha,aes(x=factor(LoA),y=Own.company)) + geom_boxplot()+  theme_bw()
pl5 <- ggplot(alpha,aes(x=factor(LoA),y=Customers)) + geom_boxplot()+ theme_bw()
pl6 <- ggplot(alpha,aes(x=factor(LoA),y=Personnel)) + geom_boxplot()+  theme_bw()
pl7 <- ggplot(alpha,aes(x=factor(LoA),y=Market.Competitors)) + geom_boxplot()+  theme_bw()

pl11 <- ggarrange(pl1,pl2,pl3,pl4,pl5,pl6,pl7,nrow=4,ncol=2)

# In the  above plots you can individually plot the boxplots or you can use ggarrange to 
# to plot all of them in one window























