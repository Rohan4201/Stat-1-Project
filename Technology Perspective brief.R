### Make sure you have the required packages installed

library(readxl)
library(dplyr)
library(tidyr)
library(corrplot)
library(ggpubr)
library(ggplot2)
library(ggthemes)
library(plot3D)

### Importing the data set *** remember you need to change the path of file ***

Data<- read_excel("D:/Maths/2nd Year Sem-3/Project/Data set EN.xlsx")

### Removing the first unwanted columns

df <- as.data.frame(Data[-1,-c(1:6,94:116,13,16)])

### Selecting the Technology perspective columns in the dataset

tp <- as.data.frame(select(df,TS04_19:TS02_18))

### Replacing null values in a column with the median value of the column

for(i in 1:ncol(tp)){
  tp[,i] <- sapply(tp[,i],as.numeric)
  tp[,i] <- replace(tp[,i],is.na(tp[,i]),median(tp[,i],na.rm=T))
}

####
## Creating a new data frame with subcatagories as columns and values as means of 
#  corresponding sub-subcategories of a particular row
####

tp2 <- data.frame(row.names = 1:47)[1:47,]

tp2 <- cbind(tp2,Construction = round(rowMeans(tp[colnames(select(tp,TS04_19:TS04_32))]),3))

tp2 <- cbind(tp2,Product = round(rowMeans(tp[colnames(select(tp,TS03_05:TS03_14))]),3))

tp2 <- cbind(tp2,Technology.development = round(rowMeans(tp[colnames(select(tp,TS02_01:TS02_18))]),3))


####
### Appending Data frames mp and mp2 (incase we need it in future)
####

tp <- bind_cols(tp,tp2)

#####
## Correlation plots
#####

c1 <- cor(select(tp,TS04_19:TS04_32),method="spearman")

c2 <- cor(select(tp,TS03_05:TS03_14),method = "spearman")

c3 <- cor(select(tp,TS02_01:TS02_18), method="spearman")

    # c11 <- corrplot(c1,method= "shade",tl.cex=0.9,mar=c(1,1,1,1))
    # c31 <- corrplot(c3,method= "shade",tl.cex=0.9,mar=c(1,1,1,1))
    # c21 <- corrplot(c2,method= "shade",tl.cex=0.9,mar=c(1,1,1,1))

## ***** Remember to remove the hash before the required plot among c11,c21,c31 for printing the correlation plot. ******


####
## One particular scatterplot of influences of subcategories of TECHNOLOGY PERSPECTIVE
####

pl <- ggplot(tp2,aes(x=Construction,y=Product,color=Technology.development)) + geom_point(size=5,alpha=0.5)
pl <- pl + theme_bw()+ scale_color_gradient(high="red",low="blue") +geom_smooth(se=F) 

#### Linear models ####

# Not much details added eg. You can create train and test data and play around with those.
model11 <- lm(Construction ~ . ,tp2)

model12 <- lm(Product ~ . ,tp2)

model13 <- lm(Technology.development ~ . ,tp2)


# To view model you can try eg. print(summary(model11))
# To plot different plots of your model you can try eg. plot(model11)

#####
### Plotting tp2 
#####

# Histogram

pl2 <- ggplot(tp2,aes(x=Construction))+ geom_histogram(bins=15,fill="blue",alpha=0.5)+theme_bw()
pl2 <- pl2 +xlab("Construction influence")

pl3 <- ggplot(tp2,aes(x=Product))+ geom_histogram(bins=15,fill="blue",alpha=0.5)+theme_bw()
pl3 <- pl3 +xlab("Product influence")

pl4 <- ggplot(tp2,aes(x=Technology.development))+ geom_histogram(bins=15,fill="blue",alpha=0.5)+theme_bw()
pl4 <- pl4 +xlab("Technology and development influence")

plh <- ggarrange(pl2,pl3,pl4)

# 2D density(filled)

pl5 <- ggplot(tp2,aes(x=Construction,y=Technology.development)) + geom_density2d_filled() +theme_bw()

pl6 <- ggplot(tp2,aes(x=Product,y=Technology.development)) + geom_density2d_filled() +theme_bw()

pl7 <- ggplot(tp2,aes(x=Construction,y=Product)) + geom_density2d_filled() +theme_bw()

pl8 <- ggarrange(pl5,pl6,pl7)

# Density

pl41 <- ggplot(tp2,aes(x=Construction)) + geom_density() + theme_bw()

pl42 <- ggplot(tp2,aes(x=Product)) + geom_density() + theme_bw()

pl43 <- ggplot(tp2,aes(x=Technology.development)) + geom_density() + theme_bw()

pl44 <- ggarrange(pl41,pl42,pl43,nrow=2,ncol=2)

# 3D plot(without any regression model fitted)
# For more use of plot3D visit :
# http://www.sthda.com/english/wiki/impressive-package-for-3d-and-4d-graph-r-software-and-data-visualization

pl9 <- scatter3D(tp2$Construction , tp2$Product, tp2$Technology.development,phi=0,
                 pch = 20, cex = 1.6, ticktype = "detailed",plot=T,size=3
                 ,xlab="Construction",ylab="Product",zlab="Technology Development",main="3D Scatterplot",type='b')

# The lines in above plot can be removed by removing or changing the type argument

# 3D plot(with regression)- Visit the above link

# To display above plots just say print(x) where x is the name of the plot