library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(likert)

### Importing the data set *** remember you need to change the path of file *** 

Data<- read_excel("D:/Maths/2nd Year Sem-3/Project/Data set EN.xlsx",sheet = "data_MAproFli_2018-10-11_09-46")

### Removing the first unwanted columns

df <- as.data.frame(Data[-1,-c(1:6,94:116,13,16)])

mp <- as.data.frame(select(df,MS02_01:MS05_08))

for(i1 in 1:ncol(mp)){
  mp[,i1] <- sapply(mp[,i1],as.numeric)
  mp[,i1] <- replace(mp[,i1],is.na(mp[,i1]),median(mp[,i1],na.rm=T))
 
}

mp2 <- data.frame(row.names = 1:47)[1:47,]
mp2 <- cbind(mp2,Market.competitors = rowMeans(mp[,c("MS02_01","MS02_02","MS02_09","MS02_10","MS02_11")]))
mp2 <- cbind(mp2,Own.company = round(rowMeans(mp[,c("MS03_12","MS03_13","MS03_14","MS03_15","MS03_16","MS03_17")]),digits=1))
mp2 <- cbind(mp2,Personnel = rowMeans(mp[,c("MS04_20","MS04_19","MS04_18","MS04_21","MS04_22")]))
mp2 <- cbind(mp2,Customers = rowMeans(mp[,c("MS05_03","MS05_05","MS05_06","MS05_07","MS05_08")]))


colnames(mp) <- c('Dynamics of market','Structure of market',
                  'Competitor structure','Entry of new competitiors','Automation strategy'
                  , 'Core competencies of company ','Degree of specialization','Place of production','Company size',
                  'Corporate culture','Willingness to invest','Employee structure','Qualification of employees',
                  'Qualification measures','Willingness to change','Works council',
                  'Demand development','Quality requirements','Delivery requirements','Price requirements','Individuality requirements')

tp <- as.data.frame(select(df,TS04_19:TS02_18))

for(k1 in 1:ncol(tp)){
  tp[,k1] <- sapply(tp[,k1],as.numeric)
  tp[,k1] <- replace(tp[,k1],is.na(tp[,k1]),median(tp[,k1],na.rm=T))

}

tp2 <- data.frame(row.names = 1:47)[1:47,]
tp2 <- cbind(tp2,Construction = round(rowMeans(tp[colnames(select(tp,TS04_19:TS04_32))]),2))
tp2 <- cbind(tp2,Product = round(rowMeans(tp[colnames(select(tp,TS03_05:TS03_14))]),2))
tp2 <- cbind(tp2,Technology.development = round(rowMeans(tp[colnames(select(tp,TS02_01:TS02_18))]),2))
 
colnames(tp) <- c('Form stability of the joining component','Sensitivity of the joining component'
                  ,'Gripping surfaces on the joining component','Variants of the joining component',
                  'Enveloping volume of the joining component','Number of stable component positions',
                  'Symmetry of the joining component','Hooking and jamming of the joining component', 'Defective joining components','Accessibility of the positioning range',
                   'Orientation of the joining component','Joining movement','Joining force or joining moment','Joining aid available on joining & base component'
                  ,'Duration of the product life cycle','Product life cycle stage','Number of product variants','Product types',
                  'New product launch','Product quantities','Product weight','Product size','Product complexity','Frequency of design changes',
                  'Technology level','Speed of innovation','Technical standards','Research and development intensity','Reusability of the equipment','Assembly technology',
                  'Availability of information'
                  )


## Creating dataframe for Monetary factors

mon <- as.data.frame(select(df,MF01_01:MF01_19))

for(i3 in 1:ncol(mon)){
  mon[,i3] <- sapply(mon[,i3],as.numeric)
  mon[,i3] <- replace(mon[,i3],is.na(mon[,i3]),median(mon[,i3],na.rm=T))

}

colnames(mon) <- c('Company turnover','Company profit','Financial situation','Personnel costs',
                   'Production overhead cost','Development and construction costs','Machine hour rate','Depreciation','Investment budget',
                   'Working time models and remuneration models','Assembly costs/piece','Annual outpu','Annual assembly costs',
                   'Amortization period for plant and equipment','Reacquisition value of plant and equipment','Calculative interest',
                   'Maintenance costs','Operating time per day in x shifts','Working days per year')



#####
### Divergent Rows
#####

mp.d <- likert(mp,nlevels=6)
mp.d.plot <-  plot(mp.d,low.color="red",high.color="forestgreen")+scale_y_continuous(labels = likert:::abs_formatter, lim = c(-100, 100),
                                                                                     breaks = seq(-100, 100, 10))


mp2.d <- likert(mp2,nlevels=6)
mp2.d.plot <-  plot(mp2.d,low.color="red",high.color="forestgreen")+scale_y_continuous(labels = likert:::abs_formatter, lim = c(-100, 100),
                                                                                       breaks = seq(-100, 100, 25))

tp.d <- likert(tp,nlevels=6)
tp.d.plot <-  plot(tp.d,low.color="red",high.color="forestgreen")+scale_y_continuous(labels = likert:::abs_formatter, lim = c(-100, 100),
                                                                                     breaks = seq(-100, 100, 10))

tp2.d <- likert(tp2,nlevels=6)
tp2.d.plot <-  plot(tp2.d,low.color="red",high.color="forestgreen")+scale_y_continuous(labels = likert:::abs_formatter, lim = c(-100, 100),
                                                                                       breaks = seq(-100, 100, 25))
mon.d <- likert(mon,nlevels=6)
mon.d.plot <- plot(mon.d,low.color="red",high.color="forestgreen")+scale_y_continuous(labels = likert:::abs_formatter, lim = c(-100, 100),
                                                                                      breaks = seq(-100, 100, 25))
  
##### To print the plots type the plot name in the console 







































