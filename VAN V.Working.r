rm(list=ls())
setwd("\\Users\\canno\\OneDrive\\Desktop\\R Script")
#install packages 
library(ggplot2)
library(dplyr)
library(car)

#read csv
VAN <- read.csv('Data-source.csv',header=TRUE, sep = ",")
VAN <- na.omit(VAN)
head(VAN)
summary(VAN)
VAN['Distance'] <- as.numeric(VAN$Distance)
summary(VAN)

#varibles
plot(VAN$Distance,VAN$BILLED)
plot(VAN$WEIGHT,VAN$BILLED)
cor(VAN$WEIGHT,VAN$BILLED,method=c("pearson")) #.150
cor(VAN$Distance,VAN$BILLED,method=c("pearson")) #.585

#distance lm
distance.fit <- lm(BILLED ~ Distance, data = VAN)
summary(distance.fit)
anova(distance.fit)
#basic data visualize 
plot(BILLED ~ Distance , data = VAN)
abline(distance.fit)


#ggplot lm 
billed_distance <- ggplot(VAN,aes(x=Distance,y=BILLED))+
  geom_point(size=3,alpha=0.6)+
  geom_smooth(col="grey40",method = "lm",se=F,lty="solid",lwd=2,formula="y~x")
billed_distance

#VAN weight and distance lm
best.fit <- lm(BILLED ~ Distance + WEIGHT , data = VAN)
summary(best.fit)
anova(best.fit)

