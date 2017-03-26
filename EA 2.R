library(readxl)
data<-read_excel("C:/Users/chatr/Downloads/drug data.xlsx",sheet=1,col_names=T,col_types = NULL)
data<-data[,1:10]
summary(data)
str(data)

hist(data[,1],xlab='Clopidogrel')
hist(data[,2],xlab='Mortality score',breaks = 15)
hist(data[,3],xlab='Diabetes',breaks = 10)
hist(data[,4],xlab='Aspirin use',breaks = 10)

#continuous variables normality check:
hist(data[,5],xlab='Age',breaks = 4)
qqnorm(data[,5],main= "age normality check");qqline(data[,5])

hist(data[,6],xlab='Height') 
qqnorm(data[,6], main = "Height normality check");qqline(data[,6],col=2) #check for transformation

#log transformation of height variable:
height<-log(data[,6])
hist(height,xlab='log of height') #left skewed distribution
qqnorm(height);qqline(height) ##log transformation does not normalize the data

hist(data[,7],xlab='weight',breaks = 5,xlim=c(40,160)) 
qqnorm(data[,7]);qqline(data[,7],col=2) #check for influence of outlier

hist(data[,8],xlab='BSA') 
qqnorm(data[,8]);qqline(data[,8]) #is fairly normal

hist(data[,9],xlab='24EBL')
qqnorm(data[,9]);qqline(data[,9]) ##needs to be tranformed for normality

#log transformation of var 24EBL makes it fairly normal in distribution
hist(data[,10],xlab='log24EBL')
qqnorm(data[,10]);qqline(data[,10])  

##converting clopidogrel,diabetes,aspirin use into categorical vars:
data[,c(1,3,4)]<-lapply(data[,c(1,3,4)], factor)

#checking continuous input vars correlation:
cor<-cor(data[,c(2,5,6,7,8)])
pairs(data[,c(2,5,6,7,8)])

#checking chi-square for categorical variables correlation:
test1<-chisq.test(data$clopidogrel,data$diabetes,correct = F)
test2<-chisq.test(data$clopidogrel,data[,4],correct = F)
test3<-chisq.test(data$diabetes,data[,4],correct = F)

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(data[1:8])

install.packages('corrplot')
library(corrplot)
corrplot(cor,method='number')


#weight and Height with BSA are highly correlated-(causes problem)
#Mortality score correlated with Age
#Height and Weight are correlated positively
#Should check correlation of Diabetes with weight


#covariance of input vars:
cov(data[,1:8])


#PCA with vars Age,weight,mortality score:
pca_obj<-prcomp(data[,c(2,5,7)],scale=T)
names(pca_obj)
pca_obj$rotation
biplot(pca_obj,scale=0)
#Screeplot:
pve<-pca_obj$sdev^2/sum(pca_obj$sdev^2)
plot(pve,ylim=c(0,1),type = 'b')
#we can choose to keep 2 PCs as it explains as much as 81% variance

#Singular Value Decomposition:
svd=svd(data[,c(2,5,7)])

#ANOVA test-Parametric test:
anova1<-aov(data[,9]~data[,1])
TukeyHSD(anova1)
plot(TukeyHSD(anova1),las=1)
###p-value=0.147(>0.005)-accept null hypothesis-blood loss### 
###is fairly the same with/without Clopidogrel###


#Analyses of outcome of total blood loss:
reg1<-lm(log24EBL~.,data=data[,-c(9)])
pred1<-predict(reg1,data[,-c(9)])
err1<-mean((pred1-data[,10])^2) #0.019337959

reg2<-lm(log24EBL~.,data=data[,-c(6,7,9)])
pred2<-predict(reg2,data[,-c(6,7,9)])
err2<-mean((pred2-data[,10])^2) #0.019780730
##No major difference in the error for the analysis##
###It is evident from the analysis and the Parametric test that Clopidogrel has no affect on the outcome of Blood loss

