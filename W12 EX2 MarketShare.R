#Phase I: Identify the problem
#建立產品市場份額(Market Share)與其他影響因子間的迴歸關係,
#以了解其他影響因子與產品市場份額(Market Share)間的關係,
#並做為後續預測產品市場份額之用,
#以提供廠商參考用

#Phase II: MAKE ASSUMPTIONS AND DEFINE ESSENTIAL VARIABLES
#Four basic assumptions
#All the important factors are included in the study.
#Dependent Variable: MShare
#Independent Variable: Other variables in the dataset.


#Phase III: Do the Math

setwd('C:\\Users\\a0987\\git-repos\\modeling precedure')
Market=read.table("MarketShare.txt",header=T)
Market$DiscP <- as.factor(Market$DiscP)
Market$PProm <- as.factor(Market$PProm)

stem(Market$MShare)
stem(Market$Price)
stem(Market$GNrate)
plot(Market$DiscP)
table(Market$DiscP)
plot(Market$PProm)
table(Market$PProm) #檢測各變數中有沒有離群值或左右偏

set.seed(123) #把測試資料區分開
Sindex=sample(nrow(Market),30)
Train=Market[Sindex,]
Test=Market[-Sindex,]

pairs(MShare~.,data=Train) #用散布圖檢查變數間的關係
#cor(Train[,c(2,3,4,5,6,8)])

M1=lm(MShare~.,data=Train)
summary(M1)
#Adjusted R-squared若比R-squared小太多，則代表此模型變數過多
#可看出R2跟Ra2差了6%，模型需改進

#Phase IV:Diagnostic(四個假設的診斷)
library(car)
library(lmtest)
library(nortest)
library(randtests)
###Function Form and Homogeneity
#e=residuals(M1) 求出殘差
es=rstandard(M1)
residualPlot(M1,type="rstandard",quadratic=F)
#畫出殘差圖，看資料的中心是否平穩在0的虛線上(function form);


resettest(M1,power=2,type='regressor')
ncvTest(M1)#This test is often called the Breusch-Pagan test; 

#Normality
qqPlot(M1)
lillie.test(es)#KS test for normality
shapiro.test(es)#Shapiro-Wilk Normality Test

plot(es,type = "l",col='2')
acf(es, ci=0.99)
#dwtest(M1)#Durbin-Watson test
runs.test(es)

#phase IV的4個假設都hold
#故Phase V的矯正跟診斷可以不用再做，直接變數選擇

#Phase V:
#直接變數選擇(用逐步回歸)
s1=step(M1)
s2=step(M1,k=log(nrow(Train)))


M2a=lm(MShare~Price+PProm+DiscP,data=Train)
summary(M2a)
#Fine tuning using t-test
M2b=lm(MShare~Price+DiscP,data=Train)
summary(M2b)


#M1 or M2a can be our final model

#共線性 might existed
vif(M1)
vif(M2a)
vif(M2b) #都沒有大於10，OK

#有效性評估
M1p=predict(M1, newdata=Test)
r1=M1p-Test$MShare
MSE1=mean(r1^2)
RMSE1 = sqrt(MSE1)
MAE1 = mean(abs(r1))
MAPE1=mean(abs(r1/Test$MShare))

M2ap=predict(M2a, newdata=Test)
r2a=M2ap-Test$MShare
MSE2a=mean(r2a^2)
RMSE2a = sqrt(MSE2a)
MAE2a = mean(abs(r2a))
MAPE2a=mean(abs(r2a/Test$MShare))

M2bp=predict(M2b, newdata=Test)
r2b=M2bp-Test$MShare
MSE2b=mean(r2b^2)
RMSE2b = sqrt(MSE2b)
MAE2b = mean(abs(r2b))
MAPE2b=mean(abs(r2b/Test$MShare))

#according to our validation result
#our final model is m2a

#Phase VI:
summary(M2a)
confint(M2a)
#the meaning of coefficients
#例: 在其他變數不變情況下，Price每增加1，Y會下降0.41
#    在其他變數不變情況下，(DiscP1)有打折的商品的Y會比沒有打折的商品高0.39%
#R2=0.78
#MAE=103,MAPE.....


#contrasts(AU)
#type <- relevel(type,ref="prof")

