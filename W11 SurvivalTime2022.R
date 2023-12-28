
#Phase I: Identify the problem
#建立病患存活時間(Survival Time)與其他病患本身特徵間的迴歸關係,
#以了解病患本身特徵與存活時間(Survival Time)間的關係,
#以提供醫師及病人參考

#Phase II: MAKE ASSUMPTIONS AND DEFINE ESSENTIAL VARIABLES
#Four basic assumptions
#All the patients' features are measured correctly.
#Depenent Varialbe: Stime
#Independent Variable: Other variables in the dataset.


#Phase III : DO THE MATH
setwd("C:\\Users\\a0987\\OneDrive\\桌面\\迴歸分析")
STdata=read.table("SurvivalTime.txt",header=T)
STdata$AU=as.factor(STdata$AU)
STdata$Gender=as.factor(STdata$Gender) #把明顯為類別變數的變數轉為factor

#attach(STdata)  #檢測各變數中有沒有離群值或左右偏
plot(STdata$AU)
table(STdata$AU)
#barplot(table(STdata$AU))
stem(STdata$BCS)
stem(STdata$PI)
stem(STdata$ET)
stem(STdata$LT)
stem(STdata$AGE)
plot(STdata$Gender)
table(STdata$Gender)
#barplot(table(STdata$Gender))
stem(STdata$Stime)
#可看出Stime這個變數有強烈右偏

#detach(STdata)12 #把測試資料區分開(從原始資料中分離)
set.seed(234)
Sindex=sample(nrow(STdata),70) #隨機抽取70筆要保留的部分資料
#Sindex=sample(N, round(N*0.8)) #抽取80%作為要保留的部分資料
Train=STdata[Sindex,]
Test=STdata[-Sindex,] #把資料分為訓練資料跟測試資料

pairs(Stime~.,data=Train) #用散布圖檢查變數間的關係
cor(Train[,c(2,3,4,5,6,8)]) #correlation 越高可能代表貢共線性越明顯

M1=lm(Stime~.,data=Train)
summary(M1)
#Adjusted R-squared若比R-squared小太多，則代表此模型變數過多



#Phase IV : Diagnostic(四個假設的診斷)
library(car)
library(lmtest)
library(nortest)
library(randtests)

###Function Form and Homogeneity
#e=residuals(M1)
es=rstandard(M1) #求出殘差
yhat=fitted.values(M1)

plot(yhat,es,col='2')
#plot(yhat,es,col='2')
abline(h=0) #e-yhat plot
residualPlot(M1,type="rstandard",quadratic=F) 
#畫出殘差圖，看資料的中心是否平穩在0的虛線上(function form);
#以及資料變異的一致性，看點是否均勻(齊一性)

resettest(M1,power=2,type='regressor') 
#檢驗屬量變數的二次項係數是否應該同時為零
#p value 小於0.05，拒絕了!代表此模型有潛在的二次趨勢

ncvTest(M1)
#Breusch-Pagan test
#檢定齊一性的，看sigma i 是否不論i為何皆相同
#拒絕了!代表sigma i之間存在顯著差異，即變異是不一致的

#Normality
qqPlot(M1) 
#看到圖上其實有很多點在CI外，所以圖形應是顯示非常態的
lillie.test(es)
#KS test for normality
#H0 : Yi服從常態 H1: Yi不服從常態
shapiro.test(es)
#Shapiro-Wilk Normality Test
#H0 : Yi服從常態 H1: Yi不服從常態
#KS test和Shapiro-Wilk Normality Test皆拒絕，即代表Yi不服從常態

#Randomness test
plot(es,type = "l",col='2')
acf(es, ci=0.99)
#兩張圖看起來都還好，前後資料並沒有明顯相關

#dwtest(M1)#Durbin-Watson test
runs.test(es)
# H0: 殘差是隨機的  H1: 殘差不是隨機的
#檢定結果殘差是隨機的，隨機性hold

#Phase IV結論: 四大基本假設除了隨機性，其餘的齊一性、常態性、和函數形式皆有問題


#Phase V : Refined and extend the model(矯正、變數選擇、有效性評估)

#Step 1:矯正

library(MASS)
boxcox(M1) #取靠近極值，更好解釋的那個(做Y的轉換時比較好跟顧客解釋)
#Stime1=log(Stime)，選擇取log
M2=lm(log(Stime)~.,data=Train)
summary(M2) #新模型

#再做一次第四步的診斷
#e2=residuals(M2)
e2s=rstandard(M2)
yhat2=fitted.values(M2)

###Function Form and Homogeneity
plot(yhat2,e2s)
abline(h=0,col=2)
residualPlot(M2,type="rstandard",quadratic=F)
#畫出殘差圖，看資料的中心是否平穩在0的虛線上(function form);
#以及資料變異的一致性，看點是否均勻(齊一性)
resettest(M2,power=2,type='regressor')
#檢驗屬量變數的二次項係數是否應該同時為零
ncvTest(M2)
#This test is often called the Breusch-Pagan test; 
#檢定齊一性的，看sigma i 是否不論i為何皆相同
#接受Function Form and Homogeneity是hold

#Normality
qqPlot(M2)
lillie.test(e2s)#KS test for normality
shapiro.test(e2s)#Shapiro-Wilk Normality Test
#接受Normality是hold

#Randomness test
plot(e2s,type = "l",col='2')
acf(e2s,ci=0.99)
#dwtest(M2)#Durbin-Watson test
runs.test(es)
#接受Randomness是hold

#Step 1:矯正完結

#Step 2:變數選擇，用逐步回歸或是criteria
#用criteria
library(leaps)
subx=regsubsets(log(Stime)~., nbest=3, data=Train)

subsets(subx,statistic="bic")
subsets(subx,statistic="bic",min.size=3, max.size=6)
#看BIC最低的參數組合就是最佳的model
subsets(subx,statistic="adjr2", legend=F)
subsets(subx,statistic="adjr2",min.size=4, max.size=8)
#看Adjusted R-squared最高的參數組合就是最佳的model

#subsets(subx,statistic="cp")
#abline(a=0,b=1)
#subsets(subx,statistic="cp",min.size=8, max.size=9)
#abline(a=0,b=1)

#或用 逐步回歸
s1=step(M2) #預設是用AIC
s2=step(M2,k=log(dim(Train)[1])) #改成用BIC


M2a=lm(log(Stime)~AU+BCS+PI+ET+LT,data=Train)
#從s1來，剛剛AIC挑出的最佳參數組合
summary(M2a)

M2b=lm(log(Stime)~AU+LT+PI+ET,data=Train)
#從s2來，剛剛BIC挑出的最佳參數組合
summary(M2b)

#General Linear Test(想把M2b中可能不顯著的AU拿掉，故要比較拿掉前跟拿掉後)
M2br = lm(log(Stime)~LT+PI+ET,data=Train) #拿掉不顯著(可能可以拿掉)的AU
anova(M2b, M2br) 
#做general linear test(拿掉一個變數的reduced model 和原本的full model比)的H0跟H1為
#H0: 該變數係數為0  H1: 該變數係數不為0 (此處是在檢驗AU的dummy variable是否同時為零)
#結果為顯著，即AU不為0，不可拿掉

#M2b or M2a can be our final model

#檢測共線性
vif(M1)
vif(M2a)
vif(M2b) #檢測共線性是否太過嚴重，看該模型vif有沒有超過10


#Step 3:有效性評估 (MSE、RMSE、MAE、MAPE)
###Step 1:用有效性的四個指標來決定最終採用的模型
M1p=predict(M1, newdata=Test) #用一開始保留的Test資料做評估
#建立M1模型(矯正前那個)的預測值
r1=M1p-Test$Stime
MSE1=mean(r1^2)
RMSE1 = sqrt(MSE1)
MAE1 = mean(abs(r1))
MAPE1=mean(abs(r1/Test$Stime))

M2ap=predict(M2a, newdata=Test)
#建立M2a模型的預測值
r2a=exp(M2ap)-Test$Stime 
#注意! 因為我們在建M2a模時資料有經過log轉換，所以誤差要注意要先換回去才能跟保留的原始資料比
MSE2a=mean(r2a^2)
RMSE2a = sqrt(MSE2a)
MAE2a = mean(abs(r2a))
MAPE2a=mean(abs(r2a/Test$Stime))

M2bp=predict(M2b, newdata=Test)
r2b=exp(M2bp)-Test$Stime
MSE2b=mean(r2b^2)
RMSE2b = sqrt(MSE2b)
MAE2b = mean(abs(r2b))
MAPE2b=mean(abs(r2b/Test$Stime))

#according to our validation result
#our final model is m2a

###Step 2: 解釋這個模型的意義和其預測的價值
summary(M2a)
confint(M2a)
#the meaning of coefficients
#例: ET  0.011043381 0.01881875
#    其餘參數不動，當ET每增加1，模型的Y會上升0.011到0.019
#R2=0.78 (R2A是用來選變數，而不是選模型)
#MAE=103,MAPE.....

p1 <- predict(M2a,newdata = Test,interval="confidence",level=0.99)
#預測平均值的CI
p2 <- predict(M2a,newdata = Test,interval="prediction",level=0.99) 
#預測單一值的CI

p1f <- exp(p1)
p1f
# fit(平均值)     lwr(信賴區間下界)      upr(信賴區間上界)

p2f <- exp(p2) #把之前轉換過的資料調整回來
p2f
# fit(預測值之單一值)     lwr(信賴區間下界)      upr(信賴區間上界)

contrasts(AU)
#調類別變數的參考類別
#type <- relevel(type,ref="prof")

