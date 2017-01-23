require(MASS)
require(car)
require(ellipse)
#problem 1#
#Compare length of confidence intervals using Bonferroni Correction and Scheffe's method
#formula of the width of CI's contains a multiplication of the Standard error of the parameter
#which will be cancelled by division.
alpha=0.05
for(p in c(2,5,20,100)){
  for(n in c(5*p,10*p,20*p)){
    Width_CI_Bonferroni = 2*qt(1-(alpha/p)/2,n-p-1)#Bonferroni Correction: alpha->alpha/p
    Width_CI_Scheffe = 2*sqrt((p+1)*qf(1-alpha,p+1,n-p-1))
    Ratio=Width_CI_Bonferroni/Width_CI_Scheffe
    print(Ratio)
  }
}

#problem 2

x=runif(50,0,1)
times=200
countnum=0#count number of slopes that stays in 95%CI
is.list(x)
for(i in 1:times){
  epsilon=rnorm(50,0,0.1)
  y=3-x+epsilon
  fit=lm(y~x)
  #Can calculate CI using the formula provided by lecture material, or simply use confint(), results are the same.
  #CI_low=coef(summary(fit))[2]-coef(summary(fit))[4]*qt(0.975,48)
  #CI_high=coef(summary(fit))[2]+coef(summary(fit))[4]*qt(0.975,48)
  #confint(fit,"x")
  if(-1>confint.lm(fit,"x")[1]&&-1<confint.lm(fit,"x")[2]){
    countnum=countnum+1
    }
}
print(countnum)#number of slopes that stays in 95% CI.
#The distribution of the number of intervals that contains the true slope satisfies a binomial(200,0.95) distribution



#problem 3

dat=read.csv("4-mile-run.csv")
#(a).Comment on the dataset: This dataset has 19 observations and 14 variables. We want to focus on 4 of them.
#If we want to use linear model to analyse the influence of these variables on the training effect, then this sample size is too small.
#It is believed that a sample size of at least 30 is desired and that sample size should be 5~10 times bigger than the number of variables(including intercept) in the model.
#Thus the sample size in this dataset is not sufficient for a full model. 


#(b).Find a simple linear regression, the best one should have largest F value,i.e. the model should be most significant.
fit=lm(dat$Calories.Burned~dat$Max.Speed)
summary(fit)
#other fit models are: fit=lm(dat$Calories.Burned~dat$Avg.Speed),lm(dat$Calories.Burned~dat$Avg.HR),lm(dat$Calories.Burned~dat$Max.HR)
#summary shows that the F statistic has Pr=0.001 which is much less than 0.05, i.e. the model is significant.
#Turns outCalories.Burned~Max.Speed has biggest F value, thus most significant.

#(c) Draw confidence region for the simple linear regression model.
#Confidence region is from this formula: ||(X'X)^(1/2)*(betahat-beta)||<=sqrt((p+1)F(1-alpha,p+1,n-p-1))*sigmahat
#use ellipse
a=ellipse(fit,which=c(1,2),level=0.99,sqrt(2*qf(0.01,2,19-1-1)))
plot(a,pch="*")#gives a ellipse-shape



#(d)Analysis of variance.
#NULL H_0: Calories.Burned~Avg.Speed+Max.Speed
#Alternative H_1: Calories.Burned~Avg.Speed+Max.Speed+Avg.HR+Max.HR
#(SSY_0-SSY_1)/(2)/SSY/(n-4-1)~F(2,n-4-1) under NULL
fit0=lm(dat$Calories.Burned~dat$Avg.Speed+dat$Max.Speed)
fit1=lm(dat$Calories.Burned~dat$Avg.Speed+dat$Max.Speed+dat$Max.HR+dat$Avg.HR)
anova(fit0,fit1)

# or use the formula in the slides, the one below calculates the F value.
(sum(residuals(fit0)^2)-sum(residuals(fit1)^2))/2/(sum(residuals(fit1)^2)/(19-4-1))
qf(0.95,2,14)
#We can see that since 0.1039<<3.73, cannot reject H_0. The full model does not bring a significant change to the accuracy of the model. 




#(e)
#Inference is based on standard assumptions of the error e_i:
# i.e. e_i are i.i.d. distributed N(0,sigma^2)
#For residuals they are normally distributed with mean 0 and Covariance sigma^2(I-H)
#Need to check: 1.Independence 2.Normality 3.Mean zero 4.Homoscedasticity

#Independence
#If the data is collected over time, we can see if there is some dependency with time using durbinWatsonTest()
durbinWatsonTest(residuals(fit0))
#from DW test chart for(p=3,n=19) dw=1.08 and du=1.53 and 1.272 is in between them, thus can't tell whether there is dependency or not.


#Normality
#use qqplot to check normality:
plot(fit0,which=2)
#seems it is normal
#========================================================#

#mean zero and homoscedasticity
residualPlots(fit0)
#we can see that the residuals are distributed evenly in two sides of 0, and there is no fan-shape detected.
#Thus we tends to believe there is no hetereoscedasticity.


#Although this does not concerns the standard assumption, we still want to check Multicolinearity, which is quite obvious in this data.
attach(dat)
pairs(Calories.Burned~Avg.Speed+Max.Speed)
#pair plot shows that Avg.Speed has correlation with Max.Speed
X=cbind(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),Avg.Speed,Max.Speed)
EngV=eigen(t(X)%*%X)
sqrt(max(EngV$values)/min(EngV$values))
#the conditional number is bigger than 100, thus multicolinearity is pretty serious

