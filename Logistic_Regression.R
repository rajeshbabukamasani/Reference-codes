getwd()
setwd("C:\\Users\\rajesh kamasani\\Desktop\\Stats Test\\Reg Class Exercise in R\\Logistic Regression")


#Regression Excercise.
rm(list=ls()) #remove all the objects

cust_data = read.csv(file = "reg data.csv", header = TRUE,na.strings = "NA")
names(cust_data)
str(cust_data)
summary <-as.data.frame(summary(cust_data))
write.csv(summary,"summary.csv")

data_calc<-function(a,ex_mis=TRUE){
  if (ex_mis) z <-a[!is.na(a)]
  x=sum(z)
  y=mean(z)
  s=sd(z)
  max=max(z)
  min=min(z)
  ucl<- y+3*s
  lcl<- y-3*s 
  
  return(c(mean=y,std=s,max=max, min=min,ucl=ucl,lcl=lcl))
}
#print(names(cust_data))
#print(str(cust_data))
#var1 <- names(cust_data)
vars <- c("RevolvingUtilizationOfUnsecuredLines", "NumberOfTime30.59DaysPastDueNotWorse", "MonthlyIncome","NumberOfTimes90DaysLate",	"NumberOfTime60.89DaysPastDueNotWorse",	"age",	"DebtRatio",	
         "NumberOfOpenCreditLinesAndLoans",	"NumberRealEstateLoansOrLines",	"NumberOfDependents")
diag <-data.frame(apply(cust_data[vars],2,data_calc))
print(diag)
write.csv(diag, "diag.csv",na=".")
str(cust_data)

#Outliers
cust_data$age[cust_data$age>96.6108044]<- 96.6108044
cust_data$RevolvingUtilizationOfUnsecuredLines[cust_data$RevolvingUtilizationOfUnsecuredLines>755.3145499]<- 755.3145499
cust_data$NumberOfTime30.59DaysPastDueNotWorse[cust_data$NumberOfTime30.59DaysPastDueNotWorse>12.9993772]<- 12.9993772
cust_data$DebtRatio[cust_data$DebtRatio>6466.4650758]<- 6466.4650758
cust_data$NumberOfOpenCreditLinesAndLoans[cust_data$NumberOfOpenCreditLinesAndLoans>23.890613]<- 23.890613
cust_data$NumberOfTimes90DaysLate[cust_data$NumberOfTimes90DaysLate>12.7738847]<- 12.7738847
cust_data$NumberRealEstateLoansOrLines[cust_data$NumberRealEstateLoansOrLines>4.407553]<- 4.407553
cust_data$NumberOfTime60.89DaysPastDueNotWorse[cust_data$NumberOfTime60.89DaysPastDueNotWorse>12.7059249]<- 12.7059249
cust_data$MonthlyIncome[cust_data$MonthlyIncome>45311.57]<- 45311.57
cust_data$NumberOfDependents[cust_data$NumberOfDependents>4]<- 4

summary(cust_data)

#missing values

cust_data$MonthlyIncome[is.na(cust_data$MonthlyIncome ==  TRUE)] <- 6460
cust_data$NumberOfDependents[is.na(cust_data$NumberOfDependents ==  TRUE)] <- 1
cust_data$age[cust_data$age==0] <- 52
summary(cust_data)

#########################################Split the data into training & testing data sets
# splitdf function will return a list of training and testing sets
splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/4))
  testset <- dataframe[trainindex, ]
  trainset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

#apply the function
splits <- splitdf(cust_data, seed=1234)

#it returns a list - two data frames called trainset and testset
str(splits)

# there are 112500 and 37500 observations in each data frame
lapply(splits,nrow)

#view the first few columns in each data frame
lapply(splits,head)

# save the training and testing sets as data frames
training <- splits$trainset
testing <- splits$testset

#lm - Linear Model
#glm - Non linear model family of models - binomial


m4 <- glm(SeriousDlqin2yrs ~ 
            #RevolvingUtilizationOfUnsecuredLines +
            NumberOfTime30.59DaysPastDueNotWorse+
            MonthlyIncome+
            NumberOfTimes90DaysLate+
            NumberOfTime60.89DaysPastDueNotWorse+
            age+
            DebtRatio+
            #NumberOfOpenCreditLinesAndLoans+
            NumberRealEstateLoansOrLines+
            NumberOfDependents, data = training, 
          family = binomial(logit))

################################ Logistic Regression output
summary(m4)
coeff<-m4$coef
write.csv(coeff, "coeff.csv")
m4$fitted 
m4$resid 
m4$effects 
anova(m4)
anova(m4, test="Chisq")

##################################### Calculating concordance discordance tied somers'D Gamma

Concordance=function(GLM.binomial){
outcome_and_fitted_col=cbind(GLM.binomial$y,GLM.fitted.values)
#get a subset of outcome where the event actually happend
ones=outcome_and_fitted_col[outcome_and_fitted_col[,1] ==1,]
#get a subset of outcome where the event didn't actually happen
zeros=outcome_and_fitted_col[outcome_and_fitted_col[,1]==0,]
#Equate the length of event and non-event tables
if(length(ones[,1]>length(zeros[,1])) {ones = ones[1:length(zeros[,1]),]}
else{zeros=zeros[1:length(ones[,1]),]}
#Following will be c(ones_outcome,ones_fitted,zeros_outcome,zeros_fitted)
ones_and_zeros=data.frame(ones,zeros)
#Initiate columns to store concordance Discordance and Tie pair evalutaions
con=rep(NA,length(ones_and_zeros[,1]))
dis=rep(NA,length(ones_and_zeros[,1]))
tie=rep(NA,length(ones_and_zeros[,1]))
for (i in 1:length(ones_and_zeros[,1])){
#This tests for concordance
if(ones_and_zeros[i,2]>ones_and_zeros[i,4])
{con[i]=1
disc[i]=0
ties[i]=0}
#This tests for tie
else .if(ones_and_zeros[i,2] == ones_and_zeros[i,4])
{con[i]=0
disc[i]=0
ties[i]=1}
#This should catch discordant pairs
else .if(ones_and_zeros[i,2]<ones_and_zeros[i,4])
{con[i]=0
disc[i]=1
ties[i]=0}
}
#Here we save various rates
conc_rate=mean(conc,na.rm=TRUE)
disc_rate=mean(disc,na.rm=TRUE)
tie_rate =mean(tie,na.rm=TRUE)
somers_D <- conc_rate-disc_rate
gamma <- (conc_rate - disc_rate)/(conc_rate + disc_rate)
#K_tau_a<-2*(sum(conc)-sum(disc))/(N*(N-1))
return(list(Concordance=conc_rate,num_concordant=sum(conc)
            Discordance=disc_rate,num_discordant=sum(disc)
			Somers_D=Somers_D,Gamma=gamma))
}

concordance(m4)

############################### SCoring Data sets#####################################
training$lf<-(-1.550861722  +
                training$NumberOfTime30.59DaysPastDueNotWorse*0.517499324860115	+
                training$MonthlyIncome*-3.67802129183398E-05	+
                training$NumberOfTimes90DaysLate*0.594423910917967	+
                training$NumberOfTime60.89DaysPastDueNotWorse*0.140030284580349	+
                training$age*-0.0273162037853039	+
                training$DebtRatio*-3.56327261566838E-05	+
                training$NumberRealEstateLoansOrLines*0.0549799420103525	+
                training$NumberOfDependents*0.0888057624298761	)
training$odds <-exp(training$lf)
training$prob <- training$odds/(1+training$odds)

testing$lf<-(-1.550861722  +
               testing$NumberOfTime30.59DaysPastDueNotWorse*0.517499324860115  +
               testing$MonthlyIncome*-3.67802129183398E-05	+
               testing$NumberOfTimes90DaysLate*0.594423910917967	+
               testing$NumberOfTime60.89DaysPastDueNotWorse*0.140030284580349	+
               testing$age*-0.0273162037853039	+
               testing$DebtRatio*-3.56327261566838E-05	+
               testing$NumberRealEstateLoansOrLines*0.0549799420103525	+
               testing$NumberOfDependents*0.0888057624298761	)
testing$odds <-exp(testing$lf)
testing$prob <- testing$odds/(1+testing$odds)

#################### Creating Deciles####################################
# find the decile locations 
decLocations <- quantile(training$prob, probs = seq(0.1,0.9,by=0.1))
# use findInterval with -Inf and Inf as upper and lower bounds
training$decile <-findInterval(training$prob,c(-Inf,decLocations, Inf))

summary(training$decile)
xtabs(~decile,training)

# find the decile locations 
decLocations <- quantile(testing$prob, probs = seq(0.1,0.9,by=0.1))
# use findInterval with -Inf and Inf as upper and lower bounds
testing$decile<-findInterval(testing$prob,c(-Inf,decLocations, Inf))

summary(testing$decile)
xtabs(~decile,testing)

write.csv(training,"training.csv")
write.csv(testing,"testing.csv")

##################################Decile Analysis Reports

require(sqldf)
Training_DA <- sqldf("select decile,  min(Prob) as Min_prob, max(Prob) as max_prob,  sum(seriousdlqin2yrs) as Bad_cnt, 
                           (count(decile)-sum(seriousdlqin2yrs)) as Good_cnt from training
                           group by decile
                           order by decile desc")
Testing_DA <- sqldf("select decile,  min(Prob) as Min_prob, max(Prob) as max_prob,  sum(seriousdlqin2yrs) as Bad_cnt, 
                           (count(decile)-sum(seriousdlqin2yrs)) as Good_cnt from testing
                           group by decile
                     order by decile desc")

write.csv(Training_DA,"Training_DA.csv")
write.csv(Testing_DA,"Testing_DA.csv")


## Summary of the model
summary(m4)

#One measure of model fit is the significance of the overall model. This test asks whether the model 
#with predictors fits significantly better than a model with just an intercept (i.e., a null model). 
#The test statistic is the difference between the residual deviance for the model with predictors and the
#null model. The test statistic is distributed chi-squared with degrees of freedom equal to the differences 
#in degrees of freedom between the current and the null model (i.e., the number of predictor variables in the model).

#To find the difference in deviance for the two models (i.e., the test statistic) we can use the command:
with(m4, null.deviance - deviance)

#The degrees of freedom for the difference between the two models is equal to the number of
#predictor variables in the mode, and can be obtained using:
with(m4, df.null - df.residual)

#Finally, the p-value can be obtained using:
with(m4, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

#The chi-square of 7187 with 8 degrees of freedom and an associated p-value of 
#less than 0.001 tells us that our model as a whole fits significantly better than an empty model.
#This is sometimes called a likelihood ratio test (the deviance residual is -2*log likelihood). 
#To see the model's log likelihood, we type:
logLik(m4)



## Beta coefficiants
coef(m4)

## odds ratios only
exp(coef(m4))

## Confidence Intervals
#confint(m4)
#exp(confint(m4))

## odds ratios and 95% CI
exp(cbind(OR = coef(m4), confint(m4)))

## Wald test
wald.test(b = coef(m4), Sigma = vcov(m4), Terms = 4:6)

backwards = step(m4)
formula(backwards)
summary(backwards)
