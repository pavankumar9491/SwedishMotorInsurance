##Read the Swedish motor insurance data
#data can be downloaded from http://instruction.bus.wisc.edu/jfrees/jfreesbooks/Regression%20Modeling/BookWebDec2010/data.html

insurance.data <- read.csv("D:/STUDIES/Big Data/R/Projects_for_R/Insurance/SwedishMotorInsurance.csv")
head(insurance.data)

##Descriptive analysis of each field to gain basic insights of the data collected and for further analysis.

summary(insurance.data)
summary(as.factor(insurance.data$Kilometres))

summary(as.factor(insurance.data$Zone))

summary(as.factor(insurance.data$Bonus))

summary(as.factor(insurance.data$Make))

summary(insurance.data$Insured)

summary(insurance.data$Claims)

summary(insurance.data$Payment)
# Observe that the minimum value is not 0 for Insured means Some car of some zone has been 
#insured for given time where as no claims or payements were made the minimum value is 0 for them

## Find whether the total value of the payment is related to the no.of claims& no.of policy years.
#perform analysis of variance to find the relation
payment.model <- aov(insurance.data$Payment~insurance.data$Claims+insurance.data$Insured)
summary(payment.model)
plot(payment.model)

#perform correlation to find relationship of claims&insured with payment variable
cor(insurance.data$Claims,insurance.data$Payment)
cor(insurance.data$Insured,insurance.data$Payment)
plot(insurance.data$Claims,insurance.data$Payment)
plot(insurance.data$Insured,insurance.data$Payment)

##Find whether distance, location, bonus, make and insured amount or claims are -
##affecting the payment or all of these are affecting it.

insurance.reg.model <- lm(Payment~Kilometres+Zone+Bonus+Make+Insured+Claims, data = insurance.data)
summary(insurance.reg.model)

##To establish new branch office, find at what location, kilometer and bonus level their insured amount, -
##claims, and payment get increased(Hint: Aggregate DataSet)

app1 <- apply(insurance.data[,c(5,6,7)],2,function(x) tapply(x, insurance.data$Kilometres,mean))
app2 <- apply(insurance.data[,c(5,6,7)],2,function(x) tapply(x, insurance.data$Zone,mean))
app3 <- apply(insurance.data[,c(5,6,7)],2,function(x) tapply(x, insurance.data$Bonus,mean))
app1  # Payments and Claims are high for group 2
app2  # Zone 4 has the highest number of Claims thus Payments as well
app3  # There is not much variation in groups of bonus except 7 with unusal high number of insured years,claims&payments
#ZONES 1-4 have more inusred years, claims and payments

##Find whether the insured amount,zone, kilometer, bonus or make affects claim rates and to what extent

claim.model <- lm(Claims~Insured+Zone+Kilometres+Bonus+Make , data = insurance.data)
claim.model
summary(claim.model)

