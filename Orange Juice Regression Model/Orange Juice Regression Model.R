# Title : Regression Model
# Problem Statement: To evaluate sale performances for orange juice brands at 3 different price points
# By: mikportfolio, 090224 1220PM


# Install packages: tidyverse, dplyr
install.packages("tidyverse")
install.packages("dplyr")
library(tidyverse)
library(dplyr)


############################
## Part 1: Import dataset ##
############################

#  Import data set "oj"
Orange_Juice <- read.csv("D://BDS Datasets//oj.csv")
View(Orange_Juice)

############################
## Part 2: Clean data     ##
############################
#  Level brands
Orange_Juice$brand <- factor(Orange_Juice$brand)
levels(Orange_Juice$brand)
head(Orange_Juice)

#  Inspect all prices by brand
summarise(Orange_Juice %>% group_by(brand), 
          avg_price = mean(price))

############################
## Part 3: Relationship   ##
############################
#  Assign colours by brand
brandcol <- c("yellow","blue","red") #"yellow, blue, and red from cheapest to most expensive"

#  Create frame for visualizations
par(mfrow=c(1,2))                    # create visualizations in one row, two columns

#  Plot data
plot(log(price) ~ brand, data=Orange_Juice, col=brandcol)
plot(log(sales) ~ log(price), data=Orange_Juice, col=brandcol[Orange_Juice$brand])


############################
## Part 4: Regression     ##
############################
reg = glm(log(sales) ~ log(price) + brand, data=Orange_Juice)
summary(reg)       ## coeff, tests, fit
coef(reg)          ## just coefficients

x <- model.matrix( ~ log(price) + brand, data=Orange_Juice)
x[c(100,200,300),]
Orange_Juice[c(100,200,300),]

reg_interact <- glm(log(sales) ~ log(price)*brand,
                    data = Orange_Juice)
coef(reg_interact)

beta <- coef(reg)
beta


###################################
## Part 5: No Interactions       ##
###################################
plot(log(sales) ~ log(price), 
     data=Orange_Juice, 
     col=brandcol[Orange_Juice$brand], 
     cex=.5, 
     pch=20, 
     bty="n")
abline(a=beta[1], 
       b=beta[2], 
       col=brandcol[1], 
       lwd=2)
abline(a=beta[1]+ beta[3], 
       b=beta[2], 
       col=brandcol[2], 
       lwd=2)
abline(a=beta[1]+beta[4], 
       b=beta[2], 
       col=brandcol[3], 
       lwd=2)
legend("bottomleft", 
       bty="n", 
       lwd=2, 
       col=brandcol, 
       legend=levels(Orange_Juice$brand))

ojreg <- glm(log(sales) ~ log(price)*brand*feat, data=Orange_Juice)
coef(ojreg)


###################################
## Part 6: 2-Way Interactions    ##
###################################
reg_interact = glm(log(sales) ~ log(price)*brand, data=Orange_Juice) #Note: "`*'" adds the main effects automatically
coef(reg_interact) # compare brand-specific log(price) slopes to our earlier elasticity (-3.1)
beta <- coef(reg_interact)

plot(log(sales) ~ log(price), 
     data=Orange_Juice, 
     col=brandcol[Orange_Juice$brand], 
     cex=.1, 
     pch=20, 
     bty="n")
abline(a=beta[1], 
       b=beta[2], 
       col=brandcol[1], 
       lwd=2)
abline(a=beta[1]+beta[3], 
       b=beta[2]+beta[5], 
       col=brandcol[2], 
       lwd=2)
abline(a=beta[1]+beta[4], 
       b=beta[2]+beta[6], 
       col=brandcol[3], 
       lwd=2)
legend("bottomleft", 
       bty="n", 
       lwd=2, 
       col=brandcol, 
       legend=levels(Orange_Juice$brand))

###################################
## Part 7: 3-Way Interactions    ##
###################################
ojreg <- glm(log(sales) ~ log(price)*brand*feat, data=Orange_Juice)
coef(ojreg)


###################################
## Part 8: Elasticity Tables     ##
###################################
b <- coef(ojreg)
b["log(price)"] 
b["log(price)"] + b["log(price):brandminute.maid"]
b["log(price)"] + b["log(price):brandtropicana"]
b["log(price)"] + b["log(price):feat"] 
b["log(price)"] + b["log(price):brandminute.maid"] + b["log(price):feat"] + b["log(price):brandminute.maid:feat"]
b["log(price)"] + b["log(price):brandtropicana"] + b["log(price):feat"] + b["log(price):brandtropicana:feat"]

salestable <- tapply(Orange_Juice$sales, Orange_Juice[,c("feat","brand")], sum)
mosaicplot(salestable,col=brandcol)



###################################
## Part 9: Prediction            ##
###################################
## create some data for prediction, using the data.frame function
## Note: the care in specifying brand factor (levels must match original data)
## we don't need all variables in Orange_Juice; just those used as covariates in reg.
newdata=data.frame(price=rep(4,3),
                   brand=factor(c("tropicana",
                                  "minute.maid",
                                  "dominicks"),
                                levels=levels(Orange_Juice$brand)))
## Predict
predict(reg, newdata=newdata)  ## predicted log units moved
exp(predict(reg, newdata=newdata)) ## predicted # of units moved