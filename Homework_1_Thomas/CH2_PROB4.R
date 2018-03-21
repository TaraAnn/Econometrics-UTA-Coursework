"Woolridge Chapter 2 C4"
"Use the data in WAGE2 to estimate a simple regression explaining monthly salary (wage) in terms of
IQ score (IQ).
(i)  Find the average salary and average IQ in the sample. What is the sample standard deviation of
     IQ? (IQ scores are standardized so that the average in the population is 100 with a standard deviation
     equal to 15.)
(ii) Estimate a simple regression model where a one-point increase in IQ changes wage by a constant
     dollar amount. Use this model to find the predicted increase in wage for an increase in
     IQ of 15 points. Does IQ explain most of the variation in wage?
(iii)Now, estimate a model where each one-point increase in IQ has the same percentage effect on
     wage. If IQ increases by 15 points, what is the approximate percentage increase in predicted wage?"



# Clear the workspace in R
rm(list=ls(all=TRUE))

#FYI Ctr+L clears the console

#Set the working directory
setwd("C:/Folders/Education/Course Work/Sem 2/Econometrics/Course Material/Homework/Homework 3")

"Following are the packages that we will require to perform operations like OLS, plotting,
 max likelihood application, etc. Now this is just a set of library's we required for our course.
 So there are a lot of library's that wont be used in this code."

install.packages(c("plm","mvtnorm","sandwich","lmtest",
                   "foreign","arm","rms", "readxl", "Hmisc", 
                   "haven", "car", "compactr", "multcomp", "splines"))

# Load libraries
library(mvtnorm)
library(sandwich) #to help get Heteroskedasticity robust standard errors
library(lmtest) #lmtest provides a large collection of diagnostic tests
library(foreign)
library(arm) #to help estimated interaction terms
library(rms) #for OLS regressions
library(readxl) #to read excel files
library(Hmisc) #for adding variable descriptions
library(haven) #need for uploading stata files
library(car) #companion to applied regression
library(compactr)
library(multcomp) #we need this to run the Generalized least squares.  This will help
#performing statistical tests using Robust Standard errors. 
library(plm) #Helps to use the Wald test for joint restrictions on coefficients
library(splines) #if you need to perform a spline regressions

#########################################################################################
#Reading a csv file
#wage2 <- read.csv("wage2.csv")
#wage <- wage2[,1]

wage2 <- read_excel("wage2.xls")
wage <- wage2$wage
# When column names aren't present in the file, R assigns default column names
#wage <- wage2$X__1
#or
#names(wage2)[1]<- "wage"

wage

#part (i)
summary(wage)
#sum(wage)
#length(wage)
#The summary function already gives you the mean. However, if you need to just
#find the mean, then the mean function can be used
mean(wage)

#Manual calculation
#avg_sal <- (sum(sal)/length(sal))
#avg_sal

iq <- wage2$IQ
#or for csv files
#iq <- wage2[,3]
summary(iq)
mean(iq)
#Standard Deviation
sd(iq)

# Part (ii)
"So we want to estimate the following model
where we regress wage on IQ

wage_i = Beta0 + Beta1*iq_i + u_i"

m1 <- lm(wage~iq)
summary(m1)

"Once again, summary gives you everything you need but it just displays the result
and doesn't store any of the coefficients. We can extract the coeffs as follows."
#here are the coefficients
coefficients(m1)
coefficients(m1)[2]
coeffs <- coefficients(m1)
coeffs

"On regressing, we obtain the following fitted model

wagehat_i = 117.597300 + 8.298354*iq_i"


#Determine predicted increase in wage for a 15 point increase in iq
wagehat_delta = coeffs[2]* 15
wagehat_delta 

"In order to determine how much of the variation in wage is explained by IQ,
we need to look at the Rsquare value in the summary we obtained after regression.
Or, we can calculate it manually!"
summary(m1)$r.squared

#Part (iii)
"Well, this is a log-level model. In a log-level model, each one unit increase 
in the explanatory variable has the same percentage effect on the dependent
variable. So we run the following regression."

lnwage <- log(wage) #natural log
# Or we can use the lwage column in our file
m2 <- lm(lnwage~iq)
summary(m2)
coeffs1 <- coefficients(m2)

"And obtain the new estimated model 

ln(wagehat_i) = 5.887301564 + 0.008804767*iq_i"

#Determine predicted percentage increase in wage for a 15 point increase in iq
wagehat_delta_per = coeffs1[2]* 15*100
wagehat_delta_per

