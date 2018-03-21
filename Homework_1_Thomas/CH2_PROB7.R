"Woolridge Chapter 2 C7"


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
#charity <- read.csv("charity.csv")
#gift <- charity[,2]

charity <- read_excel("charity.xls")
gift <- charity$gift
# When column names aren't present in the file, R assigns default column names
#gift <- charity$X__1
#or
#names(charity)[1]<- "gift"

gift

#part (i)
summary(gift)
#sum(gift)
#length(gift)
#The summary function already gives you the mean. However, if you need to just
#find the mean, then the mean function can be used
mean(gift)

#Manual calculation
#avg_gift <- (sum(gift)/length(gift))
#avg_gift

# Determine %age of people who gave no gift
no_gift <- length(subset(charity,gift==0)$gift)
# Let's break this down. 
"The subset function will return a tibble with all instances of ceosal2 
having ceoten = 0. Now I want to find out how many such instances are there. 
For this i'll use the length function. Using the length function on the result
of subset alone will give you the no. of columns. So we must extract the ceoten
column from the output of the subset function and then apply the length function
to it"
(no_gift/length(gift))*100

# Part (ii)

mailsyear <- charity$mailsyear
#or for csv files
#mailsyear <- charity[,6]
summary(mailsyear)
mean(mailsyear) # average mails per year
max(mailsyear)
min(mailsyear)


#Part (iii)
"So we want to estimate the following model
where we regress gift on mailsyear

gift_i = Beta0 + Beta1*mailsyear_i + u_i"

m1 <- lm(gift~mailsyear)
summary(m1)

"Once again, summary gives you everything you need but it just displays the result
and doesn't store any of the coefficients. We can extract the coeffs as follows."
#here are the coefficients
coefficients(m1)
coefficients(m1)[2]
coeffs <- coefficients(m1)
coeffs

"On regressing, we obtain the following fitted model

gifthat_i = 2.014080 + 2.649546*mailsyear_i"

"In order to determine how much of the variation in wage is explained by IQ,
we need to look at the Rsquare value in the summary we obtained after regression.
Or, we can calculate it manually!"
summary(m1)$r.squared

length(gift) # Sample size


#Part(iv)
# Interpret the slope coefficient
gifthat_delta <- coeffs[2]* 1
gifthat_delta
# Beta1 is the predicted change(increase in this case) in gift when mailsyear increases by one unit.
#Since Beta1 is positive, each mail will have a net gain of 2.65 on charity

#Part(v)
#Determine the smallest predicted charitable contribution in the sample
#Determine predicted increase in wage for a 15 point increase in iq
gifthat_delta_min <- coeffs[1] + coeffs[2]* 0
gifthat_delta_min

"This means that even if there are no mails sent in a year the charity will 
have a gain. Obviously this makes no sense. Thus using this model we can never
predict zero for gift"


