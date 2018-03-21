"Woolridge Chapter 2 C2"

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
                   "haven", "car", "compactr", "multcomp", "splines", "plyr", "S4Vectors"))

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
library(plyr)# To use the count function


#########################################################################################
#Reading a csv file
#ceosal2 <- read.csv("ceosal2.csv")
#salary <- ceosal2[,1]

ceosal2 <- read_excel("ceosal2.xls")
ceosal2
salary <- ceosal2$salary
# When column names aren't present in the file, R assigns default column names
#salary <- ceosal2$X__1
#or
#names(ceosal2)[1]<- "salary"

salary

#part (i)
summary(salary)
#sum(salary)
#length(salary)
#The summary function already gives you the mean. However, if you need to just
#find the mean, then the mean function can be used
mean(salary)

#Manual calculation
#avg_sal <- (sum(salary)/length(salary))
#avg_sal

tenure <- ceosal2$comten
#or for csv files
#tenure <- ceosal2[,5]
summary(tenure)
mean(tenure)

# Part (ii)

#No. of CEO's in the first year as CEO
ceo_tenure <- ceosal2$ceoten
#length(which(ceo_tenure==0))
#sum(ceo_tenure==0)
#count(ceo_tenure==0)
#filter(ceosal2,ceo_tenure==0)
length(subset(ceosal2,ceosal2$ceoten==0)$ceoten)
# Let's break this down. 
"The subset function will return a tibble with all instances of ceosal2 
having ceoten = 0. Now I want to find out how many such instances are there. 
For this i'll use the length function. Using the length function on the result
of subse will give you the no. of columns. So we must extract the ceoten
column from the output of the subset function and then apply the length function
to it. Note: length function applies only to coulumns."

#Longest tenure as CEO
max(ceo_tenure)

#Part (iii)
"So we want to estimate the following model
where we regress log(salary) on ceo_tenure

log(salary_i) = Beta0 + Beta1*ceo_tenure_i + u_i"
# This is a log-level model

lnsalary <- log(salary) #natural log
# Or we can use the lsalary column in our file
#lsalary <- ceosal2$lsalary
m1 <- lm(lnsalary~ceo_tenure)
summary(m1)


"Once again, summary gives you everything you need but it just displays the result
and doesn't store any of the coefficients. We can extract the coeffs as follows."
#here are the coefficients
coefficients(m1)
coefficients(m1)[2]
coeffs <- coefficients(m1)
coeffs

"On regressing, we obtain the following fitted model

log(salaryhat_i) = 6.505497932 + 0.009723636*ceo_tenure_i"


#Determine predicted increase in salary given one more year as CEO
salaryhat_delta_perc = coeffs[2]* 1 *100
salaryhat_delta_perc

