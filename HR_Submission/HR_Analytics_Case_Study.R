# HR Analytics Case Study
# Steps are Business understanding, Data prep & understanding (including EDA), Model building & Model evaluation followed by summarising the learnings and plan

# Load Libraries
library(MASS)
library(car)
library(stringr)
library(tidyr)
library(dplyr)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)

# Start by importing the data sets
empsurveydata <- read.csv("employee_survey_data.csv", stringsAsFactors = F)
mgrsurveydata <- read.csv("manager_survey_data.csv", stringsAsFactors = F)
intime <- read.csv("in_time.csv", stringsAsFactors = F)
outtime <- read.csv("out_time.csv", stringsAsFactors = F)
generaldata <- read.csv("general_data.csv", stringsAsFactors = F)

str(empsurveydata)
View(empsurveydata) # Contains  ordinal variables around emp satisfaction and the employee ID which could be the unique identifier. We'll check for duplicates to validate this.
str(mgrsurveydata) 
View(mgrsurveydata) # Contains  ordinal variables around the manager's rating of the employee and the employee ID which could be the unique identifier. We'll check for duplicates to validate this.
str(intime)
View(intime) # Contains "IN" swipe data for a period of a year, for working days. Has a huge number of columns since it is in wide format
str(outtime)
View(outtime) # Contains "OUT" swipe data for a period of a year, for working days. Has a huge number of columns since it is in wide format
str(generaldata) 
View(generaldata) # Contains a mix of employee data with varibaales that are numeric, ordinal & categorical and includes the taget variable, which is attrition

# All seem to have 4410 observations and an Employee ID field which could be the unique identifier (As X in the in and out time files). Now let's check if there are any duplicates for employee ID

length(unique(empsurveydata$EmployeeID)) # no duplicates
length(unique(mgrsurveydata$EmployeeID)) # no duplicates
length(unique(generaldata$EmployeeID)) # no duplicates
length(unique(intime$X)) # no duplicates
length(unique(outtime$X)) # no duplicates

# Check for difference in employee IDs
setdiff(generaldata$EmployeeID,empsurveydata$EmployeeID) # no differences
setdiff(generaldata$EmployeeID,mgrsurveydata$EmployeeID) # no differences
setdiff(generaldata$EmployeeID,intime$X) # no differences
setdiff(generaldata$EmployeeID,outtime$X) # no differences


# We have 4410 unique entries present across so we can merge them using the employee ID
# Let's start with general data, employee survery data and manager survey data as the time files have a huge number of rows so we can process them separately and then add the relevant columns back in

masterdata <- merge(generaldata,empsurveydata, by = "EmployeeID", all = F)
masterdata <- merge(masterdata,mgrsurveydata, by = "EmployeeID", all = F )
View(masterdata)

# For the in and out times, rather than adding all the data into the master set, let's think about how we can use this and what we can derive from it so that we can simplify and then import what we require 
# Since we do not have details around shift timings of employees, we amy not be able to look at late login/early logout, etc. We can though get the daily logged hours by getting the difference of In Vs Out
# We can also get total working days for that year (where In & Out times have been captured)

# Let's calculate these before moving forward
blankdatesin <-sapply(intime, function(y) sum(length(which(is.na(y)))))
View(blankdatesin) # We see many dates have all values as blank which means those were probably holidays. LEt's identify those first and match it with the out time as well
blankdatesin[which(blankdatesin == 4410)] # There are 12 dates. LEt's compare with out time
blankdatesout <-sapply(outtime, function(y) sum(length(which(is.na(y)))))
blankdatesout[which(blankdatesout == 4410)] # The dates are exactly the same so let's drop those from our data set

# Let's remove these 12 dates from our in and out time data sets
intime <- intime[, colSums(is.na(intime)) != nrow(intime)]
View(intime)
outtime <- outtime[, colSums(is.na(outtime)) != nrow(outtime)]
View(outtime)
sum(is.na(intime))
sum(is.na(outtime))
# We have the same number of NAs in both data sets. What we can do is check if the NAs match for each employee. That will indicate that the employee would have been on leave on those days where both are NA
intime$leavedaysin <- rowSums(is.na(intime))
View(intime$leavedaysin)
outtime$leavedaysout <- rowSums(is.na(outtime))
View(outtime$leavedaysout)
setdiff(intime$leavedaysin,outtime$leavedaysout)
# We can see that this is exactly the same so those days are leaves that those employees have taken, which is now a derived metric for us. We can add this back to the masterdata
# First match column names
colnames(intime)
colnames(intime)[1] <- "EmployeeID"
colnames(outtime)[1] <- "EmployeeID"
leave <- intime[,c(1,251)]
View(leave)
masterdata <- merge(masterdata,leave, by = "EmployeeID")
View(masterdata) # Leave days are now added to this 

colnames(intime)
colnames(outtime)

intime <- intime[,-251]
outtime <- outtime[,-251]
setdiff(colnames(intime),colnames(outtime)) # We have the same columns

# What we also need is the difference of in and out times to give us logged hours

# Now let's check the formats of the values in the time data sets to calculate this
str(intime) # All character so we need to convert them to date
colnames(intime)
intime[,2:250] <- lapply(intime[,2:250],function(x) as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", origin = "1970-01-01 00:00:00"))
View(intime)
str(intime) # All converted to datetime format

# Repeat same for outtime
str(outtime) # All character so we need to convert them to date
colnames(outtime)
outtime[,2:250] <- lapply(outtime[,2:250],function(x) as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", origin = "1970-01-01 00:00:00"))
View(outtime)
str(outtime) # All converted to datetime format

# Now that we have converted the time, let's get the time difference
timediff <- outtime[,2:250] - intime[,2:250] # Got the difference and now add back emp ID
timediff <- cbind(intime$EmployeeID,timediff) # Add back the Emp id field
colnames(timediff)[1] <- "EmployeeID"
str(timediff) 
# Convert this to a numeric value
timediff[,2:250] <- lapply(timediff[,2:250],function(x) round(as.numeric(x),2))
str(timediff)
View(timediff) # We have now managed to get the daily number of login hours rounded off to 2 digits
# We know as per the data set that the standard login hours is 8
# Daily login hours is too difficult to deal with individually and won't tell us much as it'll vary by day so we need to derive better metrics
# We can get total hours by employee as well as average hours per day. We know that if an employee is on leave, the value will be NA so we will take average by employee of non-NAs

timediff$totalworked <- rowSums(!is.na(timediff)) # to get total working days per employee
timediff$totalhours <- rowSums(timediff[,2:250], na.rm = T) # To get total hours per employee
summary(timediff$totalworked) # It seems like we broadly have all employess supposed to be working for same number of days (except leaves they took) which means they were on rolls for the whole year and ideally should have clocked 8 hours into the expected number of working days
summary(timediff$totalhours) # The max value is quite high

# Let's calculate the average working hours per individual for the days they worked as that would be a better metric to use and since we already derived the number of leaves earlier, we needn't use the working days again
timediff$avglogin <- timediff$totalhours/timediff$totalworked
timediff$avglogin <- round(timediff$avglogin,2)
summary(timediff$avglogin)
colnames(timediff)

boxplot(timediff$avglogin) # There are quite a few outliers but since this could be a key factor in predicting attrition, we'll deal with it once we look at the data set as a whole

# Let's now add average login hours by employee into the master DF
logintime <- timediff[,c(1,253)]
masterdata <- merge(masterdata,logintime,by = "EmployeeID")
View(masterdata)

# We now have all our data in one table
# Now let's begin looking at the data in the masterframe and prep it for exploration

str(masterdata) # 31 variables
# Attrition is set as character, we need to change that to factor - same with Business Travel, Department, EducationField, Gender, JobRole, MaritalStatus, Over18
# Change to factor and convert levels where needed
masterdata$Attrition <- as.factor(masterdata$Attrition)
summary(masterdata$Attrition) # Two levels so let's change this to 0 and 1
levels(masterdata$Attrition) <- c(0,1) # Levels set
masterdata$BusinessTravel <- as.factor(masterdata$BusinessTravel)
summary(masterdata$BusinessTravel) # 3 categories
masterdata$Department <- as.factor(masterdata$Department)
summary(masterdata$Department) # # levels
masterdata$EducationField <- as.factor(masterdata$EducationField)
summary(masterdata$EducationField) # 6 levels
masterdata$Gender <- as.factor(masterdata$Gender)
summary(masterdata$Gender) # 2 levels so we can change to 0 and 1
levels(masterdata$Gender) <- c(0,1) # Levels set
masterdata$JobRole <- as.factor(masterdata$JobRole)
summary(masterdata$JobRole) # 9 levels
masterdata$MaritalStatus <- as.factor(masterdata$MaritalStatus)
summary(masterdata$MaritalStatus) # 3 levels
masterdata$Over18 <- as.factor(masterdata$Over18)
summary(masterdata$Over18) # just has 1 level

# Check summary of data again
str(masterdata) # We notice that some fields just have one value so let's see if we need to eliminate those as they won't help with the model
length(unique(masterdata$EmployeeCount)) # This has just one value so we can eliminate it
summary(masterdata$Over18) # This has just one value so we can eliminate it
length(unique(masterdata$StandardHours)) # This has just one value as well so we can eliminate it
colnames(masterdata)
# Eliminate unnecessary columns identified
masterdata <- masterdata[,-c(9,16,18)]
str(masterdata) # We are now down to 28 variables

# We also have variables which are ordered so we can covert those to factor as well
masterdata$Education <- as.factor(masterdata$Education)
masterdata$JobLevel <- as.factor(masterdata$JobLevel)
masterdata$StockOptionLevel <- as.factor(masterdata$StockOptionLevel)
masterdata$EnvironmentSatisfaction <- as.factor(masterdata$EnvironmentSatisfaction)
masterdata$JobSatisfaction <- as.factor(masterdata$JobSatisfaction)
masterdata$WorkLifeBalance <- as.factor(masterdata$WorkLifeBalance)
masterdata$JobInvolvement <- as.factor(masterdata$JobInvolvement)
masterdata$PerformanceRating <- as.factor(masterdata$PerformanceRating)


# Check for Blanks & NAs
length(which(masterdata == "")) # No blank values
sum(is.na(masterdata)) # 111 NA values in master data. Let's identify where we have this and decide how to deal with it

# Separate out the rows with NA values to identify how to deal with them
masterna <- masterdata[rowSums(is.na(masterdata)) >0,]
length(unique(masterna$EmployeeID)) # 110 employees have NA values
# Where are the NAs?
masternacount <- sapply(masterna, function(x) sum(is.na(x)))
# 38 NAs for work life balance, 25 for environment satisfaction, 20 for job satisfation, 19 for number of companies worked for and 9 for total working years
# Just 110 employees out of 4410 in total, so less than 2.5% of values but let's check the attrition data for these
summary(masterdata$Attrition) # Approx 16% attrition
summary(masterna$Attrition) # 17% attrition for this subset so seems in line with the arger data set. Also, 2.25% of attrites so we could take a call to eliminate these rows since they shouldn't affect the overall prediction

masterdata <-  masterdata[rowSums(is.na(masterdata)) == 0,]
str(masterdata) # Down to 4300 observations
sum(is.na(masterdata)) # Revalidating 	to make sure we have no NA values

# Begin exploration
str(masterdata)

# Look at overall attrition
ggplot(masterdata,aes(Attrition,fill=Attrition))+geom_bar() 
prop.table(table(masterdata$Attrition))# 16.16% Attrition

# Comparison on categorical variables
ggplot(masterdata,aes(BusinessTravel,fill=Attrition))+geom_bar() # High percentage of people who travel, attrite and this reduces as the amount of travel drops to none
ggplot(masterdata,aes(Department,fill = Attrition))+geom_bar() # Attrition for the HR department seems pretty high, though the number of employees is quite low
ggplot(masterdata,aes(EducationField,fill=Attrition))+geom_bar() # Percentage wise we can see heavy attrition among thsoe who study Human Resources (also aligned with what we saw above), though volumes are driven by those studying Medicine or Life Sciences
ggplot(masterdata,aes(Gender,fill=Attrition))+geom_bar() # Attrition is slighlty higher for male employees
ggplot(masterdata,aes(JobRole,fill=Attrition))+geom_bar() # Sales, Lab techs and research scientists drive a large number of attrition volumes
ggplot(masterdata,aes(MaritalStatus,fill=Attrition))+geom_bar() # Those who are single are highly likely to attrite while those who are divorced are least likely to, in comparison
ggplot(masterdata,aes(Education,fill=Attrition))+geom_bar() # %age of those who have only gone to college, seems higher for attrition
ggplot(masterdata,aes(EnvironmentSatisfaction,fill=Attrition))+geom_bar() # Higher %age of attrition for those who have a low rating for work environment
ggplot(masterdata,aes(JobInvolvement,fill=Attrition))+geom_bar() # High %age attrite if their involvment is low but even those with a very high involvement, have the next highest %age of attrition
ggplot(masterdata,aes(JobSatisfaction,fill=Attrition))+geom_bar() # Very high likelihood of attrition if the job satisfaction is low and less if it is high
ggplot(masterdata,aes(PerformanceRating,fill=Attrition))+geom_bar() # Higher performance rating has a slightly higher chance of attrition
ggplot(masterdata,aes(JobLevel,fill=Attrition))+geom_bar() # Job Level 2 seems to have a higher attrition rate
ggplot(masterdata,aes(StockOptionLevel,fill=Attrition))+geom_bar() # Stock Option Level 2 seems to have a higher attrition rate
ggplot(masterdata,aes(WorkLifeBalance,fill=Attrition))+geom_bar() # Attrition is extremely high for those who rate work life balance low

# Comparision on numeric variables
str(masterdata)
ggplot(masterdata,aes(Age,fill=Attrition))+geom_density() + facet_grid(~Attrition)# The attrition for really young employees (18 to 20) seems to be very high, though in terms of volumes, most of those who attrite seem to be aged between 25 to 35
ggplot(masterdata,aes(DistanceFromHome,fill=Attrition))+geom_bar()# Percentage is high for those who live 15-20km from the office
ggplot(masterdata,aes(MonthlyIncome,fill=Attrition))+geom_density() + facet_grid(~Attrition) # Percentage is high for those who have very low salaries and also where the salaries are around 150-160k
ggplot(masterdata,aes(NumCompaniesWorked,fill=Attrition))+geom_density()  # Percentage is high for those who have switched 4 to 7 companies and also high if they have switched just once or never
ggplot(masterdata,aes(PercentSalaryHike,fill=Attrition))+geom_density() + facet_grid(~Attrition) # Percentage seems to climb as the percentage increases which seems slightly counter-intuitive
ggplot(masterdata,aes(TotalWorkingYears,fill=Attrition))+geom_density() + facet_grid(~Attrition) # Attrition rate seems to be very high for those who have started working less than 3 years ago and then seems to dip as experience increases, except at the extreme end which could possibly due to retirement or voluntary retirement
ggplot(masterdata,aes(TrainingTimesLastYear,fill=Attrition))+geom_density() + facet_grid(~Attrition) # Attrition rate seems to be lower when more time has been spent in training
ggplot(masterdata,aes(YearsAtCompany,fill=Attrition))+geom_density() + facet_grid(~Attrition) # Higher attrition if they have been with the org for less than 4 years or very high tenure
ggplot(masterdata,aes(YearsSinceLastPromotion,fill=Attrition))+geom_density() + facet_grid(~Attrition) # Attrition is high when it's been less than 4 years since last promotion and seems to peak between 6 to 8 years. It is also high if it has been over 13 years
ggplot(masterdata,aes(YearsWithCurrManager,fill=Attrition))+geom_density() + facet_grid(~Attrition) # Highest likelihood indicated if they have been with the same manager for less than 2 years
ggplot(masterdata,aes(leavedaysin,fill=Attrition))+geom_density() + facet_grid(~Attrition) # Those who take very few leaves (less than 5) seem to have a higher attrition percentage, followed by those who take around 8 to 10 days
ggplot(masterdata,aes(avglogin,fill=Attrition))+geom_density() + facet_grid(~Attrition) # Those who are overworked and log in more than 8 hours seem to attrite more

# We have got a good amount of insight from this data with indicators of what could be driving attrition
# We can now get to model building but before that, let's look at our numeric variables to identify if we need to treaat outliers and scale the data
str(masterdata)
quantile(masterdata$Age,seq(0,1,.01))
boxplot(masterdata$Age) # Can leave as is since there no outliers standing out
quantile(masterdata$DistanceFromHome,seq(0,1,.01))
boxplot(masterdata$DistanceFromHome) # Can leave as is since there no outliers standing out
quantile(masterdata$MonthlyIncome,seq(0,1,.01))
boxplot(masterdata$MonthlyIncome) # Let's cap it at 163280.0
masterdata$MonthlyIncome[which(masterdata$MonthlyIncome > 163280.0)] <- 163280.0
quantile(masterdata$NumCompaniesWorked,seq(0,1,.01))
boxplot(masterdata$NumCompaniesWorked) # Can cap this at 8
masterdata$NumCompaniesWorked[which(masterdata$NumCompaniesWorked > 8)] <- 8
quantile(masterdata$PercentSalaryHike,seq(0,1,.01))
boxplot(masterdata$PercentSalaryHike) # Can leave as is
quantile(masterdata$TotalWorkingYears,seq(0,1,.01))
boxplot(masterdata$TotalWorkingYears) # Can cap at 29
masterdata$TotalWorkingYears[which(masterdata$TotalWorkingYears > 29)] <- 29
quantile(masterdata$TrainingTimesLastYear,seq(0,1,.01))
boxplot(masterdata$TrainingTimesLastYear) # Set min as 1 and max as 5
masterdata$TrainingTimesLastYear[which(masterdata$TrainingTimesLastYear > 5)] <- 5
masterdata$TrainingTimesLastYear[which(masterdata$TrainingTimesLastYear < 1)] <- 1
quantile(masterdata$YearsAtCompany,seq(0,1,.01))
boxplot(masterdata$YearsAtCompany) # Can cap at 20
masterdata$YearsAtCompany[which(masterdata$YearsAtCompany > 20)] <- 20
quantile(masterdata$YearsSinceLastPromotion,seq(0,1,.01))
boxplot(masterdata$YearsSinceLastPromotion) # Can cap at 8
masterdata$YearsSinceLastPromotion[which(masterdata$YearsSinceLastPromotion > 8)] <- 8
quantile(masterdata$YearsWithCurrManager,seq(0,1,.01))
boxplot(masterdata$YearsWithCurrManager) # Cap at 14
masterdata$YearsWithCurrManager[which(masterdata$YearsWithCurrManager > 14)] <- 14
quantile(masterdata$leavedaysin,seq(0,1,.01))
boxplot(masterdata$leavedaysin) # Leave as is
quantile(masterdata$avglogin,seq(0,1,.01))
boxplot(masterdata$avglogin) # Let's leave this as is since there isn't that much of a difference

# We can create another derived metric as we know that standard login hours are 8 - this could be login hour variance which will be the difference between standard hours and avg logged hours
masterdata$loginvar <- round(8 - masterdata$avglogin, 2)
summary(masterdata$loginvar) # Range of -2.98 to + 2.08 indicating that some employees have logged in almost 3 hours less than the required time and some have spent over 2 hours more than the required time

# Check for correlation
library(corrplot)
colnames(masterdata)
mastercorr <- masterdata[,c(2,6,13,14,15,17,18,19,20,21,27,28,29)]
res<- cor(mastercorr)
corrplot(res)

# We can see heavy correlation between time based factors so we will have to watch out for this in our model
# Also, we can see the clear coorelation of avglogin and loginvar since it is a derived metric. We can drop one of these so let's drop avglogin
masterdata <- masterdata[,-28]

# Further look at data simpification and dummay variables
str(masterdata) # We see that performance rating has only 2 levels so we can make this 0 and 1
summary(masterdata$PerformanceRating) # So rating 3/Excellent will become 0 and rating 4/Outstanding will become 1
levels(masterdata$PerformanceRating) <- c(0,1)

##### Tested model with and without scaling the variables and we got the same predictors but with a stronger model without scaling so we will avoid scaling here
# Data has already undergone outlier treatment
# Scale all numeric variables
#masterdata$Age <- scale(masterdata$Age)
#masterdata$DistanceFromHome <- scale(masterdata$DistanceFromHome)
#masterdata$MonthlyIncome <- scale(masterdata$MonthlyIncome)
#masterdata$NumCompaniesWorked <- scale(masterdata$NumCompaniesWorked)
#masterdata$PercentSalaryHike <- scale(masterdata$PercentSalaryHike)
#masterdata$TotalWorkingYears <- scale(masterdata$TotalWorkingYears)
#masterdata$TrainingTimesLastYear <- scale(masterdata$TrainingTimesLastYear)
#masterdata$YearsAtCompany <- scale(masterdata$YearsAtCompany)
#masterdata$YearsSinceLastPromotion <- scale(masterdata$YearsSinceLastPromotion)
#masterdata$YearsWithCurrManager <- scale(masterdata$YearsWithCurrManager)
#masterdata$leavedaysin <- scale(masterdata$leavedaysin)
#masterdata$loginvar <- scale(masterdata$loginvar)

str(masterdata) # We have now scaled numeric variables so we can move to creating dummy variables out of categorical variables

# Create dummy variables for the all the categorical factor variables which have more than2 levels
str(masterdata)
dummy1 <- model.matrix(~BusinessTravel, data = masterdata)
View(dummy1)
dummy1 <- dummy1[,-1]

dummy2 <- model.matrix(~Department, data = masterdata)
View(dummy2)
dummy2 <- dummy2[,-1]

dummy3 <- model.matrix(~Education, data = masterdata)
View(dummy3)
dummy3 <- dummy3[,-1]

dummy4 <- model.matrix(~EducationField, data = masterdata)
View(dummy4)
dummy4 <- dummy4[,-1]

dummy5 <- model.matrix(~JobLevel, data = masterdata)
View(dummy5)
dummy5 <- dummy5[,-1]

dummy6 <- model.matrix(~JobRole, data = masterdata)
View(dummy6)
dummy6 <- dummy6[,-1]

dummy7 <- model.matrix(~MaritalStatus, data = masterdata)
View(dummy7)
dummy7 <- dummy7[,-1]

dummy8 <- model.matrix(~StockOptionLevel, data = masterdata)
View(dummy8)
dummy8 <- dummy8[,-1]

dummy9 <- model.matrix(~EnvironmentSatisfaction, data = masterdata)
View(dummy9)
dummy9 <- dummy9[,-1]

dummy10 <- model.matrix(~JobSatisfaction, data = masterdata)
View(dummy10)
dummy10 <- dummy10[,-1]

dummy11 <- model.matrix(~WorkLifeBalance, data = masterdata)
View(dummy11)
dummy11 <- dummy11[,-1]

dummy12 <- model.matrix(~JobInvolvement, data = masterdata)
View(dummy12)
dummy12 <- dummy12[,-1]

# Remove categorical variables and replace with dummy variables
colnames(masterdata)
masterdatanew <- masterdata[,-c(4,5,7,8,10,11,12,16,22,23,24,25)]
masterdatanew <- cbind(masterdatanew, dummy1, dummy2,dummy3, dummy4, dummy5, dummy6, dummy7, dummy8, dummy9, dummy10, dummy11, dummy12)
# Also drop Employee ID as each row now is a unique employee so this is not required
str(masterdatanew)
masterdatanew <- masterdatanew[,-1] # We have 57 variables now

# Begin Logit model building
# splitting the data between train and test
set.seed(100)
trainindices <- sample(1:nrow(masterdatanew), 0.7*nrow(masterdatanew)) # Take 70% for training
atttrain <- masterdatanew[trainindices,] # Train data set
atttest <- masterdatanew[-trainindices,] # Test data set

View(atttrain)
# First logistic regression model

model1 <- glm(Attrition ~ ., data = atttrain, family = "binomial")
summary(model1) # AIC is 2124.2 

# Use stepwise to remove non-value add variables
step <- stepAIC(model1, direction = "both")
step # AIC is 2089 with null dev of 2625 and Res dev of 2019

# Build this into our second model
model2 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + loginvar + BusinessTravelTravel_Frequently + 
                BusinessTravelTravel_Rarely + `DepartmentResearch & Development` + 
                DepartmentSales + Education3 + Education4 + Education5 + 
                EducationFieldOther + JobLevel2 + JobLevel5 + `JobRoleManufacturing Director` + 
                `JobRoleResearch Director` + `JobRoleSales Executive` + MaritalStatusSingle + 
                StockOptionLevel1 + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                WorkLifeBalance4 + JobInvolvement3 + `EducationFieldTechnical Degree`, 
              family = "binomial", data = atttrain)

summary(model2) #AIC of 2089

# Use VIF and P values to further clean the model
library(car)
vif(model2) # R & D dept and sales dept have high vif but are also significant. Same with business travel frequently

# Business travel rarely has a high vif and low significance so we can drop that 
model3 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + loginvar + BusinessTravelTravel_Frequently + 
                 `DepartmentResearch & Development` + 
                DepartmentSales + Education3 + Education4 + Education5 + 
                EducationFieldOther + JobLevel2 + JobLevel5 + `JobRoleManufacturing Director` + 
                `JobRoleResearch Director` + `JobRoleSales Executive` + MaritalStatusSingle + 
                StockOptionLevel1 + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                WorkLifeBalance4 + JobInvolvement3 + `EducationFieldTechnical Degree`, 
              family = "binomial", data = atttrain)

summary(model3) # AIC of 2094
vif(model3) # Worklifebalance 2& 3 have high VIF but are also significant and same with total working years so now we can start to focus on high P values and eliminate those

# Remove `EducationFieldTechnical Degree` due to high P value
model4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + loginvar + BusinessTravelTravel_Frequently + 
                `DepartmentResearch & Development` + 
                DepartmentSales + Education3 + Education4 + Education5 + 
                EducationFieldOther + JobLevel2 + JobLevel5 + `JobRoleManufacturing Director` + 
                `JobRoleResearch Director` + `JobRoleSales Executive` + MaritalStatusSingle + 
                StockOptionLevel1 + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                WorkLifeBalance4 + JobInvolvement3, 
              family = "binomial", data = atttrain)

summary(model4) # AIC of 2094

# Remove Education3
model5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + loginvar + BusinessTravelTravel_Frequently + 
                `DepartmentResearch & Development` + 
                DepartmentSales +  Education4 + Education5 + 
                EducationFieldOther + JobLevel2 + JobLevel5 + `JobRoleManufacturing Director` + 
                `JobRoleResearch Director` + `JobRoleSales Executive` + MaritalStatusSingle + 
                StockOptionLevel1 + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                WorkLifeBalance4 + JobInvolvement3, 
              family = "binomial", data = atttrain)

summary(model5) # AIC of 2094

# Remove Education4
model6 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + loginvar + BusinessTravelTravel_Frequently + 
                `DepartmentResearch & Development` + 
                DepartmentSales +   Education5 + 
                EducationFieldOther + JobLevel2 + JobLevel5 + `JobRoleManufacturing Director` + 
                `JobRoleResearch Director` + `JobRoleSales Executive` + MaritalStatusSingle + 
                StockOptionLevel1 + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                WorkLifeBalance4 + JobInvolvement3, 
              family = "binomial", data = atttrain)

summary(model6) # AIC of 2093

# Remove Education 5
model7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + loginvar + BusinessTravelTravel_Frequently + 
                `DepartmentResearch & Development` + 
                DepartmentSales +    
                EducationFieldOther + JobLevel2 + JobLevel5 + `JobRoleManufacturing Director` + 
                `JobRoleResearch Director` + `JobRoleSales Executive` + MaritalStatusSingle + 
                StockOptionLevel1 + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                WorkLifeBalance4 + JobInvolvement3, 
              family = "binomial", data = atttrain)

summary(model7) # AIC of 2093

# Remove Joblevel 2
model8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + loginvar + BusinessTravelTravel_Frequently + 
                `DepartmentResearch & Development` + 
                DepartmentSales +    
                EducationFieldOther +  JobLevel5 + `JobRoleManufacturing Director` + 
                `JobRoleResearch Director` + `JobRoleSales Executive` + MaritalStatusSingle + 
                StockOptionLevel1 + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                WorkLifeBalance4 + JobInvolvement3, 
              family = "binomial", data = atttrain)

summary(model8) # AIC of 2094

# Remove Jobrole of sales exec
model9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + loginvar + BusinessTravelTravel_Frequently + 
                `DepartmentResearch & Development` + 
                DepartmentSales +    
                EducationFieldOther +  JobLevel5 + `JobRoleManufacturing Director` + 
                `JobRoleResearch Director` +  MaritalStatusSingle + 
                StockOptionLevel1 + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                WorkLifeBalance4 + JobInvolvement3, 
              family = "binomial", data = atttrain)

summary(model9) # AIC of 2095

# Remove Percentsalaryhike
model10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + loginvar + BusinessTravelTravel_Frequently + 
                 `DepartmentResearch & Development` + 
                 DepartmentSales +    
                 EducationFieldOther +  JobLevel5 + `JobRoleManufacturing Director` + 
                 `JobRoleResearch Director` +  MaritalStatusSingle + 
                 StockOptionLevel1 + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                 EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                 JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                 WorkLifeBalance4 + JobInvolvement3, 
               family = "binomial", data = atttrain)

summary(model10) # AIC of 2096

# Remove Stockoptionlevel1
model11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + loginvar + BusinessTravelTravel_Frequently + 
                 `DepartmentResearch & Development` + 
                 DepartmentSales +    
                 EducationFieldOther +  JobLevel5 + `JobRoleManufacturing Director` + 
                 `JobRoleResearch Director` +  MaritalStatusSingle + 
                 EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                 EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                 JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                 WorkLifeBalance4 + JobInvolvement3, 
               family = "binomial", data = atttrain)

summary(model11) # AIC of 2097


# Remove Jobinvolvement3
model12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + loginvar + BusinessTravelTravel_Frequently + 
                 `DepartmentResearch & Development` + 
                 DepartmentSales +    
                 EducationFieldOther +  JobLevel5 + `JobRoleManufacturing Director` + 
                 `JobRoleResearch Director` +  MaritalStatusSingle + 
                 EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                 EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                 JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                 WorkLifeBalance4 , 
               family = "binomial", data = atttrain)

summary(model12) # AIC of 2099

# Remove EducationFieldOther
model13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + loginvar + BusinessTravelTravel_Frequently + 
                 `DepartmentResearch & Development` + 
                 DepartmentSales +    
                  JobLevel5 + `JobRoleManufacturing Director` + 
                 `JobRoleResearch Director` +  MaritalStatusSingle + 
                 EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                 EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                 JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                 WorkLifeBalance4 , 
               family = "binomial", data = atttrain)

summary(model13) # AIC of 2102

# Remove `JobRoleResearch Director`
model14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + loginvar + BusinessTravelTravel_Frequently + 
                 `DepartmentResearch & Development` + 
                 DepartmentSales +    
                 JobLevel5 + `JobRoleManufacturing Director` + 
                   MaritalStatusSingle + 
                 EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                 EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                 JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                 WorkLifeBalance4 , 
               family = "binomial", data = atttrain)

summary(model14) # AIC of 2104

# Remove JobLevel5
model15 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + loginvar + BusinessTravelTravel_Frequently + 
                 `DepartmentResearch & Development` + 
                 DepartmentSales +    
                  `JobRoleManufacturing Director` + 
                 MaritalStatusSingle + 
                 EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                 EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                 JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                 WorkLifeBalance4 , 
               family = "binomial", data = atttrain)

summary(model15) # AIC of 2107

# Remove JobSatisfaction2
model16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + loginvar + BusinessTravelTravel_Frequently + 
                 `DepartmentResearch & Development` + 
                 DepartmentSales +    
                 `JobRoleManufacturing Director` + 
                 MaritalStatusSingle + 
                 EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                 EnvironmentSatisfaction4 +  JobSatisfaction3 + 
                 JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                 WorkLifeBalance4 , 
               family = "binomial", data = atttrain)

summary(model16) # AIC of 2111

# Remove JobSatisfaction3
model17 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + loginvar + BusinessTravelTravel_Frequently + 
                 `DepartmentResearch & Development` + 
                 DepartmentSales +    
                 `JobRoleManufacturing Director` + 
                 MaritalStatusSingle + 
                 EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                 EnvironmentSatisfaction4 +   
                 JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                 WorkLifeBalance4 , 
               family = "binomial", data = atttrain)

summary(model17) # AIC of 2115

# Remove Job Role Manufacturing Director
model18 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + loginvar + BusinessTravelTravel_Frequently + 
                 `DepartmentResearch & Development` + 
                 DepartmentSales +    
                 MaritalStatusSingle + 
                 EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                 EnvironmentSatisfaction4 +   
                 JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                 WorkLifeBalance4 , 
               family = "binomial", data = atttrain)

summary(model18) # AIC of 2122

# Remove Training Times Last Year
model19 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                 TotalWorkingYears +  YearsSinceLastPromotion + 
                 YearsWithCurrManager + loginvar + BusinessTravelTravel_Frequently + 
                 `DepartmentResearch & Development` + 
                 DepartmentSales +    
                 MaritalStatusSingle + 
                 EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                 EnvironmentSatisfaction4 +   
                 JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                 WorkLifeBalance4 , 
               family = "binomial", data = atttrain)

summary(model19) # AIC of 2129

# It seems like we have a model with all significant variables now
# In total we have 17 variables which are broadly 12 variables with sub-levels under some of them like work life balance, environment satisafaction, etc

# Let's save this and test it on our test data
final_model <- model19
View(atttest)

test_pred <- predict(final_model, type = "response", 
                     newdata = atttest[,-2])

summary(test_pred)
View(test_pred)

atttest$prob <- test_pred
View(atttest)

# Let's use the probability cutoff of 50% to test the results

test_pred_att <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_att <- factor(ifelse(atttest$Attrition==1,"Yes","No"))

table(test_actual_att,test_pred_att)

test_conf <- confusionMatrix(test_pred_att, test_actual_att, positive = "Yes")
test_conf

# High accuracy of 85% but the sensitivity is 26% which is low. If we want to take action here, we need to increase our sensitivity
# We need a good mix here because we want to take action for employees who will attrite but we also do not want to unnnecessarily increase efforts for employees who won't

# Let's find out the optimal probalility cutoff 
# Define cutoff function
perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_att, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

summary(test_pred)

# Creating cutoff values from 0.01 (min val - 0.0008194) to 0.90 (max val - 0.9195543) for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability


s = seq(.01,.90,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

cutoff

# Let's choose a cutoff value of 0.1718 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.1718, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_att, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

conf_final

View(atttest)

# We have all measures - Accuracy, Sensitivity & Specificity at around 75%

# Calculate the KS Statistic
library(ROCR)
library(pROC)

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_att <- ifelse(test_actual_att=="Yes",1,0)


#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_att)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) # Almost 50% which is good

plot(roc(Attrition~prob, data = atttest)) # Area under the curve 0.81 which is quite good

# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

test_actual_att
test_pred

Attrition_decile <- lift(test_actual_att, test_pred, groups = 10)

Attrition_decile

# So the final model we have is model 19 which is the best model with a cutoff of 0.1754 and the variables & coefficients are as follows:


#(Intercept)                         2.300821   0.419010   5.491 3.99e-08 ***
# Age                                -0.033514   0.008728  -3.840 0.000123 ***
# NumCompaniesWorked                  0.135699   0.024276   5.590 2.27e-08 ***
# TotalWorkingYears                  -0.072178   0.014190  -5.087 3.65e-07 ***
# YearsSinceLastPromotion             0.180693   0.029043   6.222 4.92e-10 ***
# YearsWithCurrManager               -0.117847   0.025155  -4.685 2.80e-06 ***
# loginvar                           -0.442934   0.040663 -10.893  < 2e-16 ***
# BusinessTravelTravel_Frequently     0.937418   0.128292   7.307 2.73e-13 ***
# `DepartmentResearch & Development` -0.997559   0.240488  -4.148 3.35e-05 ***
# DepartmentSales                    -0.937174   0.251117  -3.732 0.000190 ***
# MaritalStatusSingle                 0.968410   0.114991   8.422  < 2e-16 ***
# EnvironmentSatisfaction2           -0.795581   0.170196  -4.675 2.95e-06 ***
# EnvironmentSatisfaction3           -0.800041   0.151374  -5.285 1.26e-07 ***
# EnvironmentSatisfaction4           -1.302714   0.163407  -7.972 1.56e-15 ***
# JobSatisfaction4                   -0.613635   0.129926  -4.723 2.32e-06 ***
# WorkLifeBalance2                   -0.896848   0.216550  -4.142 3.45e-05 ***
# WorkLifeBalance3                   -1.306841   0.202503  -6.453 1.09e-10 ***
# WorkLifeBalance4                   -1.006566   0.255293  -3.943 8.05e-05 ***

# B0 is positive
# Number of companies an employee has worked for, the years since they were last promoted, Frequent Business Travel and Being Single (Marital Status) all have positive coefficients
# This indicates that these have a positive effect and attrition
# Age, total working years, years working under the current manager, being part of the R & D or sales dept, medium and above satisfaction with the work environment, Very high job satisfaction and a Good or above worklife balance rating have negative coeffcients and hence can bring attrition down
# Variance of login hours versus the standard hours also has a negative coefficient which means that if the login hours are less than standard hours, then it'll reduce the chances of attrition but if it more than standard hours, then there is a higher chance of attrition
