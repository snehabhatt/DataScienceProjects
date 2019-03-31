## Buisness understanding...
#New York City is a thriving metropolis. Just like most other metros that size, one of the biggest problems its citizens face
#is parking. The classic combination of a huge number of cars and cramped geography is the exact recipe that leads to a huge
#number of parking tickets.

#  the NYC Police Department has collected data for parking tickets.

## objective:
# exploratory analysis on New York City data

#Examine the data

#Find the total number of tickets for the year.
#Find out the number of unique states from where the cars that got parking tickets came from. (Hint: Use the column 'Registration State')
#There is a numeric entry '99' in the column which should be corrected. Replace it with the state having maximum entries. Give the number of unique states again.

## Data understanding
#The Data Dictionary for this dataset is as follows: [Field Name--Description]
#[1] Summons_Number--Unique Identifier Of Summons [Primary Key]   
#[2] Plate_Id--Registered Plate Id     
#[3] Registration_State--State Of Plate Registration    
#[5] Issue_Date--Issue Date      
#[6] Violation_Code--Type Of Violation     
#[7] Violation_Precinct--Precinct Of Violation     
#[8] Issuer_Precinct--Precinct Of Issuance     
#[9] Violation_Time--Time Violation Occurred     
#[10]Vehicle Body Type -- Vehicle Body Type Written On Summons (Sedan,
#[11] Vehicle Make -- 


# Load SparkR
spark_path <- '/usr/local/spark'
if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = spark_path)
}
library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))

## Load libraries...
library(dplyr)
library(stringr)

# Initialise the sparkR session
sparkR.session(master = "yarn-client", sparkConfig = list(spark.driver.memory = "1g"))

# Create a Spark DataFrame and examine structure
NewyorkParking_data <- read.df("/common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2017.csv", source = "csv",
                               header = TRUE, inferSchema = TRUE)

# Check the Spark Dataframe
head(NewyorkParking_data)

#1.7. Understanding the dimensions and structure of the the NewyorkParking_data
str(NewyorkParking_data)
head(NewyorkParking_data)
dim(NewyorkParking_data)
# Dataset has Rows: 10,803,028 | Columns: 10
printSchema(tkt_2017_nycparking)
#From the Schema and Datatypes of each column there are several non-conforming columns like Issue_Date, Vehicle_Expiration_Date, Violation Time, Date_First_Observed etc.
#These columns need to be filtered and transformed into a suitable format for processing. We will explore any further discrepancies in the individual EDA.


### Lets remove unnecccesary symbols from the dataframe column names......
colnames(NewyorkParking_data)<- str_trim(colnames(NewyorkParking_data), side= "both")
colnames(NewyorkParking_data)<- str_replace_all(colnames(NewyorkParking_data), pattern=" ", replacement = "_")
colnames(NewyorkParking_data)<- str_replace_all(colnames(NewyorkParking_data), pattern="\\?", replacement = "")
###
#Detailed Data Quality Verification of 2015 NYC Parking Ticket Dataset
# Removing any duplicate rows in the dataset [Two or More rows having the same Summons_Number ]
NewyorkParking_data<- dropDuplicates(NewyorkParking_data)  ### No duplicate values found...
dim(NewyorkParking_data)

## Convert the date parameters to suitable format....
NewyorkParking_data$Issue_Date=SparkR::to_date(NewyorkParking_data$Issue_Date, 'MM/dd/yyyy')
head(NewyorkParking_data)

##
# Before executing any hive-sql query from RStudio, you need to add a jar file in RStudio 
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")
#Since ticket Issue Date is an critical parameter to assess the quality of the Dataset. Let us check if there are any missing values in the Issue Date Parameter.
### Sql to craete a temporary view...
createOrReplaceTempView(NewyorkParking_data, "Newyork_Parking")
CountNull_IssueDate_Newyork_Parking <- SparkR::sql("SELECT SUM(CASE WHEN Issue_Date IS NULL
                                                   THEN 1
                                                   ELSE 0
                                                   END) nulls_Issue_Date,
                                                   COUNT(*) Num_of_Rows
                                                   FROM Newyork_Parking")
head(CountNull_IssueDate_Newyork_Parking)

Range_IssueDate_Newyork_Parking <- SparkR::sql("SELECT min(issue_date)as Min_IssueDate,
                                               max(issue_date)as Max_IssueDate
                                               FROM Newyork_Parking")
head(Range_IssueDate_Newyork_Parking) ### the data has tickets collected from issue date 1972-03-30 till issue date 2069-11-19...
# Let us Analyze this parameter closely to decide optimal filter condition.

#1.8.3.3 We Will create Additional Columns in the Dataset that Correspond to the Year and Month of Ticket Issue
NewyorkParking_data$Issue_Year <- year(NewyorkParking_data$Issue_Date)
NewyorkParking_data$Issue_Month <- month(NewyorkParking_data$Issue_Date)

createOrReplaceTempView(NewyorkParking_data, "Newyork_Parking")

#Now let's observe the Distribution of Issue Date.
NewyorkParking_databyYearAndMonth <- SparkR::sql("SELECT Issue_Year,
                                                 Issue_Month,
                                                 count(*)as Num_of_Records
                                                 FROM Newyork_Parking
                                                 GROUP BY Issue_Year,
                                                 Issue_Month
                                                 ORDER BY 1,2")
head(NewyorkParking_databyYearAndMonth)

#1.8.3.4 Subsetting the DataFrame according to the cuurent Year in question
# Considering A Fiscal Year to extend from the Jan of 2017 to  2017 Dec

NewyorkParking_data <- NewyorkParking_data[
  NewyorkParking_data$Issue_Date >= "2017-01-01" & 
    NewyorkParking_data$Issue_Date <= "2017-12-31"]

nrow(NewyorkParking_data)
# 5431918 records in filtered dataset
createOrReplaceTempView(NewyorkParking_data, "Newyork_Parking_2017")

CountNull_IssueDate_Newyork_Parking <- SparkR::sql("SELECT SUM(CASE WHEN Issue_Date IS NULL
                                                   THEN 1
                                                   ELSE 0
                                                   END) nulls_Issue_Date,
                                                   COUNT(*) Num_of_Rows
                                                   FROM Newyork_Parking_2017")  ##3 no null enteries found..
head(NewyorkParking_data) ##3 no null enteries found..

# add 'M' to the coumn Violation_Time
NewyorkParking_data$concat<-"M"
NewyorkParking_data$Violation_Time<-concat(NewyorkParking_data$Violation_Time,NewyorkParking_data$concat)
head(NewyorkParking_data)

## droping concat col
NewyorkParking_data<- drop(NewyorkParking_data, c("concat"))
colnames(NewyorkParking_data)

### analysing violation time into hour and seconddss,
NewyorkParking_data$Violation_Hour <- substr(NewyorkParking_data$Violation_Time, 1, 2)
NewyorkParking_data$Violation_Minute <- substr(NewyorkParking_data$Violation_Time, 3, 4)
NewyorkParking_data$Violation_AMPM <- substr(NewyorkParking_data$Violation_Time, 5, 6)

colnames(NewyorkParking_data)
head(NewyorkParking_data)

#We've observed that there are records that have both 00xxAM as well as 12xxAM. Therefore we will replace all 00xxAM with 12xxAM
NewyorkParking_data$Violation_Hour <- regexp_replace(x = NewyorkParking_data$Violation_Hour,pattern = "\\00",replacement = "12")


#Concatenating the components into a standardized Violation Time.
NewyorkParking_data$Violation_Time1 <- concat(NewyorkParking_data$Violation_Hour, NewyorkParking_data$Violation_Minute, NewyorkParking_data$Violation_AMPM)

#Converting Violation Time into a TimeStamp
NewyorkParking_data$Violation_Time1<-to_timestamp(x = NewyorkParking_data$Violation_Time1, format = "hhmma")

#The dimensions of Formatted and Cleaned Dataset that will be used for Analysis:
head(NewyorkParking_data)
dim(NewyorkParking_data)
#2017 Dataset has Rows: 5431918 | Columns: 16
printSchema(NewyorkParking_data)


#************************ Stage 3: Overview and Examining the dataset ************************#
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")
createOrReplaceTempView(NewyorkParking_data, "NewyorkParking_data_view")
#Q1.   Find total number of tickets for each year!

Number_of_Tickets<- c(nrow(NewyorkParking_data))
Year_Labels<- c("FY_2017")
tickets_vs_year<- data.frame(Number_of_Tickets, Year_Labels)
tickets_vs_year
ggplot(tickets_vs_year, aes(x=Year_Labels, y=Number_of_Tickets))+ geom_col() + xlab("Fiscal Year") + ylab("Number of Tickets") + ggtitle("Plot1. Fiscal Year vs. Number of Tickets") + geom_text(aes(label=Number_of_Tickets),vjust=-0.3)
## Number_of_Tickets     Year_Labels
##  5431918               FY_2017



#Q2. Find out the number of unique states from where the cars that got parking tickets came from
#************ Stage 2: Q2 ************#
#2.2. Find out how many unique states the cars which got parking tickets came from!

#2.2.1 Registration State Distribution for 2017



NewyorkParking_Dataset<- SparkR::sql("SELECT Registration_State, count(*)as Number_of_Tickets 
                          from NewyorkParking_data_view 
                          group by Registration_State
                          order by Number_of_Tickets desc")
head((NewyorkParking_Dataset),nrow(NewyorkParking_Dataset))

### NY is the most common state..
NewyorkParking_Dataset$Registration_State<- ifelse(NewyorkParking_Dataset$Registration_State == "99", "NY", NewyorkParking_Dataset$Registration_State)

NewyorkParking_Dataset_Count<- c(nrow(NewyorkParking_Dataset))
Year_Labels<- c("FY_2017")
NewyorkParking_Dataset_vs_year<- data.frame(NewyorkParking_Dataset_Count, Year_Labels)
head(NewyorkParking_Dataset_vs_year)
#Let's see the results on a graph.
ggplot(NewyorkParking_Dataset_vs_year, aes(x=Year_Labels, y=NewyorkParking_Dataset_Count))+ geom_col() + xlab("Fiscal Year") + ylab("Count of Unique Registration States") + ggtitle("Plot2. Fiscal Year vs. Count of Unique Registration States") + geom_text(aes(label=NewyorkParking_Dataset_Count),vjust=-0.3)
### the naswer is 65...


######################### Aggregation tasks###################################################

#Q 1 How often does each violation code occur? Display the frequency of the top five violation codes.
# Violation_Code 
NewyorkParking_DatasetbyViolationCode<- SparkR::sql("SELECT Violation_Code, count(*)as Number_of_Tickets 
                          from NewyorkParking_data_view 
                          group by Violation_Code
                          order by Number_of_Tickets desc")
head(NewyorkParking_DatasetbyViolationCode,5)

NewyorkParking_DatasetbyViolationCode_top5<- data.frame(head(NewyorkParking_DatasetbyViolationCode,5))

ggplot(NewyorkParking_DatasetbyViolationCode_top5, aes(x=as.factor(Violation_Code), y=Number_of_Tickets))+ geom_col() + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot4A. 2017 Top 5 Violation Code vs Frequency of Ticket") + geom_text(aes(label=Number_of_Tickets),vjust=-0.3)

#Q-2 How often does each 'vehicle body type' get a parking ticket? How about the 'vehicle make'?
NewyorkParking_DatasetbyVehicle_Body_Type<- SparkR::sql("SELECT Vehicle_Body_Type, count(*)as Number_of_Tickets 
                          from NewyorkParking_data_view 
                          group by Vehicle_Body_Type
                          order by Number_of_Tickets desc")
head(NewyorkParking_DatasetbyVehicle_Body_Type,5)

NewyorkParking_DatasetbyVehicle_Body_top5<- data.frame(head(NewyorkParking_DatasetbyVehicle_Body_Type,5))

ggplot(NewyorkParking_DatasetbyVehicle_Body_top5, aes(x=as.factor(Vehicle_Body_Type), y=Number_of_Tickets))+ geom_col() + xlab("Vehicle_Body_Type") + ylab("Frequency of Tickets") + ggtitle("Plot4A. 201 Top 5 Vehicle_Body_Type vs Frequency of Ticket") + geom_text(aes(label=Number_of_Tickets),vjust=-0.3)

NewyorkParking_DatasetbyVehicle_Make<- SparkR::sql("SELECT Vehicle_Make, count(*)as Number_of_Tickets 
                          from NewyorkParking_data_view 
                          group by Vehicle_Make
                          order by Number_of_Tickets desc")
head(NewyorkParking_DatasetbyVehicle_Make,5)

NewyorkParking_DatasetbyVehicle_Make_top5<- data.frame(head(NewyorkParking_DatasetbyVehicle_Make,5))

ggplot(NewyorkParking_DatasetbyVehicle_Make_top5, aes(x=as.factor(Vehicle_Make), y=Number_of_Tickets))+ geom_col() + xlab("Vehicle_Make") + ylab("Frequency of Tickets") + ggtitle("Plot4A. 2017 Top 5 Vehicle_Make vs Frequency of Ticket") + geom_text(aes(label=Number_of_Tickets),vjust=-0.3)

#Q3. A precinct is a police station that has a certain zone of the city under its command. Find the (5 highest) frequency of tickets for each of the following:
#'Violation Precinct' (this is the precinct of the zone where the violation occurred). Using this, can you make any insights for parking violations in any specific areas of the city? 

NewyorkParking_DatasetbyViolation_Precinct<- SparkR::sql("SELECT Violation_Precinct, count(*)as Number_of_Tickets 
                          from NewyorkParking_data_view 
                          group by Violation_Precinct 
                          order by Number_of_Tickets desc")

head(NewyorkParking_DatasetbyViolation_Precinct,5)

NewyorkParking_DatasetbyViolation_Precinct_top5<- data.frame(head(NewyorkParking_DatasetbyViolation_Precinct,5))

ggplot(NewyorkParking_DatasetbyViolation_Precinct_top5, aes(x=as.factor(Violation_Precinct), y=Number_of_Tickets))+ geom_col() + xlab("Violation Precinct") + ylab("Number_of_Tickets") + ggtitle("Plot7A. 2017 Top 5 Violation Precinct vs Frequency of Ticket") + geom_text(aes(label=Number_of_Tickets),vjust=-0.3)

#Top Issuer Precinct for 2017
isuprect_frequency_2017<- SparkR::sql("SELECT Issuer_Precinct, count(*)as Frequency_of_Tickets
                                      from NewyorkParking_data_view 
                                      group by Issuer_Precinct
                                      order by Frequency_of_Tickets desc")
head(isuprect_frequency_2017,5)

isuprect_2017_top5<- data.frame(head(isuprect_frequency_2017,5))

ggplot(isuprect_2017_top5, aes(x=as.factor(Issuer_Precinct), y=Frequency_of_Tickets))+ geom_col() + xlab("Issuer Precinct") + ylab("Frequency of Tickets") + ggtitle("Plot8C. 2017 Top 5 Issuer Precinct vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#Q4 Find the violation code frequency across three precincts which have issued the most number of tickets - do
#these precinct zones have an exceptionally high frequency of certain violation codes? Are these codes common across precincts? 

##In Year 2017 [Top Three Issuer Precinct's : 0, 19 and 14]

#Violation Code Distribution in Issuer Precinct 0
one_isuprect_2017<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets, Issuer_Precinct
                                from NewyorkParking_data_view 
                                where Issuer_Precinct = 0
                                group by Violation_Code, Issuer_Precinct
                                order by Frequency_of_Tickets desc")
head(one_isuprect_2017, 5)

one_isuprect_top5_2017<- data.frame(head(Violationcodefreq3precinct_2017, 5))

#Violation Code Distribution in Issuer Precinct 19
two_isuprect_2017<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets, Issuer_Precinct
                                from NewyorkParking_data_view 
                                where Issuer_Precinct = 19
                                group by Violation_Code, Issuer_Precinct
                                order by Frequency_of_Tickets desc")
head(two_isuprect_2017, 5)

two_isuprect_top5_2017<- data.frame(head(two_isuprect_2017, 5))

#Violation Code Distribution in Issuer Precinct 14
three_isuprect_2017<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets, Issuer_Precinct
                                  from NewyorkParking_data_view 
                                  where Issuer_Precinct = 14
                                  group by Violation_Code, Issuer_Precinct
                                  order by Frequency_of_Tickets desc")
head(three_isuprect_2017,5)

three_isuprect_top5_2017<- data.frame(head(three_isuprect_2017,5))

#Violation Code Distribution in Other Issuer Precincts
other_isuprect_2017<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                  from NewyorkParking_data_view 
                                  where Issuer_Precinct NOT IN (0,19,14)
                                  group by Violation_Code
                                  order by Frequency_of_Tickets desc")
head(other_isuprect_2017,5)

other_isuprect_top5_2017<- data.frame(head(other_isuprect_2017,5))
other_isuprect_top5_2017$Issuer_Precinct<- c("Other","Other","Other","Other","Other")

#Combined Violation Code Distribution vs Issuer Precincts in 2017

vioisuprect_2017_combined<- rbind(one_isuprect_top5_2017, two_isuprect_top5_2017, three_isuprect_top5_2017, other_isuprect_top5_2017)

ggplot(vioisuprect_2017_combined, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Issuer_Precinct) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot9C. 2017 Comparison of Violation Code Distribution vs. Top Issuer Precinct") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)


# Q5. You’d want to find out the properties of parking violations across different times of the day:

#2017 Dataset: Violation Time Bin vs. Violation Code Analysis

#Find a way to deal with missing values, if any.
null_violat_times_2017<- SparkR::sql("SELECT count(*)as Total_Num_of_Rows, 
                                     SUM(CASE WHEN Violation_Time is NULL
                                     THEN 1 ELSE 0 END)as Nulls_Violation_Time,
                                     100*SUM(CASE WHEN Violation_Time IS NULL
                                     THEN 1 ELSE 0 END)/count(*) as Percent_Tickets_2017_ViolationTimeMissing
                                     from NewyorkParking_data_view")
head(null_violat_times_2017)
#2017 dataset 0.6114% records with Missing Violation Time is Negligable and will therefore be removed before analysis.

adjusted_tkt_2017_nycparking<- subset(NewyorkParking_data, isNotNull(NewyorkParking_data$Violation_Time))
adjusted_tkt_2017_nycparking$Violation_Hour <- hour(cast(adjusted_tkt_2017_nycparking$Violation_Time,dataType = "string"))
createOrReplaceTempView(adjusted_tkt_2017_nycparking, "violt_2017")


#Divide 24 hours into 6 equal discrete bins of time. The intervals you choose are at your discretion. 
violation_hour_bin_2017 <- SparkR::sql("SELECT Violation_Hour,
                                       Violation_Code,
                                       CASE WHEN Violation_Hour BETWEEN 0 AND 3
                                       THEN '0_3'
                                       WHEN Violation_Hour BETWEEN 4 AND 7
                                       THEN '4_7'
                                       WHEN Violation_Hour BETWEEN 8 AND 11
                                       THEN '8_11'
                                       WHEN Violation_Hour BETWEEN 12 AND 15
                                       THEN '12_15' 
                                       WHEN Violation_Hour BETWEEN 16 AND 19
                                       THEN '16_19' 
                                       WHEN Violation_Hour BETWEEN 20 AND 23
                                       THEN '20_23' 
                                       END AS Violation_Hour_Bin
                                       FROM violt_2017")

createOrReplaceTempView(violation_hour_bin_2017, "violt_hour_2017_nyc")

hour_bin_tkts_2017 <- SparkR::sql("SELECT Violation_Hour_Bin,
                                  Violation_Code,
                                  Frequency_of_Tickets
                                  FROM (SELECT Violation_Hour_Bin,
                                  Violation_Code,
                                  Frequency_of_Tickets,
                                  dense_rank() over (partition by Violation_Hour_Bin order by Frequency_of_Tickets desc) Rnk
                                  FROM (SELECT Violation_Hour_Bin,
                                  Violation_Code,
                                  count(*)as Frequency_of_Tickets
                                  FROM violt_hour_2017_nyc
                                  GROUP BY Violation_Hour_Bin,
                                  Violation_Code))
                                  WHERE Rnk <= 3")

df_hour_bin_tkts_2017 <- data.frame(head(hour_bin_tkts_2017, nrow(hour_bin_tkts_2017)))

ggplot(df_hour_bin_tkts_2017, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Violation_Hour_Bin) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot13C. 2017 Comparison of Violation Code Distribution vs. Violation_Hour_Bin") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#*******************************************************************************************************#
#******************** Part 2: Violation Code [Top-3] vs. Violation Time Bin Distribution ********************#

#2017 Dataset: Top-3 Violation Code vs. Violation Time Bin Analysis
top_3_violations_2017 <- SparkR::sql("SELECT Violation_Code,
                                     count(*) no_of_tickets
                                     FROM violt_hour_2017_nyc
                                     GROUP BY Violation_Code
                                     ORDER BY no_of_tickets desc")

head(top_3_violations_2017,3)
#Top-3 Violation Codes for 2017 are 21, 36 and 38.

common_times_2017 <- SparkR::sql("SELECT Violation_Code,
                                 Violation_Hour_Bin,
                                 count(*) no_of_tickets
                                 FROM violt_hour_2017_nyc
                                 WHERE violation_code IN (21,36,38)
                                 GROUP BY Violation_Code, 
                                 Violation_Hour_Bin
                                 ORDER BY Violation_Code, 
                                 Violation_Hour_Bin,
                                 no_of_tickets desc")	

df_common_times_viol_2017 <- data.frame(head(common_times_2017, nrow(common_times_2017)))

ggplot(df_common_times_viol_2017, aes(x= as.factor(Violation_Hour_Bin), y=no_of_tickets))+ geom_col()+ facet_grid(~Violation_Code) + xlab("Violation Hour Bin") + ylab("Frequency of Tickets") + ggtitle("Plot14C. 2017 Comparison of Violation_Hour_Bin vs. No_of_tickets") + geom_text(aes(label=no_of_tickets),vjust=-0.3)


#************ Stage 3: Q6 ************#
# Checking for seasonality in dataset

#3.6.1 Season vs. Frequency Analysis

#2017 Season vs. Frequency Analysis
Season_Binning_2017 <- SparkR::sql("SELECT Summons_Number,
                                   Violation_Code,
                                   CASE WHEN Issue_Month IN (1,2,12)
                                   THEN 'Winter'
                                   WHEN Issue_Month BETWEEN 3 AND 5
                                   THEN 'Spring'
                                   WHEN Issue_Month BETWEEN 6 AND 8
                                   THEN 'Summer'
                                   WHEN Issue_Month BETWEEN 9 AND 12
                                   THEN 'Fall' 
                                   END AS Season
                                   FROM NewyorkParking_data_view")
createOrReplaceTempView(Season_Binning_2017, "season_tkt_2017_nyc")

tktseason_2017<- SparkR::sql("SELECT Season,
                             Count(*)as Frequency_of_Tickets
                             FROM season_tkt_2017_nyc
                             GROUP BY Season
                             ORDER BY Frequency_of_Tickets desc")
head(tktseason_2017)

freq_tktseason_2017<- data.frame(head(tktseason_2017))
freq_tktseason_2017$Fiscal_Year<- c(2017,2017,2017,2017)
freq_tktseason_2017

#Season vs. Violation Code Distribution Analysis

#2017 Season vs. Violation Code Distribution Analysis

season_violation_2017 <- SparkR::sql("SELECT  Season,
                                     Violation_Code,
                                     Frequency_of_Tickets
                                     FROM (SELECT dense_rank() over (partition by Season order by Frequency_of_Tickets desc) rk,
                                     Season,
                                     Violation_Code,
                                     Frequency_of_Tickets
                                     FROM (SELECT Season,
                                     Violation_Code,
                                     Count(*) Frequency_of_Tickets
                                     FROM season_tkt_2017_nyc
                                     GROUP BY Season, Violation_Code))
                                     WHERE rk <= 3
                                     ORDER BY Season, Frequency_of_Tickets desc")

df_season_violation_2017 <-  data.frame(head(season_violation_2017, nrow(season_violation_2017)))
df_season_violation_2017

#Seasonwise Violation Code Distribution 2017
ggplot(df_season_violation_2017, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Season) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot12C. 2017 Comparison of Seasons vs. Frequency of Violation Codes") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)


## #************ Stage 3: Q7 ************#

#2017
violationcd_frequency_2017<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                         from NewyorkParking_data_view 
                                         group by Violation_Code
                                         order by Frequency_of_Tickets desc")
head(violationcd_frequency_2017,3)

fine_top3_2017<- data.frame(head(violationcd_frequency_2017,3))
fine_top3_2017$Fiscal_Year <- c(2017,2017,2017)
fine_top3_2017$Average_Fine_PerTicket<- c(55,50,50)
fine_top3_2017$Total_Fine_Amount<- fine_top3_2017$Frequency_of_Tickets * fine_top3_2017$Average_Fine_PerTicket
fine_top3_2017

#************************* END of CODE *************************#