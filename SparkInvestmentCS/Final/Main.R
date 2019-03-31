# Setting workspace

library(dplyr)
library(tidyr)

#Checkpoint 1

# Loading the companies and rounds data into data frames companies and rounds2 respectively

companies <- read.delim("companies.txt", header = TRUE, sep="\t")

rounds2 <- read.delim("rounds2.csv", header = TRUE, sep= ",")

#Changing companies and rounds2 column permalink and company_permalink to lower case

companies$permalink <- tolower(companies$permalink)
rounds2$company_permalink <- tolower(rounds2$company_permalink)

# 1. How many unique companies are present in rounds2?

names(rounds2) #Getting column names
nrow(distinct(rounds2,company_permalink)) #Getting distinct count of company_permalink - Unique ID of company 

# 2. How many unique companies are present in the companies file?

names(companies) #Getting column names
nrow(distinct(companies,permalink)) #Getting distinct count of Permalink - Unique ID of company 

# 4. Are there any companies in the rounds2 file which are not present in companies? Answer yes or no: Y/N
sum(!(rounds2$company_permalink %in% companies$permalink))

# 5. Merging the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame.
# Name the merged frame master_frame.How many observations are present in master_frame ?

master_frame <- merge(rounds2, companies, by.x="company_permalink", by.y = "permalink")
nrow(master_frame)
View(master_frame)

#Checkpoint 2 - Funding Type Analysis

#Filtering data for four funding types (venture, angel, seed, and private equity)

master_frame_1 <- filter(master_frame, funding_round_type == "angel" | funding_round_type == "venture" | funding_round_type == "seed" | funding_round_type == "private_equity")

#Finding average Values of Investments for Each of these Funding Types 

avg_investments<- aggregate(master_frame_1$raised_amount_usd, by=list(master_frame_1$funding_round_type), FUN=mean, na.rm = T)
colnames(avg_investments) <- c("funding_round_type","avg_raised_amt_usd")
View(avg_investments)

#Checkpoint 3 - Country Analysis

#Creating data frame top9

#Finding top nine countries which have received the highest total funding (across ALL sectors for the chosen investment type)

Venture_df <- filter(master_frame, funding_round_type == "venture" ) #Creating separate dataframe for the chosen investment type
a <- aggregate(Venture_df$raised_amount_usd, 
               by=list(country_code=Venture_df$country_code), FUN=sum, na.rm = T) #Total investments across countries
a <- a[!(a$country_code==""),] #Removing row which has country name as blank
b <- top_n(a,9,x) # Top 9 investment countries 

#Creating the data frame named top9 with the top nine countries (based on the total investment amount each country has received)

top9 <- arrange(b,desc(x)) # Sorting in descending order
colnames(top9) <- c("country_code","amount_raised_usd")
View(top9)

#Checkpoint 4 - Sector Analysis 1

#Extracting the primary sector of each category list from the category_list column

master_frame$category_list1 <- master_frame$category_list #Replicating category list for extracting primary sector
master_frame <- separate(master_frame, category_list1 , into=c("primary_sector"), sep = "\\|")

#Merging master data frame with each primary sector mapped to its main sector 

#Loading the mapping.csv file into data frame mapping
mapping <- read.delim("mapping.csv", header = TRUE, sep= ",")

#Converting Wide to long format data 
names(mapping) #Getting column names
new_mapping <- gather(mapping, key = "main_sector", my_val, Automotive...Sports:Social..Finance..Analytics..Advertising)

# Cleansing master dataframe category_list blank values
master_frame$primary_sector[(master_frame$primary_sector=="")] <- NA

# Cleansing mapping data
new_mapping <- new_mapping[!(new_mapping$my_val == 0),] # Removing row having value as '0' since it denotes that the category doesn't belong to that sector
new_mapping <- new_mapping[,-3] # Removing the last column my_val from the data frame
new_mapping$category_list <- gsub("0","na",new_mapping$category_list) # Replacing '0' with 'na'. eg. A0lytics which should be Analytics
new_mapping$category_list <- gsub("\\.na",".0",new_mapping$category_list) # Correcting the data eg. enterprise 2.na to enterprise 2.0

#Changing master_frame and new_mapping column primary_sector and category_list to lower case
master_frame$primary_sector <- tolower(master_frame$primary_sector)
new_mapping$category_list <- tolower(new_mapping$category_list)

#Using the mapping file 'mapping.csv', Merging master data frame to map each primary sector to one of the eight main sectors.

sector_frame <- merge(master_frame,new_mapping,by.x = "primary_sector", by.y = "category_list", all.x = TRUE)
View(sector_frame)
write.csv(sector_frame,"sector_frame.csv",row.names = FALSE, na = "")

#Checkpoint 5 - Sector Analysis 2

#Creating three separate data frames D1, D2 and D3 for each of the three countries containing the 
#observations of funding type FT falling within the 5-15 million USD range

D1 <- filter(sector_frame, sector_frame$country_code=="USA" & sector_frame$funding_round_type=="venture"
             & sector_frame$raised_amount_usd>=5000000 & sector_frame$raised_amount_usd<=15000000) 
D2 <- filter(sector_frame, sector_frame$country_code=="GBR" & sector_frame$funding_round_type=="venture"
             & sector_frame$raised_amount_usd>=5000000 & sector_frame$raised_amount_usd<=15000000) 
D3 <- filter(sector_frame, sector_frame$country_code=="IND" & sector_frame$funding_round_type=="venture"
             & sector_frame$raised_amount_usd>=5000000 & sector_frame$raised_amount_usd<=15000000) 

#Getting the total number (or count) of investments for each main sector in a separate column

D1_sector_cnt<-aggregate(D1$funding_round_permalink,by=list(D1$main_sector),FUN=length) #Finding count of investments for each main sector for Country 1
colnames(D1_sector_cnt)<-c("main_sector","Count_of_Investments") # Renaming aggregated column
D1<-merge(D1, D1_sector_cnt, by="main_sector", all.x = TRUE) # Merging count of investments to Data frame D1

D2_sector_cnt<-aggregate(D2$funding_round_permalink,by=list(D2$main_sector),FUN=length) #Finding count of investments for each main sector for Country 2
colnames(D2_sector_cnt)<-c("main_sector","Count_of_Investments") # Renaming aggregated column
D2<-merge(D2, D2_sector_cnt, by="main_sector", all.x = TRUE) # Merging count of investments to Data frame D2

D3_sector_cnt<-aggregate(D3$funding_round_permalink,by=list(D3$main_sector),FUN=length) #Finding count of investments for each main sector for Country 3
colnames(D3_sector_cnt)<-c("main_sector","Count_of_Investments") # Renaming aggregated column
D3<-merge(D3, D3_sector_cnt, by="main_sector", all.x = TRUE) # Merging count of investments to Data frame D3

#Getting the total amount invested in each main sector in a separate column

D1_sector_amt<-aggregate(D1$raised_amount_usd,by=list(D1$main_sector),FUN=sum, na.rm=T) #Finding total amount invested in main each sector for Country 1
colnames(D1_sector_amt)<-c("main_sector","Total_amt_Invested") # Renaming aggregated column
D1<-merge(D1, D1_sector_amt, by="main_sector", all.x = TRUE) # Merging total amount invested to Data frame D1

D2_sector_amt<-aggregate(D2$raised_amount_usd,by=list(D2$main_sector),FUN=sum, na.rm=T) #Finding total amount invested in main each sector for Country 2
colnames(D2_sector_amt)<-c("main_sector","Total_amt_Invested") # Renaming aggregated column
D2<-merge(D2, D2_sector_amt, by="main_sector", all.x = TRUE) # Merging total amount invested to Data frame D2

D3_sector_amt<-aggregate(D3$raised_amount_usd,by=list(D3$main_sector),FUN=sum, na.rm=T) #Finding total amount invested in main each sector for Country 3
colnames(D3_sector_amt)<-c("main_sector","Total_amt_Invested") # Renaming aggregated column
D3<-merge(D3, D3_sector_amt, by="main_sector", all.x = TRUE) # Merging total amount invested to Data frame D3

# 1. Total number of investments (count)

summarise(D1,length(D1$raised_amount_usd))
summarise(D2,length(D2$raised_amount_usd))
summarise(D3,length(D3$raised_amount_usd))

# 2. Total amount of investment (USD)

summarise(D1,sum(D1$raised_amount_usd,na.rm = T))
summarise(D2,sum(D2$raised_amount_usd,na.rm = T))
summarise(D3,sum(D3$raised_amount_usd,na.rm = T))

# Top 3 sector (based on count of investments)
D1_top3_count <- top_n(D1_sector_cnt,3,D1_sector_cnt$Count_of_Investments)
arrange(D1_top3_count,desc(D1_top3_count$Count_of_Investments))

D2_top3_count <- top_n(D2_sector_cnt,3,D2_sector_cnt$Count_of_Investments)
arrange(D2_top3_count,desc(D2_top3_count$Count_of_Investments))

D3_top3_count <- top_n(D3_sector_cnt,3,D3_sector_cnt$Count_of_Investments)
arrange(D3_top3_count,desc(D3_top3_count$Count_of_Investments))

#For the top sector count-wise (point 3), which company received the highest investment?

D1_sector_top1<-filter(D1,D1$main_sector=="Others")
D1_sector_top1_amt<-aggregate(D1_sector_top1$raised_amount_usd,by=list(D1_sector_top1$company_permalink),FUN=sum, na.rm=T)
top_n(D1_sector_top1_amt,1,x)

D2_sector_top1<-filter(D2,D2$main_sector=="Others")
D2_sector_top1_amt<-aggregate(D2_sector_top1$raised_amount_usd,by=list(D2_sector_top1$company_permalink),FUN=sum, na.rm=T)
top_n(D2_sector_top1_amt,1,x)

D3_sector_top1<-filter(D3,D3$main_sector=="Others")
D3_sector_top1_amt<-aggregate(D3_sector_top1$raised_amount_usd,by=list(D3_sector_top1$company_permalink),FUN=sum, na.rm=T)
top_n(D3_sector_top1_amt,1,x)

#10. For the second-best sector count-wise (point 4), which company received the highest investment?

D1_sector_top2<-filter(D1,D1$main_sector=="Social..Finance..Analytics..Advertising")
D1_sector_top2_amt<-aggregate(D1_sector_top2$raised_amount_usd,by=list(D1_sector_top2$company_permalink),FUN=sum, na.rm=T)
top_n(D1_sector_top2_amt,1,x)

D2_sector_top2<-filter(D2,D2$main_sector=="Social..Finance..Analytics..Advertising")
D2_sector_top2_amt<-aggregate(D2_sector_top2$raised_amount_usd,by=list(D2_sector_top2$company_permalink),FUN=sum, na.rm=T)
top_n(D2_sector_top2_amt,1,x)

D3_sector_top2<-filter(D3,D3$main_sector=="Social..Finance..Analytics..Advertising")
D3_sector_top2_amt<-aggregate(D3_sector_top2$raised_amount_usd,by=list(D3_sector_top2$company_permalink),FUN=sum, na.rm=T)
top_n(D3_sector_top2_amt,1,x)
