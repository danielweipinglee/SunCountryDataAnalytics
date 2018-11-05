# This program contains the R code for data clean-up and pre-processing for the Sun Country Airlines case

# We start by reading the data file, which is a 1% random sample of the original airline reservations data

# We set the working directory using setwd()

setwd("/Users/sdewan/Dropbox/BANA 200/R Programs and Data")

# Then we read the data using the read.csv function

data <- read.csv("SC_Data.csv")

# Use head() to see the first 6 rows of the data

head(data)

# We will be using the dplyr package for clean up and pre-processing 

install.packages("dplyr")
library(dplyr)

#Only keep records where we know the birthdate
filter(data, !is.na(birthdateid))

#Only retain records where we know the gender
data$GenderCode<-as.character(data$GenderCode)
filter(data, GenderCode!="")
data$GenderCode<-as.factor(data$GenderCode)

#Some odd age values... we'll replace with the median.
data$Age[data$Age < 0] <- median(data$Age)
data$Age[data$Age > 120] <- median(data$Age)

#If there is someone does not have a reward number we assign it a 0
data$UFlyRewardsNumber[is.na(data$UFlyRewardsNumber)]<-0

#We construct a reward status factor variable.
data$UflyMemberStatus<-as.character(data$UflyMemberStatus)
data$UflyMemberStatus[data$UflyMemberStatus==''] <-"non-ufly"

#Discard duplicate records
data <- group_by(data, PNRLocatorID,CouponSeqNbr,PaxName,ServiceStartCity,ServiceEndCity,ServiceStartDate)
filter(data, n() == 1)

#Replace odd one off booking channels with 'Other'
data$BookingChannel<-as.character(data$BookingChannel)
data$BookingChannel[data$BookingChannel!="Outside Booking" & data$BookingChannel!="SCA Website Booking" & data$BookingChannel!="Tour Operator Portal" & data$BookingChannel!="Reservations Booking" & data$BookingChannel!="SY Vacation"] <- "Other"
data$BookingChannel<-as.factor(data$BookingChannel)

#Now we only keep records that involve SunCountry airlines tickets, for which MarketingAirlineCode=="SY".
data$MarketingAirlineCode<-as.character(data$MarketingAirlineCode)
filter(data,MarketingAirlineCode=="SY")
data$MarketingAirlineCode<-as.factor(data$MarketingAirlineCode)

#Delete PNRs that have odd values and indicate an error.
data <- group_by(data, PNRLocatorID)
data <- mutate(data, error=ifelse(min(CouponSeqNbr)!=1,1,0))
filter(data, error==0)

#Here we create a unique customer ID by concatenating name, gender and birthday
mutate (data, customerID = paste(PaxName, GenderCode, as.character(birthdateid),sep="-")) 


#Now we create Age buckets for age ranges, creating a new categorical variable "age_group" with the following posisble values: "0-17", "18-24", "25-34", "35-54", "55+" and "N/A"
mutate(data,age_group=
         ifelse(Age>=0&Age<=17,"0-17",
                ifelse(Age>17&Age<=24,"18-24",
                       ifelse(Age>24&Age<=34,"25-34",
                              ifelse(Age>34&Age<=54,"35-54",
                                     ifelse(Age>=55,"55+",
                                            ifelse("N/A")))))))

#For a given PNR, figure out true origin city (source of first leg)
true_origins <- data%>%
  arrange(PNRLocatorID,CouponSeqNbr)%>% 
  group_by(PNRLocatorID,PaxName)%>% 
  do(data.frame(true_origin=first(.$ServiceStartCity)))

data<-merge(data,true_origins, by.x=c("PNRLocatorID","PaxName"),by.y = c("PNRLocatorID","PaxName"))

#For a given PNR, we figure out final destination (target of last leg), 
#then merge the "data" and "final_destination" data frames, as we did earlier for true_origins.
final_destination<-data%>%
  arrange(PNRLocatorID,CouponSeqNbr)%>%
  group_by(PNRLocatorID,PaxName)%>%
  do(data.frame(final_destination=last(.$ServiceEndCity)))

data<-merge(data,final_destination, by.x=c("PNRLocatorID","PaxName"),by.y = c("PNRLocatorID","PaxName"))

# Now, we remove the true_origins and final_destination data frames
rm(true_origins)
rm(final_destination)

# We will use the lubridate package for operations involving date strings
install.packages("lubridate")
library(lubridate)

#Now figure out "true" destination, city in which customer spent the most time
diff1<-data%>%arrange(PNRLocatorID,CouponSeqNbr)%>% 
  group_by(PNRLocatorID,PaxName)%>%
  mutate(stay=lead(date(ServiceStartDate))-date(ServiceStartDate),default=0)%>% 
  select(PNRLocatorID,PaxName,ServiceStartCity,ServiceEndCity,ServiceStartDate,stay)

diff1$stay[is.na(diff1$stay)]<-0 
diff1$stay<-as.numeric(diff1$stay) 
true_destination<-diff1%>%
  group_by(PNRLocatorID,PaxName)%>% 
  do(data.frame(true_destination=first(.$ServiceEndCity[.$stay==max(.$stay)])))

data<-merge(data,true_destination, by.x=c("PNRLocatorID","PaxName"),by.y = c("PNRLocatorID","PaxName"))

rm(diff1)
rm(true_destination)

#Here we check whether is the booking a round-trip or one-way
data<-data%>%
  mutate(round_trip = ifelse(as.character(true_origins)==as.character(final_destination), 1, 0))

#Determine the size of the group
data<-data%>%mutate(uid=paste(EncryptedName,GenderCode,birthdateid,sep=""))
data<-data%>%
  group_by(PNRLocatorID)%>% 
  mutate(group_size = length(unique(uid)))

#Create a binary indicator "group" corresponding to whether it was a group or single party traveling.
data<-data%>%
  group_by(PNRLocatorID)%>% 
  mutate(group= ifelse(group_size>1,1,0))

#We Figure out which calendar quarter the trip took place in. 
#Create a categorical variable "Seasonality" which takes on the following values: 
#"Q1" for months 1:3, "Q2" for months 4:6, "Q3" for months 7:9 and "Q4" for months 10:12

#method 1
mutate(data,Seasonality=ifelse(month(data$ServiceStartDate)>=1 & month(data$ServiceStartDate)<=3,"Q1",
                               ifelse(month(data$ServiceStartDate)>=4 & month(data$ServiceStartDate)<=6,"Q2",
                                      ifelse(month(data$ServiceStartDate)>=7 & month(data$ServiceStartDate)<=9,"Q3",
                                             ifelse(month(data$ServiceStartDate)>=10 & month(data$ServiceStartDate)<=12,"Q4",
                                                    ifelse("NA"))))))
#method 2
data$Seasonality[month(data$ServiceStartDate)>=1 & month(data$ServiceStartDate)<=3] <-"Q1"
data$Seasonality[month(data$ServiceStartDate)>=4 & month(data$ServiceStartDate)<=6] <-"Q2"
data$Seasonality[month(data$ServiceStartDate)>=7 & month(data$ServiceStartDate)<=9] <-"Q3"
data$Seasonality[month(data$ServiceStartDate)>=10 & month(data$ServiceStartDate)<=12] <-"Q4"

#We figure out how many days in advance was the trip booked
data$PNRCreateDate<-as.Date(data$PNRCreateDate) 
data$ServiceStartDate<-as.Date(data$ServiceStartDate) 
data<-data%>%
  mutate(days_pre_booked=as.numeric(floor(difftime(ServiceStartDate,PNRCreateDate,units=c("days")))))

write.csv(data, "SC_data_CleanedUp.csv")