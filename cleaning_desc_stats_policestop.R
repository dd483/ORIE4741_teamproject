library(dplyr)
library(tidyr)
library(ggplot2)

df = read.csv("Police_Stop_Data_nonmissing.csv")
dim(df)
colnames(df)

##CLEANING DATA
#missing values
summary(is.na(df$reason))
summary(is.na(df$problem))
summary(is.na(df$callDisposition))
summary(is.na(df$personSearch))
summary(is.na(df$vehicleSearch))
summary(is.na(df$preRace))
summary(is.na(df$race))
summary(is.na(df$gender))
summary(is.na(df$policePrecinct))
summary(is.na(df$neighborhood))


#trim and view proportions
table(df$reason)
table(df$problem)
table(df$callDisposition)
df$callDisposition = str_trim(df$callDisposition)
table(df$citationIssued)
table(df$personSearch)
table(df$vehicleSearch)
table(df$preRace)
table(df$race)
table(df$gender)
table(df$policePrecinct)
table(df$neighborhood)
df$neighborhood = str_trim(df$neighborhood)
summary(df$lat)

#drop erroneous precincts
df = df[df$lat > 0,]
df = df[!is.na(df$policePrecinct),]
summary(df$lat)
summary(df$long)


barplot(prop.table(table(df$citationIssued)), main="Citation Issued Proportion",
        ylim=c(0,1))

df$responseDate_year=as.numeric(substr(df$responseDate, 1,4))
df$responseDate_month=as.numeric(substr(df$responseDate,6,7))
df$responseDate_day=as.numeric(substr(df$responseDate,9,10))
df$responseDate_hour=as.numeric(substr(df$responseDate,12,13))
df$responseDate_minute=as.numeric(substr(df$responseDate,15,16))

write.csv(df,"Police_Stop_Data_cleaned.csv")

#PREPROCESSING
df = read.csv("Police_Stop_Data_cleaned.csv")
df$callDisposition_m = "Other"
df$callDisposition_m[df$callDisposition == "ADV-Advised"] = "ADV-Advised"
df$callDisposition_m[df$callDisposition == "GOA-Gone on Arrival"] = "GOA-Gone on Arrival"
df$callDisposition_m[df$callDisposition == "TAG-Tagged"] = "TAG-Tagged"
df$callDisposition_m[df$callDisposition == "AOK- All OK"] = "AOK- All OK"
df$callDisposition_m[df$callDisposition == "BKG-Booking"] = "BKG-Booking"
df$callDisposition_m[df$callDisposition == "SNT-Sent"] = "SNT-Sent"
df$callDisposition_m[df$callDisposition == "RPT-Report"] = "RPT-Report"
df$callDisposition_m[df$callDisposition == "UTL-Unable to Locate"] = "UTL-Unable to Locate"
df$callDisposition_m[df$callDisposition == "INF-Information"] = "INF-Information"
df$callDisposition_m[df$callDisposition == "TRN-Transport"] = "TRN-Transport"

df$neighborhood_m = "Other"
df$neighborhood_m[df$neighborhood == "Downtown West"] = "Downtown West"
df$neighborhood_m[df$neighborhood == "Hawthorne"] = "Hawthorne"
df$neighborhood_m[df$neighborhood == "Near - North"] = "Near - North"
df$neighborhood_m[df$neighborhood == "Whittier"] = "Whittier"
df$neighborhood_m[df$neighborhood == "Jordan"] = "Jordan"
df$neighborhood_m[df$neighborhood == "Marcy Holmes"] = "Marcy Holmes"
df$neighborhood_m[df$neighborhood == "Lyndale"] = "Lyndale"
df$neighborhood_m[df$neighborhood == "Folwell"] = "Folwell"
df$neighborhood_m[df$neighborhood == "Willard - Hay"] = "Willard - Hay"
df$neighborhood_m[df$neighborhood == "Lowry Hill East"] = "Lowry Hill East"

write.csv(df,"Police_Stop_Data_preprocessed.csv")

##ONE HOT
df = read.csv("Police_Stop_Data_preprocessed.csv")

df$policePrecinct = as.factor(df$policePrecinct)
data = df
reqd = c("reason", "problem", "callDisposition_m","personSearch","vehicleSearch",
         "preRace","race","gender","policePrecinct","neighborhood_m")
data = data[,reqd]
newdata = one_hot(as.data.table(data))
newdata[is.na(newdata)] = 0
data = cbind(df,newdata)
drop = c("X","callDisposition_m","neighborhood_m","reason","problem","personSearch",
         "vehicleSearch","preRace","race","gender","policePrecinct","callDisposition",
         "neighborhood")
data = data[ , !(names(data) %in% drop)]
df = data
write.csv(df,"Police_Stop_Data_onehot.csv")





#RACE AND CITATION
data = df %>%
  group_by(citationIssued, race) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n))

ggplot(data, aes(x = race, y = freq))+
  geom_bar(
    aes(fill = citationIssued), stat = "identity", color = "white",
    position = position_dodge(0.9)
  ) +  
  xlab("Race") + 
  ylab("Proportional Frequency") +
  ggtitle("Citation Outcome in Overall Proportions by Race") 
  

data = df %>%
  group_by(citationIssued, race) %>%
  summarise(n = n()) 
data = spread(data, citationIssued, n)
data$n = data$NO + data$YES
data$NO = data$NO / data$n
data$YES = data$YES / data$n
data = data[,1:3]

data_long = gather(data, citationIssued, 
                    prop, NO:YES, factor_key=TRUE)
ggplot(data_long, aes(x = race, y = prop))+
  geom_bar(
    aes(fill = citationIssued), stat = "identity", color = "white",
    position = position_dodge(0.9)
  ) +  
  xlab("Race") + 
  ylab("Proportional Frequency") +
  ggtitle("Citation Outcome in Race Proportions") 


#PRECINCT AND CITATION
data = df %>%
  group_by(citationIssued, policePrecinct) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n))

ggplot(data, aes(x = policePrecinct, y = freq))+
  geom_bar(
    aes(fill = citationIssued), stat = "identity", color = "white",
    position = position_dodge(0.9)
  ) +  
  xlab("Precinct") + 
  ylab("Proportional Frequency") +
  ggtitle("Citation Outcome in Overall Proportions by Precinct") 


data = df %>%
  group_by(citationIssued, policePrecinct) %>%
  summarise(n = n()) 
data = spread(data, citationIssued, n)
data$n = data$NO + data$YES
data$NO = data$NO / data$n
data$YES = data$YES / data$n
data = data[,1:3]

data_long = gather(data, citationIssued, 
                   prop, NO:YES, factor_key=TRUE)
ggplot(data_long, aes(x = policePrecinct, y = prop))+
  geom_bar(
    aes(fill = citationIssued), stat = "identity", color = "white",
    position = position_dodge(0.9)
  ) +  
  xlab("Precinct") + 
  ylab("Proportional Frequency") +
  ggtitle("Citation Outcome in Precinct Proportions") 





#PROBLEM AND CITATION
data = df %>%
  group_by(citationIssued, problem) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n))

ggplot(data, aes(x = problem, y = freq))+
  geom_bar(
    aes(fill = citationIssued), stat = "identity", color = "white",
    position = position_dodge(0.9)
  ) +  
  xlab("Problem") + 
  ylab("Proportional Frequency") +
  ggtitle("Citation Outcome in Overall Proportions") 


data = df %>%
  group_by(citationIssued, problem) %>%
  summarise(n = n()) 
data = spread(data, citationIssued, n)
data$n = data$NO + data$YES
data$NO = data$NO / data$n
data$YES = data$YES / data$n
data = data[,1:3]

data_long = gather(data, citationIssued, 
                   prop, NO:YES, factor_key=TRUE)
ggplot(data_long, aes(x = problem, y = prop))+
  geom_bar(
    aes(fill = citationIssued), stat = "identity", color = "white",
    position = position_dodge(0.9)
  ) +  
  xlab("Precinct") + 
  ylab("Proportional Frequency") +
  ggtitle("Citation Outcome in Problem Proportions") 






#REASON AND CITATION
data = df %>%
  group_by(citationIssued, reason) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n))

ggplot(data, aes(x = reason, y = freq))+
  geom_bar(
    aes(fill = citationIssued), stat = "identity", color = "white",
    position = position_dodge(0.9)
  ) +  
  xlab("Problem") + 
  ylab("Proportional Frequency") +
  ggtitle("Citation Outcome in Overall Proportions by Problem") 


data = df %>%
  group_by(citationIssued, reason) %>%
  summarise(n = n()) 
data = spread(data, citationIssued, n)
data$n = data$NO + data$YES
data$NO = data$NO / data$n
data$YES = data$YES / data$n
data = data[,1:3]

data_long = gather(data, citationIssued, 
                   prop, NO:YES, factor_key=TRUE)
ggplot(data_long, aes(x = reason, y = prop))+
  geom_bar(
    aes(fill = citationIssued), stat = "identity", color = "white",
    position = position_dodge(0.9)
  ) +  
  xlab("Precinct") + 
  ylab("Proportional Frequency") +
  ggtitle("Citation Outcome in Reason Proportions") 


