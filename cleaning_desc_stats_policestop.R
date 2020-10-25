library(dplyr)
library(tidyr)
library(ggplot2)

df = read.csv("Police_Stop_Data_nonmissing.csv")
dim(df)
colnames(df)

table(df$reason)
table(df$problem)
table(df$callDisposition)
table(df$citationIssued)
table(df$personSearch)
table(df$vehicleSearch)
table(df$preRace)
table(df$race)
table(df$gender)
table(df$policePrecinct)
table(df$neighborhood)
summary(df$lat)

df = df[df$lat > 0,]

summary(df$lat)
summary(df$long)

barplot(prop.table(table(df$citationIssued)), main="Citation Issued Proportion",
        ylim=c(0,1))

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


