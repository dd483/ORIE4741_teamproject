library(dplyr)
library(tidyr)
library(ggplot2)

df = read.csv("Police_Use_of_Force_nonmissing.csv")
dim(df)

colnames(df)
summary(is.na(df$Problem))
summary(is.na(df$Is911Call))
summary(is.na(df$SubjectInjury))
summary(is.na(df$SubjectRole))
summary(is.na(df$SubjectRoleNumber))
summary(is.na(df$ForceType))
summary(is.na(df$ForceTypeAction))
summary(is.na(df$Race))
summary(is.na(df$Sex))
summary(is.na(df$TypeofResistance))
summary(is.na(df$Precinct))
summary(is.na(df$Neighborhood))


table(df$Problem)
table(df$Is911Call)
table(df$PrimaryOffense)
table(df$SubjectInjury)
table(df$SubjectRole)
table(df$SubjectRoleNumber)
table(df$ForceType)
table(df$ForceTypeAction)
table(df$Race)
table(df$Sex)
table(df$TypeOfResistance)
table(df$Precinct)
table(df$Neighborhood)

summary(df$CenterLatitude)

df = df[df$Race != "not recorded",]
df = df[df$CenterLatitude != 0,]
dim(df)

summary(df$CenterLatitude)
summary(df$CenterLongitude)
hist(df$TotalCityCallsForYear)
hist(df$TotalPrecinctCallsForYear)
hist(df$TotalNeighborhoodCallsForYear)
hist(df$EventAge)

barplot(prop.table(table(df$ForceType)), main="Force Type Proportion",
        ylim=c(0,1),cex.names=0.6)

write.csv(df,"Police_Use_of_Force_cleaned.csv")


#RACE AND FORCE TYPE
data = df %>%
  group_by(ForceType, Race) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n))

ggplot(data, aes(x = Race, y = freq))+
  geom_bar(
    aes(fill = ForceType), stat = "identity", color = "white",
    position = position_dodge(0.9)
  ) +  
  xlab("Race") + 
  ylab("Proportional Frequency") +
  ggtitle("Force Type Outcome in Overall Proportions by Race") 

data = df %>%
  group_by(ForceType, Race) %>%
  summarise(n = n()) 
data = spread(data, ForceType, n)
for (i in 1:12) {
  for (j in 1:7) {
    if (is.na(data[j,i])) {
      data[j,i] = 0  
    }    
  }
}
data$n = data$Baton + data$`Bodily Force` + data$`Chemical Irritant`+
  data$Firearm + data$`Gun Point Display` + data$`Improvised Weapon`+
  data$`Less Lethal` + data$`Less Lethal Projectile`+ data$`Maximal Restraint Technique`+
  data$`Police K9 Bite` + data$Taser
for (i in 2:12) {
  for (j in 1:7) {
    data[j,i] = data[j,i]/data[j,13]  
  }
}
data = data[,1:12]

data_long = gather(data, ForceType, 
                   prop, Baton:`Bodily Force`:`Chemical Irritant`:Firearm:`Gun Point Display`:
                     `Improvised Weapon`:`Less Lethal`:`Less Lethal Projectile`:`Maximal Restraint Technique`:
                     `Police K9 Bite`:Taser,
                   factor_key=TRUE)
ggplot(data_long, aes(x = Race, y = prop))+
  geom_bar(
    aes(fill = ForceType), stat = "identity", color = "white",
    position = position_dodge(0.9)
  ) +  
  xlab("Race") + 
  ylab("Proportional Frequency") +
  ggtitle("Citation Outcome in Race Proportions") 



#Precinct
data = df[df$Precinct != "0",]
data = data[data$Precinct != "0M",]
data = data[data$Precinct != "0O",]
data = data[data$Precinct != "0U",]
data = data %>%
  group_by(ForceType, Precinct) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n))

ggplot(data, aes(x = Precinct, y = freq))+
  geom_bar(
    aes(fill = ForceType), stat = "identity", color = "white",
    position = position_dodge(0.9)
  ) +  
  xlab("Precinct") + 
  ylab("Proportional Frequency") +
  ggtitle("Force Type Outcome in Overall Proportions by Precinct")
