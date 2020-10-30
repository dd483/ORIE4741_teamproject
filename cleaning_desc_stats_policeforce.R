library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(mltools)
library(stringr)

df = read.csv("Police_Use_of_Force_nonmissing.csv")
dim(df)

##CLEANING DATA
#check missing values
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

#trim and view proportions
table(df$Problem)
df$Problem = str_trim(df$Problem)

table(df$Is911Call)
df$Is911Call = str_trim(df$Is911Call)

table(df$PrimaryOffense)
df$PrimaryOffense = str_trim(df$PrimaryOffense)

table(df$SubjectInjury)

table(df$ForceReportNumber)

table(df$SubjectRole)
df$SubjectRole = str_trim(df$SubjectRole)

table(df$SubjectRoleNumber)

table(df$ForceType)
df$ForceType = str_trim(df$ForceType)

table(df$ForceTypeAction)
df$ForceTypeAction = str_trim(df$ForceTypeAction)

table(df$Race)
df$Race = str_trim(df$Race)

table(df$Sex)

table(df$TypeOfResistance)
df$TypeofResistance = str_trim(df$TypeofResistance)

table(df$Precinct)

table(df$Neighborhood)
df$Neighborhood = str_trim(df$Neighborhood)

#drop erroneous precincts
df = df[df$Precinct != "0",]
df = df[df$Precinct != "0M",]
df = df[df$Precinct != "0O",]
df = df[df$Precinct != "0U",]

#check latitudes
summary(df$CenterLatitude)

df = df[df$Race != "not recorded",]
df = df[df$Sex != "not recorded",] #drops 5
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


#standardize string entries
df$TypeOfResistance = gsub(" ", "", df$TypeOfResistance, fixed = TRUE) 
df$TypeOfResistance[df$TypeOfResistance == "AssaultedOfficer" | 
                      df$TypeOfResistance == "AssaultedPoliceK9" |
                      df$TypeOfResistance == "AssaultingPoliceK9"] = "Assault Officer"
df$TypeOfResistance[df$TypeOfResistance == "AssaultingPoliceHorse" |
                      df$TypeOfResistance == "AssaultedPoliceHorse"] = "Assault Police Horse"
df$TypeOfResistance[df$TypeOfResistance == "CommissionofaCrime" | 
                      df$TypeOfResistance == "commissionofcrime" |
                      df$TypeOfResistance == "CommissionofCrime" |
                      df$TypeOfResistance == "COMMISSIONOFCRIME"] = "Commission of Crime"
df$TypeOfResistance[df$TypeOfResistance == "FledinaVehicle" |
                      df$TypeOfResistance == "FledinVehicle"] = "Fled in Vehicle"
df$TypeOfResistance[df$TypeOfResistance == "FledonFoot"] = "Fled on Foot"
df$TypeOfResistance[df$TypeOfResistance == "tensed" |
                      df$TypeOfResistance == "Tensed" |
                      df$TypeOfResistance == "TENSED"] = "Tensed"
df$TypeOfResistance[df$TypeOfResistance == "verbalnon-compliance" |
                      df$TypeOfResistance == "VerbalNon-Compliance"] = "Verbal Non-Compliance"

df$ForceTypeAction[df$ForceTypeAction == "Body Weight Pin"] = "Body Weight to Pin"
df$ForceTypeAction[df$ForceTypeAction == "Knee"] = "Knees"
df$ForceTypeAction[df$ForceTypeAction == "Punch"] = "Punches"
df$ForceTypeAction[df$ForceTypeAction == "Slap"] = "Slaps"


df$ResponseDate_year=as.numeric(substr(df$ResponseDate, 1,4))
df$ResponseDate_month=as.numeric(substr(df$ResponseDate,6,7))
df$ResponseDate_day=as.numeric(substr(df$ResponseDate,9,10))
df$ResponseDate_hour=as.numeric(substr(df$ResponseDate,12,13))
df$ResponseDate_minute=as.numeric(substr(df$ResponseDate,15,16))

write.csv(df,"Police_Use_of_Force_cleaned.csv")


#PREPROCESSING
df = read.csv("Police_Use_of_Force_cleaned.csv")
df$Neighborhood_m = "Other"
df$Neighborhood_m[df$Neighborhood == "Downtown West"] = "Downtown West"
df$Neighborhood_m[df$Neighborhood == "Jordan"] = "Jordan"
df$Neighborhood_m[df$Neighborhood == "Hawthorne"] = "Hawthorne"
df$Neighborhood_m[df$Neighborhood == "Near - North"] = "Near - North"
df$Neighborhood_m[df$Neighborhood == "Willard - Hay"] = "Willard - Hay"

df$SubjectRole_m = "Other"
df$SubjectRole_m[df$SubjectRole == "A"] = "A"
df$SubjectRole_m[df$SubjectRole == "ARRESTEE"] = "ARRESTEE"
df$SubjectRole_m[df$SubjectRole == "S"] = "S"
df$SubjectRole_m[df$SubjectRole == "OT"] = "OT"
df$SubjectRole_m[df$SubjectRole == "SUSPECT"] = "SUSPECT"

df$PrimaryOffense_m = "Other"
df$PrimaryOffense_m[df$PrimaryOffense == "OBSTRU"] = "OBSTRU"
df$PrimaryOffense_m[df$PrimaryOffense == "DISCON"] = "DISCON"
df$PrimaryOffense_m[df$PrimaryOffense == "ASLT4"] = "ASLT4"
df$PrimaryOffense_m[df$PrimaryOffense == "FORCE"] = "FORCE"
df$PrimaryOffense_m[df$PrimaryOffense == "NARC"] = "NARC"

df$Problem_m = "Other"
df$Problem_m[df$Problem == "Suspicious Person"] = "Suspicious Person"
df$Problem_m[df$Problem == "Fight"] = "Fight"
df$Problem_m[df$Problem == "Disturbance"] = "Disturbance"
df$Problem_m[df$Problem == "Domestic Abuse-In Progress"] = "Domestic Abuse-In Progress"
df$Problem_m[df$Problem == "Suspicious Vehicle"] = "Suspicious Vehicle"
df$Problem_m[df$Problem == "Traffic Law Enforcement"] = "Traffic Law Enforcement"
df$Problem_m[df$Problem == "Unknown Trouble"] = "Unknown Trouble"
df$Problem_m[df$Problem == "Assault in Progress"] = "Assault in Progress"
df$Problem_m[df$Problem == "Unwanted Person"] = "Unwanted Person"
df$Problem_m[df$Problem == "Emotionally Disturb Person"] = "Emotionally Disturb Person"

df$ForceTypeAction_m = "Other"
df$ForceTypeAction_m[df$ForceTypeAction == "Body Weight to Pin"] = "Body Weight to Pin"
df$ForceTypeAction_m[df$ForceTypeAction == "Punches"] = "Punches"
df$ForceTypeAction_m[df$ForceTypeAction == "Joint Lock"] = "Joint Lock"
df$ForceTypeAction_m[df$ForceTypeAction == "Personal Mace"] = "Personal Mace"
df$ForceTypeAction_m[df$ForceTypeAction == "Push Away"] = "Push Away"
df$ForceTypeAction_m[df$ForceTypeAction == "Knees"] = "Knees"
df$ForceTypeAction_m[df$ForceTypeAction == "Crowd Control Mace"] = "Crowd Control Mace"
df$ForceTypeAction_m[df$ForceTypeAction == "Firing Darts"] = "Firing Darts"
df$ForceTypeAction_m[df$ForceTypeAction == "Kicks"] = "Kicks"
df$ForceTypeAction_m[df$ForceTypeAction == "Takedown"] = "Takedown"


write.csv(df,"Police_Use_of_Force_preprocessed.csv")



##ONE HOT
df = read.csv("Police_Use_of_Force_preprocessed.csv")

df$Precinct = as.factor(df$Precinct)
data = df
reqd = c("Problem_m", "Is911Call", "PrimaryOffense_m","SubjectInjury","SubjectRole_m",
          "ForceTypeAction_m","Race","Sex","TypeOfResistance","Precinct","Neighborhood_m")
data = data[,reqd]
newdata = one_hot(as.data.table(data))
newdata[is.na(newdata)] = 0
data = cbind(df,newdata)
drop = c("X.3", "X.2", "X.1","Problem", "Is911Call", "PrimaryOffense","SubjectInjury","SubjectRole",
         "ForceTypeAction","Race","Sex","TypeOfResistance","Precinct","Neighborhood",
         "Neighborhood_m","SubjectRole_m","PrimaryOffense_m","Problem_m","ForceTypeAction_m")
data = data[ , !(names(data) %in% drop)]
df = data
write.csv(data,"Police_Use_of_Force_onehot.csv")




##VISUALIZATIONS
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
data = df %>%
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

