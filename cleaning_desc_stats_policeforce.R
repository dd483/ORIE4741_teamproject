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
