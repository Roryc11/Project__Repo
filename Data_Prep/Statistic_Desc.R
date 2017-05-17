
############################################## Count of Accidents by Vehicle Type ##############################################
# Import CSV files to Data frames
accidents <- read.csv("dft-accident-data/all_accidents_info.csv", TRUE)
list_of_VT <- read.csv("dft-accident-data/contextCSVs/Vehicle_Type.csv", TRUE)

head(accidents, n = 1)
acc_by_VT <- data.frame(Index = numeric(0) , stringsAsFactors = TRUE)
# Create a Data frame with the number of accidents per Vehicle Type
for (i in 1:nrow(list_of_VT)) {
    list_of_VT[i, 2]
    acc_by_VT[i, "Index"] <- list_of_VT[i, 1]
    acc_by_VT[i, "Value"] <- list_of_VT[i, 2]
    acc_by_VT[i, "Count"] <- sum(accidents$Vehicle_Type == list_of_VT[i, 1], na.rm = TRUE)
}
summary(acc_by_VT)
acc_by_VT <- acc_by_VT[order(-acc_by_VT$Count),]
acc_by_VT


write.csv(acc_by_VT, file = "dft-accident-data/Statistic_Summaries/Vehicle/Vehicle_Frequency.csv", row.names = FALSE)


########################################## Count of Accidents by Vehicle Age Ranges ###########################################
age_ranges <- c("0-4","5-9","10-14","15-19","20-24","25-29","30+")
age_ranges[1]
attach(accidents)
x4 <- sum(Age_of_Vehicle >= 0 & Age_of_Vehicle < 5, na.rm = TRUE)
x9 <- sum(Age_of_Vehicle >= 5 & Age_of_Vehicle < 10, na.rm = TRUE)
x14 <- sum(Age_of_Vehicle >= 10 & Age_of_Vehicle < 15, na.rm = TRUE)
x19 <- sum(Age_of_Vehicle >= 15 & Age_of_Vehicle < 20, na.rm = TRUE)
x24 <- sum(Age_of_Vehicle >= 20 & Age_of_Vehicle < 25, na.rm = TRUE)
x29 <- sum(Age_of_Vehicle >= 25 & Age_of_Vehicle < 30, na.rm = TRUE)
x30_ <- sum(Age_of_Vehicle >= 30 & Age_of_Vehicle < Inf, na.rm = TRUE)
no_vehicles <- c(x4, x9, x14, x19, x24, x29, x30_)
detach(accidents)
vehicles_age_sum <- data.frame(age_ranges,no_vehicles)
summary(vehicles_age_sum)

write.csv(acc_by_VT, file = "dft-accident-data/Statistic_Summaries/Vehicle/Vehicle_Age_Frequency.csv", row.names = FALSE)


########################################## Top 10 Vehicle Ages to be in an Accidents ###########################################
top10_VehicleAges <- sort(summary(as.factor(accidents$Age_of_Vehicle)), decreasing = TRUE)[1:10]
write.csv(top10_VehicleAges, file = "dft-accident-data/Statistic_Summaries/Vehicle/Top10_VehicleAges.csv", row.names = TRUE)


################################## Count of Accidents by Vehicle Age Ranges and Vehicle Type #####################################
Acc_VT_Age <- data.frame()
# Create a Data frame with the number of accidents per Vehicle Type
attach(accidents)
for (i in 1:nrow(list_of_VT)) {
    list_of_VT[i, 2]
    Acc_VT_Age[i, "Vehicle_Type"] <- list_of_VT[i, 2]
    Acc_VT_Age[i, "0_4"] <- sum(Age_of_Vehicle >= 0 & Age_of_Vehicle < 5 & Vehicle_Type == list_of_VT[i, 1], na.rm = TRUE)
    Acc_VT_Age[i, "5_9"] <- sum(Age_of_Vehicle >= 5 & Age_of_Vehicle < 10 & Vehicle_Type == list_of_VT[i, 1], na.rm = TRUE)
    Acc_VT_Age[i, "10_14"] <- sum(Age_of_Vehicle >= 10 & Age_of_Vehicle < 15 & Vehicle_Type == list_of_VT[i, 1], na.rm = TRUE)
    Acc_VT_Age[i, "15_19"] <- sum(Age_of_Vehicle >= 15 & Age_of_Vehicle < 20 & Vehicle_Type == list_of_VT[i, 1], na.rm = TRUE)
    Acc_VT_Age[i, "20_24"] <- sum(Age_of_Vehicle >= 20 & Age_of_Vehicle < 25 & Vehicle_Type == list_of_VT[i, 1], na.rm = TRUE)
    Acc_VT_Age[i, "25_30"] <- sum(Age_of_Vehicle >= 25 & Age_of_Vehicle < 30 & Vehicle_Type == list_of_VT[i, 1], na.rm = TRUE)
    Acc_VT_Age[i, "30+"] <- sum(Age_of_Vehicle >= 30 & Age_of_Vehicle < Inf & Vehicle_Type == list_of_VT[i, 1], na.rm = TRUE)
}
detach(accidents)
Acc_VT_Age <- Acc_VT_Age[order(-Acc_VT_Age$`0_4`),]
summary(Acc_VT_Age)
write.csv(Acc_VT_Age, file = "dft-accident-data/Statistic_Summaries/Vehicle/Accident_By_VehAge_And_VehType.csv", row.names = TRUE)



########################################## Count of Accidents by Casuatly Severity #############################################
casualty_Severity_Types <- read.csv("dft-accident-data/contextCSVs/Casualty_Severity.csv", TRUE)
Acc_Casualty_Secerity <- data.frame()

attach(accidents)
for (i in 1:nrow(casualty_Severity_Types)) {

    Acc_Casualty_Secerity[i, "Severity"] <- as.factor(casualty_Severity_Types[i, 2])
    Acc_Casualty_Secerity[i, "No_Accidents"] <- sum(Casualty_Severity == casualty_Severity_Types[i, 1], na.rm = TRUE)
}
detach(accidents)
Acc_Casualty_Secerity <- Acc_Casualty_Secerity[order(-Acc_Casualty_Secerity$No_Accidents),]
write.csv(Acc_Casualty_Secerity, file = "dft-accident-data/Statistic_Summaries/Casualty/Accident_By_Severity.csv", row.names = TRUE)


#################################### Count of Accidents by Casuatly Severity and Vehicle Type #######################################
Acc_Severity_VT <- data.frame()
# Create a Data frame with the number of accidents per Vehicle Type
attach(accidents)
for (i in 1:nrow(list_of_VT)) {
    list_of_VT[i, 2]
    Acc_Severity_VT[i, "Vehicle_Type"] <- list_of_VT[i, 2]
    Acc_Severity_VT[i, "Slight"] <- sum(Casualty_Severity == 3 & Vehicle_Type == list_of_VT[i, 1], na.rm = TRUE)
    Acc_Severity_VT[i, "Serious"] <- sum(Casualty_Severity == 2 & Vehicle_Type == list_of_VT[i, 1], na.rm = TRUE)
    Acc_Severity_VT[i, "Fatal"] <- sum(Casualty_Severity == 1 & Vehicle_Type == list_of_VT[i, 1], na.rm = TRUE)
   
}
detach(accidents)

Acc_Severity_VT <- Acc_Severity_VT[order(-Acc_Severity_VT$Slight),]
write.csv(Acc_Casualty_Secerity, file = "dft-accident-data/Statistic_Summaries/Casualty/Accident_By_Severity.csv", row.names = TRUE)


#################################### Count of Accidents by Casuatly Severity and Vehicle Age #######################################
Acc_Severity_Age <- data.frame()
# Create a Data frame with the number of accidents per Vehicle Type
attach(accidents)
for (i in 1:nrow(casualty_Severity_Types)) {
    Acc_Severity_Age[i, "Severity"] <- casualty_Severity_Types[i, 2]
    Acc_Severity_Age[i, "0_4"] <- sum(Age_of_Vehicle >= 0 & Age_of_Vehicle < 5 & Casualty_Severity == casualty_Severity_Types[i, 1], na.rm = TRUE)
    Acc_Severity_Age[i, "5_9"] <- sum(Age_of_Vehicle >= 5 & Age_of_Vehicle < 10 & Casualty_Severity == casualty_Severity_Types[i, 1], na.rm = TRUE)
    Acc_Severity_Age[i, "10_14"] <- sum(Age_of_Vehicle >= 10 & Age_of_Vehicle < 15 & Casualty_Severity == casualty_Severity_Types[i, 1], na.rm = TRUE)
    Acc_Severity_Age[i, "15_19"] <- sum(Age_of_Vehicle >= 15 & Age_of_Vehicle < 20 & Casualty_Severity == casualty_Severity_Types[i, 1], na.rm = TRUE)
    Acc_Severity_Age[i, "20_24"] <- sum(Age_of_Vehicle >= 20 & Age_of_Vehicle < 25 & Casualty_Severity == casualty_Severity_Types[i, 1], na.rm = TRUE)
    Acc_Severity_Age[i, "25_30"] <- sum(Age_of_Vehicle >= 25 & Age_of_Vehicle < 30 & Casualty_Severity == casualty_Severity_Types[i, 1], na.rm = TRUE)
    Acc_Severity_Age[i, "30+"] <- sum(Age_of_Vehicle >= 30 & Age_of_Vehicle < Inf & Casualty_Severity == casualty_Severity_Types[i, 1], na.rm = TRUE)
}
detach(accidents)
Acc_Severity_Age <- Acc_Severity_Age[order(-Acc_Severity_Age$`0_4`),]
summary(Acc_Severity_Age)
write.csv(Acc_VT_Age, file = "dft-accident-data/Statistic_Summaries/Casualty/Accident_By_Severity_And_Vehicle_Age.csv", row.names = TRUE)