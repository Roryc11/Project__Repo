# Import CSV files to Data frames
accidents <- read.csv("dft-accident-data/Accidents0515.csv", TRUE)
casualties <- read.csv("dft-accident-data/Casualties0515.csv", TRUE)
vehicles <- read.csv("dft-accident-data/Vehicles0515.csv", TRUE)

head(accidents, n = 2)

accidents <- accidents[!accidents$X...Accident_Index == -1,]
casualties <- casualties[!casualties$X...Accident_Index == -1,]
vehicles <- vehicles[!vehicles$X...Accident_Index == -1,]
# Get the names of each of the context files and add them to a list
# Remove the file extentions from each item in the list
context_files <- list.files(path = "dft-accident-data/ContextCSVs", full.names = F)
context_files <- gsub("\\..*", "", context_files)

trim_data <- function(frame_to_Trim) {
    # Trims any unwanted whitespace from data frame
    frame_to_Trim <- data.frame(lapply(frame_to_Trim, trimws))
}

trim_data(accidents)
trim_data(casualties)
trim_data(vehicles)

# Loops context file name
for (i in 1:length(context_files)) {
    # checks if the name of the context file is the name of a column in the data frame
    if (context_files[i] %in% names(accidents)) {
        colname <- context_files[i]
        # imports the data from the context file to a data frame
        # As this is all string information, each context file also gets trimed to help clean the data
        x <- read.csv(paste("dft-accident-data/ContextCSVs/", context_files[i], ".csv", sep = ""), TRUE)
        x <- data.frame(lapply(x, trimws))
        for (j in 1:nrow(x)) {
            # checks the context file to get the actual value 
            # for the index referance within the accidents data frame
            accidents[colname == x[j, 1]] <- x[j, 2]
        }
        print(head(accidents[colname], n = 50))
    }
}

# Rounding Long Lat to 2 decimal places
# This is being done to get a more generallised area for crashes
accidents$Latitude <- round(accidents$Latitude, digits = 2)
accidents$Longitude <- round(accidents$Longitude, digits = 2)


# merge three data sets into one
# This was done in two sections as the merge function would not allow for 
# Three data frames to be merged together
all_accidents_info <- data.frame(merge(x = accidents, y = vehicles, all = TRUE))
all_accidents_info <- data.frame(merge(x = all_accidents_info, y = casualties, all = TRUE))



## Info In Accidents data set
all_accidents_info$Location_Easting_OSGR <- NULL
all_accidents_info$Location_Northing_OSGR <- NULL
all_accidents_info$LSOA_of_Accident_Location <- NULL
all_accidents_info$Police_Force <- NULL
all_accidents_info$Local_Authority_.District. <- NULL
all_accidents_info$Local_Authority_.Highway. <- NULL
all_accidents_info$X1st_Road_Number <- NULL
all_accidents_info$X2nd_Road_Number <- NULL
all_accidents_info$Pedestrian_Crossing.Human_Control <- NULL
all_accidents_info$Pedestrian_Crossing.Physical_Facilities <- NULL
all_accidents_info$Special_Conditions_at_Site <- NULL
all_accidents_info$Did_Police_Officer_Attend_Scene_of_Accident <- NULL
## Info In Accidents data set

## Info in Vehicle Data set
all_accidents_info$Towing_and_Articulation <- NULL
all_accidents_info$Vehicle_Location.Restricted_Lane <- NULL
all_accidents_info$Skidding_and_Overturning <- NULL
all_accidents_info$Was_Vehicle_Left_Hand_Drive. <- NULL
all_accidents_info$Propulsion_Code <- NULL
all_accidents_info$Driver_IMD_Decile <- NULL
## Info in Vehicle Data set

# Export the final Dataset to CSV
write.csv(all_accidents_info, file = "dft-accident-data/all_accidents_info.csv", row.names = FALSE)
