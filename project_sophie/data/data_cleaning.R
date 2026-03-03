## chris_data.csv

chris_data<- read.csv("chris_data.csv", stringsAsFactors = FALSE)

# removed extra space before all Isla Enmedio
chris_data$location_name[45335:45338] <- trimws(chris_data$location_name[45335:45338]) 

# corrected coloumns shifted for Admiral Patch Reef row 61871
row_data <- chris_data[61871, ]
combined <- paste(row_data[7], row_data[8], sep = ",") # merge coloumns
cleaned <- gsub('"', '', combined) # remove wrong quotes
chris_data[61871, 7] <- cleaned
n_cols <- ncol(chris_data) # shift columns 9 onwards
chris_data[61871, 8:(n_cols - 1)] <- chris_data[61871, 9:n_cols]
chris_data[61871, n_cols] <- NA

# corrected Tiao-Shi, Nanwan, Kenting, Taiwan
rows_to_fix <- grep("Tiao-Shi, Nanwan, Kenting, Taiwan", chris_data$location_name)
chris_data$location_name[rows_to_fix] <- gsub(", ", " ", chris_data$location_name[rows_to_fix])


# renaming locations in the RedSea
library(dplyr)
chris_data <- chris_data %>%
  mutate(location_name = recode(location_name,
                                "Abu Qalawa" = "Abu Qalawa, Jeddah, Red Sea",
                                "Al-Ahyaa" = "Al-Ahyaa, Egypt, Red Sea",
                                "Beer Odeeb, Northern Gulf of Suez, Red Sea" = "Beer Odeeb, Northern Gulf of Suez, Red Sea",  # unchanged
                                "Coast of Jeddah, Saudi Arabia, Central Red Sea" = "Coast of Jeddah, Saudi Arabia, Red Sea",
                                "Eilat, Israel" = "Eilat, Israel, Red Sea",
                                "El-Fanadir" = "El-Fanadir, Egypt, Red Sea",
                                "Gulf of Suez" = "Gulf of Suez, Red Sea",
                                "Na'ama Bay, Egypt" = "Na'ama Bay, Egypt, Red Sea",
                                "Yanbu" = "Yanbu, Saudi Arabia, Red Sea"
  ))


###

# list of all location in alphabetical order
list_all_location <- chris_data$location_name
list_all_location_sorted <- sort(unique(list_all_location))
writeLines(list_all_location_sorted, "list_all_location_sorted.txt")

# list of all species in alphabetical order
list_all_species <- chris_data$specie_name
list_all_species_sorted <- sort(unique(list_all_species))
writeLines(list_all_species_sorted, "list_all_species_sorted.txt")

###

# data of only red sea
chris_data_red_sea <- subset(chris_data, grepl("Red Sea", location_name, ignore.case = TRUE))
write.csv(chris_data_red_sea , "chris_data_red_sea.csv", row.names = FALSE)

# list of all red sea location in alphabetical order
list_location_red_sea <- chris_data_red_sea$location_name
list_location_sorted_red_sea <- sort(unique(list_location_red_sea))
writeLines(list_location_sorted_red_sea, "list_location_sorted_red_sea.txt")

# list of all red sea species in alphabetical order
list_species_red_sea <- chris_data_red_sea$specie_name
list_species_sorted_red_sea <- sort(unique(list_species_red_sea))
writeLines(list_species_sorted_red_sea, "list_species_sorted_red_sea.txt")

