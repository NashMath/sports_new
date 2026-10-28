###############################################################################
#################Aggregated Analysis for Paper#################################
###############################################################################
###################Insert datasets#############################################
library(readxl)
kyp<-read_xlsx("C:/Users/user/Desktop/sports_new/kypello.xlsx")
ira<-read_xlsx("C:/Users/user/Desktop/sports_new/irakleio.xlsx")

#cleaning kyp 

######## drop unwanted columns########
names(kyp)
kyp = kyp[,2:12]

######## rename variables ###########

colnames(kyp)[1] = "gender"
colnames(kyp)[2] = "age"
colnames(kyp)[3] = "transport"
colnames(kyp)[4] = "veh_passengers"
colnames(kyp)[5] = "transport_ath"
colnames(kyp)[6] = "zip_code"
colnames(kyp)[7] = "reason"
colnames(kyp)[8] = "transport_change"
colnames(kyp)[9] = "env_cons"
colnames(kyp)[10] = "soc_cons"
colnames(kyp)[11] = "interests"
names(kyp)
head(kyp)



table(kyp$transport)

# First, convert to character to safely edit labels
kyp$transport <- as.character(kyp$transport)

# Replace similar values
kyp$transport[kyp$transport %in% c("Καραβι", "Καράβι", "Πλοίο")] <- "Πλοίο"
kyp$transport[kyp$transport %in% c("Αεροπλανο", "Αεροπλάνο")] <- "Αεροπλάνο"
kyp$transport[kyp$transport %in% c("Μηχανή", "μηχανάκι")] <- "Μηχανή/μηχανάκι"  # if you want to combine

# Define the levels explicitly
kyp$transport <- factor(kyp$transport,
                         levels = c("Αυτοκίνητο", "Λεωφορείο", "Μηχανή/μηχανάκι", "Μετρό", "Πεζή", "Ποδήλατο", "Πλοίο", "Αεροπλάνο"))

# Now table will work correctly
table(kyp$transport)



table(kyp$transport_ath)

# Replace similar values
kyp$transport_ath[kyp$transport_ath %in% c("Αμαξι", "Αμάξι")] <- "Αυτοκίνητο"
kyp$transport_ath[kyp$transport_ath %in% c("Κτελ", "Λεωφορείο")] <- "Λεωφορείο"
kyp$transport_ath[kyp$transport_ath %in% c("Λαρισα", "Δεν ήρθα από Κρήτη")] <- "ΝΑ"  # if you want to combine


# Subset of observations that answered both questions
answered_both <- kyp[!is.na(kyp$transport) & !is.na(kyp$transport_ath), ]

# View how many answered both
nrow(answered_both)

# get the row indices:
which(!is.na(kyp$transport) & !is.na(kyp$transport_ath))

#dataset
View(kyp[!is.na(kyp$transport) & !is.na(kyp$transport_ath), ])

#who answered the same to both
# Observations where both variables are equal (and not NA)
same_value <- kyp[!is.na(kyp$transport) & !is.na(kyp$transport_ath) & kyp$transport == kyp$transport_ath, ]

# View the relevant columns
same_value[, c("transport", "transport_ath")]

# replace the ones that don't make sense with 99
# Define the target values
target_modes <- c("Αεροπλάνο", "Μετρό", "Πλοίο")

# Find the rows that meet all conditions
rows_to_update <- !is.na(kyp$transport) &
  !is.na(kyp$transport_ath) &
  kyp$transport == kyp$transport_ath &
  kyp$transport %in% target_modes

# Replace transport with "99" for those rows
# Convert to character
kyp$transport <- as.character(kyp$transport)

# Replace values
kyp$transport[rows_to_update] <- "99"

# (Optional) Convert back to factor if needed
kyp$transport <- as.factor(kyp$transport)


# create a second zip_code variable with the possible starting points depending on the transportation used
# First make sure transport and transport_ath are character
kyp$transport <- as.character(kyp$transport)
kyp$transport_ath <- as.character(kyp$transport_ath)

# Create zip_code2 initialized with NA
kyp$zip_code2 <- NA

# Fill values based on your conditions
kyp$zip_code2[
  kyp$transport_ath == "Αεροπλάνο"] <- 19019

kyp$zip_code2[
  kyp$transport_ath == "Πλοίο"] <- 18648

kyp$zip_code2[
  kyp$transport_ath == "Μετρό"] <- 10431

kyp$zip_code2[
  kyp$transport_ath == "Αυτοκίνητο"] <- 10431

kyp$zip_code2[
  kyp$transport_ath == "Λεωφορείο"] <- 10442


kyp$zip_code2<-as.numeric(kyp$zip_code2)


# Define the levels explicitly
kyp$transport <- factor(kyp$transport,
                         levels = c("Αυτοκίνητο", "Λεωφορείο", "Μηχανή/μηχανάκι", "Μετρό", "Πεζή", "Ποδήλατο", "Πλοίο", "Αεροπλάνο"))

# Now table will work correctly
table(kyp$transport)
table(kyp$veh_passengers, useNA = "ifany")
kyp$veh_passengers <- as.numeric(kyp$veh_passengers)
table(kyp$veh_passengers)
kyp$veh_passengers[kyp$veh_passengers > 5] <- NA
table(kyp$veh_passengers)

# Replace specific values with desired replacements
# kyp$veh_passengers[kyp$veh_passengers %in% c("Δυο", "2.0")] <- "2"
# kyp$veh_passengers[kyp$veh_passengers %in% c("4-5", "5.0")] <- "5"
# kyp$veh_passengers[kyp$veh_passengers %in% c("1-4", "1.0", "0.0")] <- "1"
# kyp$veh_passengers[kyp$veh_passengers %in% c("3.0")] <- "3"
# kyp$veh_passengers[kyp$veh_passengers %in% c("4.0")] <- "4"
# kyp$veh_passengers[kyp$veh_passengers == "Ioz 5037"] <- NA
# kyp$veh_passengers[kyp$veh_passengers %in% c("1-4", "1.0", "0.0")] <- "1"

# If needed, convert the column to numeric, replacing non-numeric strings with NA
#kyp$veh_passengers <- as.numeric(kyp$veh_passengers)
# Verify the updated table
table(kyp$veh_passengers, useNA = "ifany")



##### dummies for environmental actions###########
library(fastDummies)
library(dplyr)
library(tidyr)
library(stringr)
library(dplyr)
library(purrr)



table(kyp$interests)

# Greek-to-English mapping
translation <- c(
  "Υποστήριξη κοινωνικά ευπαθών ομάδων" = "volunteering",
  "Ανακύκλωση" = "recycle",
  "Δενδροφύτευση" = "trees",
  "Εκπαιδευτικά προγράμματα" = "trainings",
  "Άλλο (παρακαλώ προσδιορίστε)" = "Other"
)
# Ensure interests column is character
kyp$interests <- as.character(kyp$interests)

# Create binary columns for each keyword in translation
for (key in names(translation)) {
  new_col_name <- translation[[key]]  # Get the English column name
  kyp[[new_col_name]] <- as.integer(grepl(key, kyp$interests, fixed = TRUE))
}
# View the first rows
head(kyp)
table(kyp$volunteering)
table(kyp$recycle)
table(kyp$trees)
table(kyp$trainings)
table(kyp$recycle)
table(kyp$Other)

table(kyp$interests)


##### dummies for reason ###########



table(kyp$reason, useNA = "ifany")

# Greek-to-English mapping
translation <- c(
  "Άνεση" = "Convenience",
  "Ασφάλεια" = "Safety",
  "Οικολογική συνείδηση" = "eco_cons",
  "Αξιοπιστία" = "Reliability",
  "Εξοικονόμηση χρόνου" = "Time",
  "Κόστος" = "Money",
  "Ευκολία" = "Easier"
)

# Ensure interests column is character
kyp$reason <- as.character(kyp$reason)

# Create binary columns for each keyword in translation
for (key in names(translation)) {
  new_col_name <- translation[[key]]  # Get the English column name
  kyp[[new_col_name]] <- as.integer(grepl(key, kyp$reason, fixed = TRUE))
}

table(kyp$reason, useNA = "ifany")
# View the first rows
head(kyp)
table(kyp$Convenience)
table(kyp$Safety)
table(kyp$eco_cons)
table(kyp$Reliability)
table(kyp$Time)
table(kyp$Money)
table(kyp$Easier)

############ Use the dataset with greek zip codes ################
library(readr)
library(tidygeocoder)
library(geosphere)
library(dplyr)

kyp$zip_code
kyp$zip_code[kyp$zip_code %in% c("Νικαια 18450")] <- 18450
kyp$zip_code[kyp$zip_code %in% c("ΙΩΑΝΝΙΝΑ  45221")] <- 45221
kyp$zip_code<-as.numeric(kyp$zip_code)

#TK of field
kyp$field <- 15123 #oaka
kyp$field<-as.character(kyp$field)

kyp$zip_code2<-as.character(kyp$zip_code2)
library(dplyr)
library(readr)
library(geosphere)


for (i in seq_len(nrow(kyp))) {
  if (is.na(kyp$zip_code2[i]) && !is.na(kyp$zip_code[i])) {
    kyp$zip_code2[i] <- kyp$zip_code[i]
  }
}


#########################################################
# Step 1: Load Greek ZIP code dataset
greek_zip_data <- read_csv("https://raw.githubusercontent.com/MentatInnovations/grpostcodes/master/data/postcode_lat_long_output_file.csv") %>%
  rename(zip_code = tk, lat = lat, lon = lon) %>%
  mutate(zip_code = as.character(zip_code))

# First: Get coordinates from zip_code → zip_code2
kyp <- kyp %>%
  mutate(zip_code = as.character(zip_code),
         zip_code2 = as.character(zip_code2)) %>%
  left_join(greek_zip_data, by = c("zip_code" = "zip_code")) %>%
  rename(home_lat = lat, home_lon = lon) %>%
  left_join(
    greek_zip_data %>% rename(zip_code2 = zip_code, inter_lat = lat, inter_lon = lon),
    by = "zip_code2"
  ) %>%
  mutate(
    distance_km1 = distHaversine(cbind(home_lon, home_lat), cbind(inter_lon, inter_lat)) / 1000
  )

# Second: Get coordinates from zip_code2 → field
kyp <- kyp %>%
  mutate(field = as.character(field)) %>%
  left_join(
    greek_zip_data %>% rename(field = zip_code, stadium_lat = lat, stadium_lon = lon),
    by = "field"
  ) %>%
  mutate(
    distance_km2 = distHaversine(cbind(inter_lon, inter_lat), cbind(stadium_lon, stadium_lat)) / 1000
  )

kyp <- kyp %>%
  mutate(total_distance_km = distance_km1 + distance_km2)


#################################
table(greek_zip_data$territory)
greek_zip_data[greek_zip_data$territory == "Ανατολικά Προάστια",]
#replace
greek_zip_data[greek_zip_data$territory == "Ανατολικά Προάστια",]$lat=38.08
greek_zip_data[greek_zip_data$territory == "Ανατολικά Προάστια",]$lon=23.9
greek_zip_data[greek_zip_data$territory == "Ανατολικά Προάστια",]
greek_zip_data[greek_zip_data$territory == "ΜΑΡΟΥΣΙ ΝΕΑ ΦΙΛΟΘΕΗ",]
greek_zip_data[greek_zip_data$territory == "ΟΛΥΜΠΙΑΚΟ ΧΩΡΙΟ",]
greek_zip_data[greek_zip_data$territory == "ΚΟΛΩΝΟΣ",]
greek_zip_data[greek_zip_data$territory == "ΚΟΛΩΝΟΣ",]$lat=38.0
greek_zip_data[greek_zip_data$territory == "ΚΟΛΩΝΟΣ",]$lon=23.7
greek_zip_data[greek_zip_data$territory == "ΚΟΛΩΝΟΣ",]

greek_zip_data[greek_zip_data$territory == c("ΠΕΙΡΑΙΑΣ ΤΑΜΠΟΥΡΙΑ","ΠΕΙΡΑΙΑΣ ΑΠΟΛΛΩΝΟΣ"),]$lat=37.9
greek_zip_data[greek_zip_data$territory == c("ΠΕΙΡΑΙΑΣ ΤΑΜΠΟΥΡΙΑ","ΠΕΙΡΑΙΑΣ ΑΠΟΛΛΩΝΟΣ"),]$lon=23.6
greek_zip_data[greek_zip_data$territory == c("ΠΕΙΡΑΙΑΣ ΤΑΜΠΟΥΡΙΑ","ΠΕΙΡΑΙΑΣ ΑΠΟΛΛΩΝΟΣ"),]



#############fix distances##############
table(kyp$distance_km1)
table(kyp$distance_km2)

table(kyp$zip_code2)
#emission factors


kyp$territory.x[which(kyp$distance_km2>56)]

kyp$distance_km1[which(kyp$distance_km2>56)]

#filter wrong values

kyp$zip_code_conc <- trunc(as.numeric(kyp$zip_code)*10^(-2))
kyp$zip_code
library(dplyr)
kyp<- subset(kyp, !(kyp$distance_km2>56 & kyp$zip_code_conc<199 & kyp$zip_code_conc>100))


#which(kyp$distance_km2>56 & kyp$distance_km1>56)

#idx <- which(kyp$distance_km2 > 56 & kyp$distance_km1 > 56)
#kyp$distance_km2[idx] <- 0

#weird_people <- data.frame(
# id = idx,
#d1 = kyp$distance_km1[idx],
#d2 = kyp$distance_km2[idx],
#territory = kyp$territory.x[idx],
#zip_code = kyp$zip_code[idx],
#zip_code2 = kyp$zip_code2[idx],
#transport = kyp$transport[idx],
# transport_ath = kyp$transport_ath[idx]
#)

#weird_people
#kyp$distance_km[kyp$distance_km > 115] <- NA

plot(kyp$distance_km1, kyp$distance_km2)
abline(h = 56, v = 56, col = "red")
library(dplyr)



# Step 4: Define emission factors
emission_factors <- tibble(
  transport = c("Αυτοκίνητο", "Λεωφορείο", "Μηχανή/μηχανάκι", 
                "Πεζή", "Ποδήλατο", "Αεροπλάνο", "Πλοίο", "Μετρό"),
  emission_factor = c(139, 75, 113, 0, 0, 201, 112, 64)  # g CO2-e per km
)

# Step 5: Merge emission factors based on transport mode
kyp <- kyp %>%
  left_join(emission_factors, by = "transport")

# Step 6: Calculate Eco-Footprint (round trip included)
kyp <- kyp %>%
  mutate(co2_emissions1 = distance_km1 * emission_factor * 2)  # Multiply by 2 for round-trip


# Step 6: Calculate Eco-Footprint (round trip included)
kyp <- kyp %>%
  mutate(co2_emissions2 = distance_km2 * emission_factor * 2) 

# Step 7: Check the dataset
head(kyp)

table(kyp$co2_emissions1)
table(kyp$co2_emissions2)
kyp$co2_emissions = kyp$co2_emissions1 + kyp$co2_emissions2
#kyp$co2_emissions = kyp$co2_emissions #+ (700*2*112)+(700*3*201)

######calculate the total carbon footprint for this match####
# Sum total carbon emissions for this match
total_carbon_match <- sum(kyp$co2_emissions, na.rm = TRUE)

# Print result
print(total_carbon_match)
# total carbon game in tons
#Convert this to tons
total_game_footprint_tons <- total_carbon_match + (700*2*112)+(700*3*201)
total_game_footprint_tons <- total_game_footprint_tons/ 1000000
print(total_game_footprint_tons)

#####another approach
avg_sp<-total_game_footprint_tons/209
avg_sp
#avg_sp2<-total_game_footprint_tons/(209+300)
#avg_sp2

avg_sp_proj<-avg_sp*55000
avg_sp_proj

#find means for each transport

table(kyp$transport_ath)

avg_car<-mean(kyp$co2_emissions[kyp$transport == "Αυτοκίνητο"], na.rm=TRUE)
avg_bus<-mean(kyp$co2_emissions[kyp$transport == "Λεωφορείο"], na.rm=TRUE)
avg_metro<-mean(kyp$co2_emissions[kyp$transport == "Μετρό"], na.rm=TRUE)
avg_moto<-mean(kyp$co2_emissions[kyp$transport == "Μηχανή/μηχανάκι"], na.rm=TRUE)
avg_feet<-mean(kyp$co2_emissions[kyp$transport == "Πεζή"], na.rm=TRUE)
avg_air<-mean(kyp$co2_emissions[kyp$transport_ath == "Αεροπλάνο"], na.rm=TRUE)
avg_ship<-mean(kyp$co2_emissions[kyp$transport_ath == "Πλοίο"], na.rm=TRUE)

avg_proj<-avg_car*0.35+avg_bus*0.1+avg_metro*0.3+avg_moto*0.1+avg_air*0.05+avg_ship*0.1
avg_proj_tons<- avg_proj/1000000
total_proj<- avg_proj*55000
total_proj
total_proj_tons<-total_proj/1000000
total_proj_tons

dim(kyp)

########
#average after projection





# Compute average carbon footprint per spectator for this match
dim(kyp)

avg_carbon_per_spectator<-mean(kyp$co2_emissions , na.rm = TRUE)
avg_carbon_per_spectator
avg_carbon_per_spectator_tons <- avg_carbon_per_spectator / 1000000
avg_carbon_per_spectator_tons
########################2nd approach###########################
avg_carbon_per_spectator <- kyp$co2_emissions/(209+1000)
avg_carbon_per_spectator_tons <- avg_carbon_per_spectator / 1000000
avg_carbon_per_spectator_tons
avg <- mean(avg_carbon_per_spectator_tons , na.rm = TRUE)
avg
# Project total carbon footprint for all home games
#total_seasonal_footprint <- avg_carbon_per_spectator * 3000 * 16

# Print result
#print(total_seasonal_footprint)

#Convert this to tons
#total_seasonal_footprint_tons <- total_seasonal_footprint / 1000000
#print(total_seasonal_footprint_tons)

#avg_match_footprint_tons <- total_seasonal_footprint_tons / 16
#print(avg_match_footprint_tons)

#cat("Total estimated carbon footprint for the season:", total_seasonal_footprint_tons, "t CO₂-e\n")
#cat("Average carbon footprint per match:", avg_match_footprint_tons, "t CO₂-e\n")



###############PPEN KYP Dataset with qualitative charasteristics of the game#####
#kyp dataset has 186 observations and 30 variables:


####################append the dataset##################

library(readxl)

# read the whole sheet
quant<- read_excel("C:/Users/user/Desktop/quan.xlsx")

# column names (titles)
titles <- names(quant)

# values from row 4
values <- quant[5, ]
vals_named <- as.list(values)
names(vals_named) <- titles

kyp <- cbind(
  kyp,
  as.data.frame(
    lapply(vals_named, function(x) rep(x, nrow(kyp)))
  )
)








################IRAKLEIO GAMEEEEEEE#######################

#cleaning ira
######## drop unwanted columns########
names(ira)
ira = ira[,2:11]

######## rename variables ###########

colnames(ira)[1] = "gender"
colnames(ira)[2] = "age"
colnames(ira)[3] = "transport"
colnames(ira)[4] = "veh_passengers"
colnames(ira)[5] = "zip_code"
colnames(ira)[6] = "reason"
colnames(ira)[7] = "transport_change"
colnames(ira)[8] = "env_cons"
colnames(ira)[9] = "soc_cons"
colnames(ira)[10] = "interests"
names(ira)
head(ira)

######## rename values and fix type #############
table(ira$age)
ira$age <- factor(ira$age,order=TRUE, levels = c("<18", "18-24", "25-34","35-44", "45-54", "55-64"), #, ">64"
                   labels = c("<18", "18-24", "25-34","35-44", "45-54", "55-64")) #, ">64"
table(ira$gender)
ira$gender <- factor (ira$gender, order=FALSE, labels =c("Άνδρας","Γυναίκα"))
table(ira$transport)

# Replace the last two values with "Τραμ"
ira$transport[ira$transport %in% c("Ταξι")] <- "Αυτοκίνητο"
ira$transport <- factor(ira$transport, order=FALSE,
                         labels = c("Αυτοκίνητο", "Λεωφορείο", "Μηχανή/μηχανάκι", "Πεζή", "Ποδήλατο" ))


table(ira$veh_passengers, useNA = "ifany")
ira$veh_passengers <- as.numeric(ira$veh_passengers)
table(ira$veh_passengers)
ira$veh_passengers[ira$veh_passengers > 5] <- NA
table(ira$veh_passengers)

# Replace specific values with desired replacements
# ira$veh_passengers[ira$veh_passengers %in% c("Δυο", "2.0")] <- "2"
# ira$veh_passengers[ira$veh_passengers %in% c("4-5", "5.0")] <- "5"
# ira$veh_passengers[ira$veh_passengers %in% c("1-4", "1.0", "0.0")] <- "1"
# ira$veh_passengers[ira$veh_passengers %in% c("3.0")] <- "3"
# ira$veh_passengers[ira$veh_passengers %in% c("4.0")] <- "4"
# ira$veh_passengers[ira$veh_passengers == "Ioz 5037"] <- NA
# ira$veh_passengers[ira$veh_passengers %in% c("1-4", "1.0", "0.0")] <- "1"

# If needed, convert the column to numeric, replacing non-numeric strings with NA
#ira$veh_passengers <- as.numeric(ira$veh_passengers)
# Verify the updated table
table(ira$veh_passengers, useNA = "ifany")

########## zip_code ################
table(ira$zip_code, useNA = "ifany")
ira$zip_code[ira$zip_code == "70014 αναληψη"] <- "70014"
ira$zip_code[ira$zip_code == "Γαζί, 71410"] <-"71410"
ira$zip_code[ira$zip_code %in% c("Ηρακλειο 71304", "Ηρακλειου", "ηράκλειο", "Ηράκλειο")] <- "71304"
ira$zip_code[ira$zip_code %in% c("Χανιά" , "Χανιά 73100")] <- "73100"
# ira$zip_code[ira$zip_code %in% c("Παγκράτι")] <- "11633"
# ira$zip_code[ira$zip_code %in% c("Παλαιο φαληρο 17563")] <- "17563"
# ira$zip_code[ira$zip_code %in% c("Άλιμο")] <- "17456"
# ira$zip_code[ira$zip_code %in% c("νέα μάκρη")] <- "19005"
# ira$zip_code[ira$zip_code %in% c("Νέο κοσμο")] <- "11744"
ira$zip_code <- as.numeric(ira$zip_code)
table(ira$zip_code, useNA = "ifany")

##### dummies for environmental actions###########
library(fastDummies)
library(dplyr)
library(tidyr)
library(stringr)
library(dplyr)
library(purrr)



table(ira$interests)

# Greek-to-English mapping
translation <- c(
  "Υποστήριξη κοινωνικά ευπαθών ομάδων" = "volunteering",
  "Ανακύκλωση" = "recycle",
  "Δενδροφύτευση" = "trees",
  "Εκπαιδευτικά προγράμματα" = "trainings",
  "Άλλο (παρακαλώ προσδιορίστε)" = "Other"
)
# Ensure interests column is character
ira$interests <- as.character(ira$interests)

# Create binary columns for each keyword in translation
for (key in names(translation)) {
  new_col_name <- translation[[key]]  # Get the English column name
  ira[[new_col_name]] <- as.integer(grepl(key, ira$interests, fixed = TRUE))
}
# View the first rows
head(ira)
table(ira$volunteering)
table(ira$recycle)
table(ira$trees)
table(ira$trainings)
table(ira$recycle)
table(ira$Other)

table(ira$interests)


##### dummies for reason ###########



table(ira$reason, useNA = "ifany")

# Greek-to-English mapping
translation <- c(
  "Άνεση" = "Convenience",
  "Ασφάλεια" = "Safety",
  "Οικολογική συνείδηση" = "eco_cons",
  "Αξιοπιστία" = "Reliability",
  "Εξοικονόμηση χρόνου" = "Time",
  "Κόστος" = "Money",
  "Ευκολία" = "Easier"
)

# Ensure interests column is character
ira$reason <- as.character(ira$reason)

# Create binary columns for each keyword in translation
for (key in names(translation)) {
  new_col_name <- translation[[key]]  # Get the English column name
  ira[[new_col_name]] <- as.integer(grepl(key, ira$reason, fixed = TRUE))
}

table(ira$reason, useNA = "ifany")
# View the first rows
head(ira)
table(ira$Convenience)
table(ira$Safety)
table(ira$eco_cons)
table(ira$Reliability)
table(ira$Time)
table(ira$Money)
table(ira$Easier)


############ Use the dataset with greek zip codes ################
library(readr)
library(tidygeocoder)
library(geosphere)
library(dplyr)

ira$zip_code
ira$zip_code[ira$zip_code %in% c(8, 85200)] <- NA

#TK of 
ira$field <- "71303"
#ira$field_pagkrhtio <- "71303"

# Step 1: Load Greek ZIP code dataset
greek_zip_data <- read_csv("https://raw.githubusercontent.com/MentatInnovations/grpostcodes/master/data/postcode_lat_long_output_file.csv")

# Rename columns for easier use
greek_zip_data <- greek_zip_data %>%
  rename(zip_code =tk, lat = lat, lon = lon)

# Convert zip codes to character (important for merging)
greek_zip_data <- greek_zip_data %>%
  mutate(zip_code = as.character(zip_code))

greek_zip_data$zip_code

# Step 2: Merge coordinates into `ira` for both viewers' home and stadium
ira <- ira %>%
  mutate(zip_code = as.character(zip_code), field = as.character(field)) %>% # Ensure both are character
  left_join(greek_zip_data, by = c("zip_code" = "zip_code")) %>%
  rename(home_lat = lat, home_lon = lon) %>%  # Rename for clarity
  left_join(greek_zip_data, by = c("field" = "zip_code")) %>%
  rename(stadium_lat = lat, stadium_lon = lon)  # Rename for clarity

# Step 3: Calculate distance (in km) between home and stadium
ira <- ira %>%
  mutate(distance_km = distHaversine(cbind(home_lon, home_lat), 
                                     cbind(stadium_lon, stadium_lat)) / 1000)  # Convert meters to km
#############fix distances##############
table(ira$distance_km)
ira$territory.x[which(ira$distance_km>115)]
which(ira$distance_km>115)

ira$distance_km[ira$distance_km > 115] <- NA


library(dplyr)



# Step 4: Define emission factors
emission_factors <- tibble(
  transport = c("Αυτοκίνητο", "Λεωφορείο", "Μηχανή/μηχανάκι", 
                "Πεζή", "Ποδήλατο"),
  emission_factor = c(139, 75, 113, 0, 0)  # g CO2-e per km
)

# Step 5: Merge emission factors based on transport mode
ira <- ira %>%
  left_join(emission_factors, by = "transport")

# Step 6: Calculate Eco-Footprint (round trip included)
ira <- ira %>%
  mutate(co2_emissions = distance_km * emission_factor * 2)  # Multiply by 2 for round-trip

# Step 7: Check the dataset
head(ira)

ira$co2_emissions


######calculate the total carbon footprint for this match####
# Sum total carbon emissions for this match
total_carbon_match <- sum(ira$co2_emissions, na.rm = TRUE)

# Print result
print(total_carbon_match)
# total carbon game in tons
#Convert this to tons
total_game_footprint_tons <- total_carbon_match / 1000000
print(total_game_footprint_tons)




#####extrapolate###########
# Compute average carbon footprint per spectator for this match
avg_carbon_per_spectator <- mean(ira$co2_emissions, na.rm = TRUE)
avg_carbon_per_spectator_tons <- avg_carbon_per_spectator / 1000000
avg_carbon_per_spectator_tons


0.007973179*18930


library(ggplot2)
names(kyp)
names(ira)

######Irakleio add quant###############

# column names (titles)
titles <- names(quant)

# values from row 4
values <- quant[4, ]

vals_named <- as.list(values)
names(vals_named) <- titles

ira <- cbind(
  ira,
  as.data.frame(
    lapply(vals_named, function(x) rep(x, nrow(ira)))
  )
)

names(kyp)
names(ira)

#####################merging procedure
names(kyp)[duplicated(names(kyp))]
common_names <- intersect(names(kyp), names(ira))

kyp_clean <- kyp[, common_names]
ira_clean <- ira[, common_names]
kyp_clean <- kyp_clean[, common_names]
ira_clean <- ira_clean[, common_names]
identical(names(kyp_clean), names(ira_clean))
# should be TRUE

merged_data <- rbind(ira_clean, kyp_clean)
ira_clean$dataset <- "ira"
kyp_clean$dataset <- "kyp"

merged_data <- rbind(ira_clean, kyp_clean)

####################prdictive models#################################

set.seed(123)   # reproducibility
n <- nrow(merged_data)
train_id <- sample(seq_len(n), size = 0.8 * n)

train <- merged_data[train_id, ]
test  <- merged_data[-train_id, ]
test$co2_emissions_true <- test$co2_emissions
test$co2_emissions <- NA
predictors <- c(
  "age",
  "gender",
  "transport",
  "env_cons",
"soc_cons",
"distance_km",
"volunteering",
"recycle", "trees", "trainings", "Other",
 "Convenience", "Safety", "eco_cons", "Reliability",
"Time", "Money", "Easier","home_lat", "home_lon",
 "stadium_lat" , "stadium_lon",
  "emission_factor",
  "Weekend.Weekday",
  "Weather", "Precipitation", "Availability.of.official.Parking"
)
predictors <- predictors[predictors %in% names(train)]

lm_formula <- as.formula(
  paste("co2_emissions ~", paste(predictors, collapse = " + "))
)

lm_fit <- lm(lm_formula, data = train)
summary(lm_fit)


