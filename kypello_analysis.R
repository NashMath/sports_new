#################DATA ANALYSIS PANIONIOS DATA###############################

###########Insert dataset############
library(readxl)
pani<-read_xlsx("C:/Users/user/Desktop/sports_new/kypello.xlsx")

######## drop unwanted columns########
names(pani)
pani = pani[,2:12]

######## rename variables ###########

colnames(pani)[1] = "gender"
colnames(pani)[2] = "age"
colnames(pani)[3] = "transport"
colnames(pani)[4] = "veh_passengers"
colnames(pani)[5] = "transport_ath"
colnames(pani)[6] = "zip_code"
colnames(pani)[7] = "reason"
colnames(pani)[8] = "transport_change"
colnames(pani)[9] = "env_cons"
colnames(pani)[10] = "soc_cons"
colnames(pani)[11] = "interests"
names(pani)
head(pani)



table(pani$transport)

# First, convert to character to safely edit labels
pani$transport <- as.character(pani$transport)

# Replace similar values
pani$transport[pani$transport %in% c("Καραβι", "Καράβι", "Πλοίο")] <- "Πλοίο"
pani$transport[pani$transport %in% c("Αεροπλανο", "Αεροπλάνο")] <- "Αεροπλάνο"
pani$transport[pani$transport %in% c("Μηχανή", "μηχανάκι")] <- "Μηχανή/μηχανάκι"  # if you want to combine

# Define the levels explicitly
pani$transport <- factor(pani$transport,
                         levels = c("Αυτοκίνητο", "Λεωφορείο", "Μηχανή/μηχανάκι", "Μετρό", "Πεζή", "Ποδήλατο", "Πλοίο", "Αεροπλάνο"))

# Now table will work correctly
table(pani$transport)



table(pani$transport_ath)

# Replace similar values
pani$transport_ath[pani$transport_ath %in% c("Αμαξι", "Αμάξι")] <- "Αυτοκίνητο"
pani$transport_ath[pani$transport_ath %in% c("Κτελ", "Λεωφορείο")] <- "Λεωφορείο"
pani$transport_ath[pani$transport_ath %in% c("Λαρισα", "Δεν ήρθα από Κρήτη")] <- "ΝΑ"  # if you want to combine


# Subset of observations that answered both questions
answered_both <- pani[!is.na(pani$transport) & !is.na(pani$transport_ath), ]

# View how many answered both
nrow(answered_both)

# get the row indices:
which(!is.na(pani$transport) & !is.na(pani$transport_ath))

#dataset
View(pani[!is.na(pani$transport) & !is.na(pani$transport_ath), ])

#who answered the same to both
# Observations where both variables are equal (and not NA)
same_value <- pani[!is.na(pani$transport) & !is.na(pani$transport_ath) & pani$transport == pani$transport_ath, ]

# View the relevant columns
same_value[, c("transport", "transport_ath")]

# replace the ones that don't make sense with 99
# Define the target values
target_modes <- c("Αεροπλάνο", "Μετρό", "Πλοίο")

# Find the rows that meet all conditions
rows_to_update <- !is.na(pani$transport) &
  !is.na(pani$transport_ath) &
  pani$transport == pani$transport_ath &
  pani$transport %in% target_modes

# Replace transport with "99" for those rows
# Convert to character
pani$transport <- as.character(pani$transport)

# Replace values
pani$transport[rows_to_update] <- "99"

# (Optional) Convert back to factor if needed
pani$transport <- as.factor(pani$transport)


# create a second zip_code variable with the possible starting points depending on the transportation used
# First make sure transport and transport_ath are character
pani$transport <- as.character(pani$transport)
pani$transport_ath <- as.character(pani$transport_ath)

# Create zip_code2 initialized with NA
pani$zip_code2 <- NA

# Fill values based on your conditions
pani$zip_code2[
  pani$transport_ath == "Αεροπλάνο"] <- 19019

pani$zip_code2[
  pani$transport_ath == "Πλοίο"] <- 18648

pani$zip_code2[
  pani$transport_ath == "Μετρό"] <- 10431

pani$zip_code2[
  pani$transport_ath == "Αυτοκίνητο"] <- 10431

pani$zip_code2[
  pani$transport_ath == "Λεωφορείο"] <- 10442


pani$zip_code2<-as.numeric(pani$zip_code2)

############ Use the dataset with greek zip codes ################
library(readr)
library(tidygeocoder)
library(geosphere)
library(dplyr)

pani$zip_code
pani$zip_code[pani$zip_code %in% c("Νικαια 18450")] <- 18450
pani$zip_code[pani$zip_code %in% c("ΙΩΑΝΝΙΝΑ  45221")] <- 45221
pani$zip_code<-as.numeric(pani$zip_code)

#TK of field
pani$field <- 15123 #oaka
pani$field<-as.character(pani$field)

pani$zip_code2<-as.character(pani$zip_code2)
library(dplyr)
library(readr)
library(geosphere)


for (i in seq_len(nrow(pani))) {
  if (is.na(pani$zip_code2[i]) && !is.na(pani$zip_code[i])) {
    pani$zip_code2[i] <- pani$zip_code[i]
  }
}


#pani$transport_ath[which(is.na(pani$zip_code2) & !is.na(pani$zip_code))]
#pani$zip_code[which(is.na(pani$zip_code2) & !is.na(pani$zip_code))]


#condition <- is.na(pani$zip_code2) & !is.na(pani$zip_code)
#pani$zip_code2[condition] <- pani$zip_code[condition]




#########################################################
# Step 1: Load Greek ZIP code dataset
greek_zip_data <- read_csv("https://raw.githubusercontent.com/MentatInnovations/grpostcodes/master/data/postcode_lat_long_output_file.csv") %>%
  rename(zip_code = tk, lat = lat, lon = lon) %>%
  mutate(zip_code = as.character(zip_code))

# First: Get coordinates from zip_code → zip_code2
pani <- pani %>%
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
pani <- pani %>%
  mutate(field = as.character(field)) %>%
  left_join(
    greek_zip_data %>% rename(field = zip_code, stadium_lat = lat, stadium_lon = lon),
    by = "field"
  ) %>%
  mutate(
    distance_km2 = distHaversine(cbind(inter_lon, inter_lat), cbind(stadium_lon, stadium_lat)) / 1000
  )

pani <- pani %>%
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
table(pani$distance_km1)
table(pani$distance_km2)

table(pani$zip_code2)
#emission factors


pani$territory.x[which(pani$distance_km2>56)]

pani$distance_km1[which(pani$distance_km2>56)]

#filter wrong values

pani$zip_code_conc <- trunc(as.numeric(pani$zip_code)*10^(-2))
pani$zip_code
library(dplyr)
pani<- subset(pani, !(pani$distance_km2>56 & pani$zip_code_conc<199 & pani$zip_code_conc>100))


#which(pani$distance_km2>56 & pani$distance_km1>56)

#idx <- which(pani$distance_km2 > 56 & pani$distance_km1 > 56)
#pani$distance_km2[idx] <- 0

#weird_people <- data.frame(
 # id = idx,
  #d1 = pani$distance_km1[idx],
  #d2 = pani$distance_km2[idx],
  #territory = pani$territory.x[idx],
  #zip_code = pani$zip_code[idx],
  #zip_code2 = pani$zip_code2[idx],
  #transport = pani$transport[idx],
 # transport_ath = pani$transport_ath[idx]
#)

#weird_people
#pani$distance_km[pani$distance_km > 115] <- NA

plot(pani$distance_km1, pani$distance_km2)
abline(h = 56, v = 56, col = "red")
library(dplyr)



# Step 4: Define emission factors
emission_factors <- tibble(
  transport = c("Αυτοκίνητο", "Λεωφορείο", "Μηχανή/μηχανάκι", 
                "Πεζή", "Ποδήλατο", "Αεροπλάνο", "Πλοίο", "Μετρό"),
  emission_factor = c(139, 75, 113, 0, 0, 201, 112, 64)  # g CO2-e per km
)

# Step 5: Merge emission factors based on transport mode
pani <- pani %>%
  left_join(emission_factors, by = "transport")

# Step 6: Calculate Eco-Footprint (round trip included)
pani <- pani %>%
  mutate(co2_emissions1 = distance_km1 * emission_factor * 2)  # Multiply by 2 for round-trip


# Step 6: Calculate Eco-Footprint (round trip included)
pani <- pani %>%
  mutate(co2_emissions2 = distance_km2 * emission_factor * 2) 

# Step 7: Check the dataset
head(pani)

table(pani$co2_emissions1)
table(pani$co2_emissions2)
pani$co2_emissions = pani$co2_emissions1 + pani$co2_emissions2
#pani$co2_emissions = pani$co2_emissions #+ (700*2*112)+(700*3*201)

######calculate the total carbon footprint for this match####
# Sum total carbon emissions for this match
total_carbon_match <- sum(pani$co2_emissions, na.rm = TRUE)

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

table(pani$transport_ath)

avg_car<-mean(pani$co2_emissions[pani$transport == "Αυτοκίνητο"], na.rm=TRUE)
avg_bus<-mean(pani$co2_emissions[pani$transport == "Λεωφορείο"], na.rm=TRUE)
avg_metro<-mean(pani$co2_emissions[pani$transport == "Μετρό"], na.rm=TRUE)
avg_moto<-mean(pani$co2_emissions[pani$transport == "Μηχανή/μηχανάκι"], na.rm=TRUE)
avg_feet<-mean(pani$co2_emissions[pani$transport == "Πεζή"], na.rm=TRUE)
avg_air<-mean(pani$co2_emissions[pani$transport_ath == "Αεροπλάνο"], na.rm=TRUE)
avg_ship<-mean(pani$co2_emissions[pani$transport_ath == "Πλοίο"], na.rm=TRUE)

avg_proj<-avg_car*0.35+avg_bus*0.1+avg_metro*0.3+avg_moto*0.1+avg_air*0.05+avg_ship*0.1
avg_proj_tons<- avg_proj/1000000
total_proj<- avg_proj*55000
total_proj
total_proj_tons<-total_proj/1000000
total_proj_tons

dim(pani)

########
#average after projection





# Compute average carbon footprint per spectator for this match
dim(pani)

avg_carbon_per_spectator<-mean(pani$co2_emissions , na.rm = TRUE)
avg_carbon_per_spectator
avg_carbon_per_spectator_tons <- avg_carbon_per_spectator / 1000000
avg_carbon_per_spectator_tons
########################2nd approach###########################
avg_carbon_per_spectator <- pani$co2_emissions/(209+1000)
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













######## rename values and fix type #############




###########Insert dataset############
library(readxl)
pani<-read_xlsx("C:/Users/user/Desktop/sports_new/kypello.xlsx")

######## drop unwanted columns########
names(pani)
pani = pani[,2:12]

######## rename variables ###########

colnames(pani)[1] = "gender"
colnames(pani)[2] = "age"
colnames(pani)[3] = "transport"
colnames(pani)[4] = "veh_passengers"
colnames(pani)[5] = "transport_ath"
colnames(pani)[6] = "zip_code"
colnames(pani)[7] = "reason"
colnames(pani)[8] = "transport_change"
colnames(pani)[9] = "env_cons"
colnames(pani)[10] = "soc_cons"
colnames(pani)[11] = "interests"
names(pani)
head(pani)

table(pani$age)
pani$age <- factor(pani$age,order=TRUE, levels = c("<18", "18-24", "25-34","35-44", "45-54", "55-64", ">64"), #, ">64"
                   labels = c("<18", "18-24", "25-34","35-44", "45-54", "55-64",">64"))
table(pani$gender)
pani$gender <- factor (pani$gender, order=FALSE, labels =c("Άνδρας","Γυναίκα"))
table(pani$transport)

# First, convert to character to safely edit labels
pani$transport <- as.character(pani$transport)

# Replace similar values
pani$transport[pani$transport %in% c("Καραβι", "Καράβι", "Πλοίο")] <- "Πλοίο"
pani$transport[pani$transport %in% c("Αεροπλανο", "Αεροπλάνο")] <- "Αεροπλάνο"
pani$transport[pani$transport %in% c("Μηχανή", "μηχανάκι")] <- "Μηχανή/μηχανάκι"  # if you want to combine

# Define the levels explicitly
pani$transport <- factor(pani$transport,
                         levels = c("Αυτοκίνητο", "Λεωφορείο", "Μηχανή/μηχανάκι", "Μετρό", "Πεζή", "Ποδήλατο", "Πλοίο", "Αεροπλάνο"))

# Now table will work correctly
table(pani$transport)
table(pani$veh_passengers, useNA = "ifany")
pani$veh_passengers <- as.numeric(pani$veh_passengers)
table(pani$veh_passengers)
pani$veh_passengers[pani$veh_passengers > 5] <- NA
table(pani$veh_passengers)

# Replace specific values with desired replacements
# pani$veh_passengers[pani$veh_passengers %in% c("Δυο", "2.0")] <- "2"
# pani$veh_passengers[pani$veh_passengers %in% c("4-5", "5.0")] <- "5"
# pani$veh_passengers[pani$veh_passengers %in% c("1-4", "1.0", "0.0")] <- "1"
# pani$veh_passengers[pani$veh_passengers %in% c("3.0")] <- "3"
# pani$veh_passengers[pani$veh_passengers %in% c("4.0")] <- "4"
# pani$veh_passengers[pani$veh_passengers == "Ioz 5037"] <- NA
# pani$veh_passengers[pani$veh_passengers %in% c("1-4", "1.0", "0.0")] <- "1"

# If needed, convert the column to numeric, replacing non-numeric strings with NA
#pani$veh_passengers <- as.numeric(pani$veh_passengers)
# Verify the updated table
table(pani$veh_passengers, useNA = "ifany")



##### dummies for environmental actions###########
library(fastDummies)
library(dplyr)
library(tidyr)
library(stringr)
library(dplyr)
library(purrr)



table(pani$interests)

# Greek-to-English mapping
translation <- c(
  "Υποστήριξη κοινωνικά ευπαθών ομάδων" = "volunteering",
  "Ανακύκλωση" = "recycle",
  "Δενδροφύτευση" = "trees",
  "Εκπαιδευτικά προγράμματα" = "trainings",
  "Άλλο (παρακαλώ προσδιορίστε)" = "Other"
)
# Ensure interests column is character
pani$interests <- as.character(pani$interests)

# Create binary columns for each keyword in translation
for (key in names(translation)) {
  new_col_name <- translation[[key]]  # Get the English column name
  pani[[new_col_name]] <- as.integer(grepl(key, pani$interests, fixed = TRUE))
}
# View the first rows
head(pani)
table(pani$volunteering)
table(pani$recycle)
table(pani$trees)
table(pani$trainings)
table(pani$recycle)
table(pani$Other)

table(pani$interests)


##### dummies for reason ###########



table(pani$reason, useNA = "ifany")

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
pani$reason <- as.character(pani$reason)

# Create binary columns for each keyword in translation
for (key in names(translation)) {
  new_col_name <- translation[[key]]  # Get the English column name
  pani[[new_col_name]] <- as.integer(grepl(key, pani$reason, fixed = TRUE))
}

table(pani$reason, useNA = "ifany")
# View the first rows
head(pani)
table(pani$Convenience)
table(pani$Safety)
table(pani$eco_cons)
table(pani$Reliability)
table(pani$Time)
table(pani$Money)
table(pani$Easier)

##################################### EDA########################################
library(ggplot2)

#barplot
ggplot(pani %>% filter(!is.na(gender)), aes(x = gender, fill = gender)) +
  geom_bar() +
  labs(x = "Φύλο", y = "Πλήθος", title = "Κατανομή Φύλου") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(
    axis.text = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 14, face = "bold")
  )

#piechart


pani %>%
  filter(!is.na(gender)) %>%
  count(gender) %>%
  mutate(perc = round(n / sum(n) * 100, 1)) %>%  # Calculate percentages
  ggplot(aes(x = "", y = n, fill = gender)) +
  geom_bar(stat = "identity", width = 1, color = NA) +  # Remove border lines
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(perc, "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5, fontface = "bold") +  # Add percentage labels
  labs(title = "Κατανομή Φύλου", fill = "Φύλο") +  # Rename legend title
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 14, face = "bold")
  )


ggplot(pani %>% filter(!is.na(age)), aes(x = factor(age), fill = factor(age))) +
  geom_bar() +
  labs(x = "Ηλικία", y = "Πλήθος", title = "Κατανομή Ηλικιών") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(
    axis.text = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 14, face = "bold")
  )

###############################

library(ggplot2)
library(dplyr)
library(RColorBrewer)
# Extract colors from the "PuBuGn" palette, skipping the first one
palette_colors <- brewer.pal(n = 9, "PuBuGn")[-1]  # Remove the first color

pani %>%
  filter(!is.na(age)) %>%
  count(age) %>%
  mutate(perc = (n / sum(n)) * 100) %>%  # Convert counts to percentages
  ggplot(aes(x = factor(age), y = perc, fill = factor(age))) +
  geom_bar(stat = "identity") +  # Use "identity" for precomputed percentages
  labs(x = "Ηλικία", y = "Ποσοστό (%)", title = "Κατανομή Ηλικιών", fill = "Ηλικία") +  # Rename legend
  theme_minimal() +
  scale_fill_manual(values = palette_colors) +  # Corrected to use scale_fill_manual()
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.position = "none",
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank()   # Remove background for a cleaner look
  )

#######################
library(ggplot2)
library(dplyr)
library(scales)
library(RColorBrewer)

# Example usage:
# create_pie_chart(pani, "soc_cons", title = "Social Consumer Preferences")

table(pani$reason)

# Function to create a pie chart with custom title, color palette, and percentages
create_pie_chart <- function(data, var, title = NULL) {
  # Count the occurrences of each category
  pie_data <- data %>%
    count(!!sym(var)) %>%
    mutate(percentage = n / sum(n) * 100)  # Calculate the percentage
  
  # Create the pie chart
  ggplot(pie_data, aes(x = "", y = n, fill = !!sym(var))) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    theme_void() +
    theme(
      text = element_text(size = 14, face = "bold"),  # Big and bold letters
      axis.text = element_blank(),
      axis.title = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Adjust title size
      legend.title = element_blank()  # Remove the legend title (variable name)
    ) +
    scale_fill_brewer(palette = "PuBuGn") +  # Use color palette Set3
    labs(title = ifelse(is.null(title), paste("Pie Chart for", var), title)) +  # Dynamic title
    geom_text(aes(label = paste0(round(percentage, 1), "%")),  # Add percentages
              position = position_stack(vjust = 0.5),  # Position the text at the center of each segment
              size = 5, color = "black", fontface = "bold")  # Adjust text size, color, and boldness
}

# Example usage:
table(pani$soc_cons, )
new_df <- pani[!is.na(pani$soc_cons), ]
create_pie_chart(new_df, "soc_cons", title = "Ποσοστό ενδιαφέροντος για κοινωνικές δράσεις")

# For env_cons, the legend will not display the variable name (but will still show the answers)
rm(new_df)
table(pani$env_cons,useNA = "ifany")
new_df <- pani %>%
  filter(!is.na(env_cons), env_cons != "Ναι, Όχι")
create_pie_chart(new_df, "env_cons", title = "Ποσοστό ενδιαφέροντος για οικολογικές δράσεις")


str(pani)



###### combined plot
# Create a new variable for answer combinations between env_cons and soc_cons
pani <- pani %>%
  mutate(
    response_combination = case_when(
      env_cons == "Ναι" & soc_cons == "Ναι" ~ "Και οι δύο τύποι δράσεων",
      env_cons == "Όχι" & soc_cons == "Ναι" ~ "Μόνο κοινωνικές δράσεις",
      env_cons == "Ναι" & soc_cons == "Όχι" ~ "Μόνο περιββαλοντικές δράσεις",
      env_cons == "Όχι" & soc_cons == "Όχι" ~ "Κανένας τύπος δράσεων",
      TRUE ~ "ΔΞ/ΔΑ"  # If any other combination exists
    )
  )

# Create percentage bar plot for response combinations
pani %>%
  group_by(response_combination) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ggplot(aes(x = response_combination, y = percentage, fill = response_combination)) +
  geom_bar(stat = "identity") +
  labs(title = "Ποσοστό Απαντήσεων για Περιβαλλοντικές και Κοινωνικές Προτιμήσεις",
       x = "Συνδυασμός Απαντήσεων", y = "Ποσοστό") +
  scale_fill_brewer(palette = "Set2") +  # Choose color palette for readability
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none"
  ) +
  geom_text(aes(label = round(percentage, 1)), vjust = -0.5)  # Add percentage labels

##########################################
# Function to create a pie chart with custom title, color palette, and percentages
create_pie_chart <- function(data, var, title = NULL) {
  # Count the occurrences of each category
  pie_data <- data %>%
    count(!!sym(var)) %>%
    mutate(percentage = n / sum(n) * 100)  # Calculate the percentage
  
  # Create the pie chart
  ggplot(pie_data, aes(x = "", y = n, fill = !!sym(var))) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    theme_void() +
    theme(
      text = element_text(size = 14, face = "bold"),  # Big and bold letters
      axis.text = element_blank(),
      axis.title = element_blank(),
      plot.title = element_text(hjust = 0.4, size = 18, face = "bold"),  # Adjust title size
      legend.title = element_blank()  # Remove only the legend title (variable name)
    ) +
    scale_fill_brewer(palette = "PuBuGn") +  # Use color palette Set3
    labs(title = ifelse(is.null(title), paste("Pie Chart for", var), title)) +  # Dynamic title
    geom_text(aes(label = paste0(round(percentage, 1), "%")),  # Add percentages
              position = position_stack(vjust = 0.2),  # Position the text at the center of each segment
              size = 5, color = "black", fontface = "bold")  # Adjust text size, color, and boldness
  
}

# Create the pie chart for the response combinations
new_df <- pani %>%
  filter(env_cons %in% c("Κανένας τύπος δράσεων", "ΔΞ/ΔΑ"))

create_pie_chart(new_df, "response_combination", title = "Ποσοστό Ενδιαφέροντος για Περιβαλλοντικές και Κοινωνικές Δράσεις")

##########################

library(dplyr)
library(ggplot2)
library(forcats)

# Create filtered dataset
filt <- pani %>%
  filter(!transport %in% c(99, "99")) %>%
  mutate(transport = fct_drop(transport))  # drop unused levels if factor

# Plot using filt
filt %>%
  count(transport) %>%
  ggplot(aes(x = transport, y = n, fill = transport)) +
  geom_bar(stat = "identity") +
  labs(title = "Πλήθος Μεταφορικών Μέσων", x = "Μεταφορικό Μέσο", y = "Αριθμός") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none"
  ) +
  geom_text(aes(label = n), vjust = -0.5)


############################
pani$transport
# Create a bar plot for the 'transport' variable with percentages
filt %>%
  count(transport) %>%
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate the percentage
  ggplot(aes(x = transport, y = percentage, fill = transport)) +
  geom_bar(stat = "identity") +
  labs(title = "Ποσοστό Μεταφορικών Μέσων", x = "Μεταφορικό Μέσο", y = "Ποσοστό (%)") +
  scale_fill_brewer(palette = "Set3") +  # Choose color palette for readability
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none",
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_blank()   # Remove background for a cleaner look
  ) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5)  # Add percentage labels above the bars


########################################################

filt %>%
  count(transport) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = reorder(transport, percentage), y = percentage, fill = transport)) +  # Reorder by percentage
  geom_bar(stat = "identity") +
  coord_flip() +  # Make the bars horizontal
  labs(title = "Ποσοστό Μεταφορικών Μέσων", x = "Ποσοστό (%)", y = "Μεταφορικό Μέσο") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), hjust = -0.2)  # Adjust label position


# Replace specific values with desired replacements
#table(pani$transport)
#pani$transport[pani$transport %in% c("Πεζή")] <- "Πόδια"

########################################################

unique(filt$transport_ath)



filt %>%
  filter(!transport_ath %in% c("99", "ΝΑ", 99)) %>%
  mutate(transport_ath = droplevels(transport_ath)) %>%
  count(transport_ath) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = reorder(transport_ath, percentage), y = percentage, fill = transport_ath)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Ποσοστό Μεταφορικών Μέσων για μετακίνηση εκτός Αθηνών", x = "Ποσοστό (%)", y = "Μεταφορικό Μέσο") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), hjust = -0.2)



















# Step 1: Create a new variable to combine transportation modes into categories
pani %>%
  mutate(
    transport_group = case_when(
      transport %in% c("Λεωφορείο" , "Μετρό") ~ "Λεωφορείο/Μετρό",
      transport %in% c("Ποδήλατο", "Πεζή") ~ "Πεζοί/Ποδήλατο",
      transport %in% c("Αεροπλάνο", "Πλοίο") ~ "Αεροπλάνο/Πλοίο",
      transport %in% c("Μηχανή/μηχανάκι", "Αυτοκίνητο") ~ "Μηχανή/μηχανάκι, Αυτοκίνητο",
      TRUE ~ "Άλλο"  # Include any other values as a fallback category
    )
  ) %>%
  count(transport_group, transport) %>%  # Count the occurrences within each group
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate the percentage for each mode within the group
  ggplot(aes(y = reorder(transport_group, -percentage), x = percentage, fill = transport)) +
  geom_bar(stat = "identity", position = "stack") +  # Stack the bars by transport type within each group
  labs(title = "Ποσοστό Μεταφορικών Μέσων", x = "Ποσοστό (%)", y = "Μεταφορικό Μέσο") +
  scale_fill_manual(values = c("Λεωφορείο" = "seagreen", "Μετρό"="seagreen1", # Colors for Λεωφορείο/Μετρό/Τραμ
                               "Πεζή" = "darksalmon", "Ποδήλατο" = "darksalmon",                     # Colors for Πόδια/Ποδήλατο
                               "Μηχανή/μηχανάκι" = "orchid2", "Αυτοκίνητο" = "orchid4",
                               "Αεροπλάνο"="darkred", "Πλοίο"="red3"
  )) +  # Colors for Μηχανή/μηχανάκι, Αυτοκίνητο
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.position = "right",  # Display the legend on the right
        legend.title = element_blank(),  # Remove the legend title
        legend.text = element_text(size = 14),  # Adjust legend text size
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank()   # Remove background for a cleaner look
  ) +
  # Adding horizontal lines inside the bars to indicate where subcategories stop
  geom_bar(stat = "identity", position = "stack", color = "white", size = 0.3) +  # Add white borders between sections
  geom_hline(aes(yintercept = as.numeric(transport_group)), color = "black", size = 0.8) + geom_text(
    aes(label = sprintf("%.2f%%", percentage)),
    position = position_stack(vjust = 0.5),
    hjust = -0.1,  # Nudges the text slightly to the right of the bar
    size = 4
  ) # Add horizontal lines to separate bars

#############################################################################

# Step 1: Create a new variable to combine transportation modes into categories
pani %>%
  mutate(
    transport_group = case_when(
      transport %in% c("Λεωφορείο" , "Μετρό") ~ "Λεωφορείο/Μετρό",
      transport %in% c("Ποδήλατο", "Πεζή") ~ "Πεζοί/Ποδήλατο",
      transport %in% c("Αεροπλάνο", "Πλοίο","Μηχανή/μηχανάκι", "Αυτοκίνητο") ~ "Αεροπλάνο/Πλοίο\nΜηχανή,Αυτοκίνητο",
      TRUE ~ "Άλλο"  # Include any other values as a fallback category
    )
  ) %>%
  count(transport_group, transport) %>%  # Count the occurrences within each group
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate the percentage for each mode within the group
  ggplot(aes(y = reorder(transport_group, -percentage), x = percentage, fill = transport)) +
  geom_bar(stat = "identity", position = "stack") +  # Stack the bars by transport type within each group
  labs(title = "Ποσοστό Μεταφορικών Μέσων", x = "Ποσοστό (%)", y = "Μεταφορικό Μέσο") +
  scale_fill_manual(values = c("Λεωφορείο" = "seagreen", "Μετρό"="seagreen1", # Colors for Λεωφορείο/Μετρό/Τραμ
                               "Πεζή" = "darksalmon", "Ποδήλατο" = "salmon",                     # Colors for Πόδια/Ποδήλατο
                               "Μηχανή/μηχανάκι" = "orchid2", "Αυτοκίνητο" = "orchid4",
                               "Αεροπλάνο"="darkred", "Πλοίο"="red3"
  )) +  # Colors for Μηχανή/μηχανάκι, Αυτοκίνητο
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.position = "right",  # Display the legend on the right
        legend.title = element_blank(),  # Remove the legend title
        legend.text = element_text(size = 14),  # Adjust legend text size
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank()   # Remove background for a cleaner look
  ) +
  # Adding horizontal lines inside the bars to indicate where subcategories stop
  geom_bar(stat = "identity", position = "stack", color = "white", size = 0.3) +  # Add white borders between sections
  geom_hline(aes(yintercept = as.numeric(transport_group)), color = "black", size = 0.8) + geom_text(
    aes(label = sprintf("%.2f%%", percentage)),
    position = position_stack(vjust = 0.5),
    hjust = -0.1,  # Nudges the text slightly to the right of the bar
    size = 4
  ) # Add horizontal lines to separate bars





table(pani$interests)

table(pani$age)
prop.table(pani$age)
age_group_percentages <- function(age_variable) {
  tbl <- table(age_variable)
  pct <- prop.table(tbl) * 100
  result <- data.frame(
    Age_Group = names(tbl),
    Count = as.vector(tbl),
    Percentage = round(as.vector(pct), 2)
  )
  return(result)
}
age_group_percentages(pani$age)

#############################dummies################################

library(dplyr)
library(ggplot2)
library(tidyr)
# Create a vector with custom labels
custom_labels <- c(
  "volunteering" = "Υποστήριξη κοινωνικά ευπαθών ομάδων",
  "recycle"= "Ανακύκλωση",
  "trees" = "Δενδροφύτευση",
  "trainings" =  "Εκπαιδευτικά προγράμματα",
  "Other" = "Άλλο"
)


# Reshape the dataset and create the plot
pani %>%
  gather(key = "variable", value = "value", volunteering, recycle , trees , trainings, Other ) %>%
  group_by(variable) %>%  # Group by each variable separately
  count(value) %>%  # Count the occurrences of 1 and 0
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate the percentage within each variable
  filter(value == 1) %>%  # Keep only the rows where value is 1
  ggplot(aes(x = reorder(variable, percentage), y = percentage, fill = variable)) +
  geom_bar(stat = "identity") +  # Create bars based on the percentage
  labs(title = "Προθυμία για Περιβαλλοντικές Δράσεις", x = "", y = "Ποσοστό (%)") +
  scale_fill_brewer(palette = "Set3") +  # Choose a color palette for readability
  scale_x_discrete(labels = custom_labels) +  # Map custom labels to the x-axis
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text = element_text(size = 7),
        legend.position = "none",
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank()   # Ensure the background is clear
  ) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, size = 5, fontface = "bold")  # Adjust label position above the bars


#####################################
table(pani$reason)
names(pani)
# Create a vector with custom labels
custom_labels <- c(
  "Convenience" = "Άνεση",
  "Safety" = "Ασφάλεια",
  "Reliability" = "Αξιοπιστία",
  "Time" = "Εξοικονόμηση Χρόνου",
  "Money" = "Οικονομικοί Λόγοι",
  "Easier" = "Ευκολία"
)

# Reshape the dataset and create the plot
pani %>%
  gather(key = "variable", value = "value", Convenience, Safety, Reliability, Time, Money, Easier) %>%
  group_by(variable) %>%  # Group by each variable
  count(value) %>%  # Count occurrences of 1 and 0 within each variable
  mutate(percentage = n / sum(n) * 100) %>%  # Compute percentage per variable
  filter(value == 1) %>%  # Keep only rows where value is 1
  ggplot(aes(x = reorder(variable, percentage), y = percentage, fill = variable)) +
  geom_bar(stat = "identity") +  # Create bars based on the percentage
  labs(title = "Λόγοι Επιλογής Μέσου Μεταφοράς", x = "", y = "Ποσοστό (%)") +
  scale_fill_brewer(palette = "Set3") +  # Choose a color palette for readability
  scale_x_discrete(labels = custom_labels) +  # Map custom labels to the x-axis
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none",
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_blank()   # Remove background for a cleaner look
  ) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, size = 5, fontface = "bold")  # Adjust label position above the bars



#############plots################
library(ggplot2)
pani$
# Scatter plot of carbon footprint vs age group
ggplot(pani, aes(x = age, y = co2_emissions)) +
  geom_boxplot(fill = "lightblue", color = "black") + 
  labs(title = "Carbon Footprint by Age Group",
       x = "Age Group",
       y = "Carbon Footprint (kg CO2)") +
  theme_minimal()



table(pani$co2_emissions)

pani_filtered <- pani %>%
  filter(co2_emissions < 10000)

pani_filtered <- pani_filtered %>%
  mutate(co2_emissions_kg = co2_emissions / 1000)




ggplot(pani_filtered, aes(x = age, y = co2_emissions)) +
  geom_boxplot(fill = "#69b3a2", color = "black", outlier.shape = NA) +  # Hide remaining mild outliers
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +  # Add mean
  labs(
    title = "Αποτύπωμα Άνθρακα ανά Ηλικιακή Ομάδα",
    x = "Ηλικιακή Ομάδα",
    y = "Αποτύπωμα CO_2 (kg)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


#############extra plots



# Filter the dataset for the specified transport modes
filtered_pani <- pani %>%
  filter(transport %in% c("Λεωφορείο", "Μετρό", "Ποδήλατο", "Πεζή"))

# Reshape the dataset and create the plot
filtered_pani %>%
  gather(key = "variable", value = "value", Convenience, Safety, Reliability, Time, Money, Easier) %>%
  group_by(variable) %>%
  count(value) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  filter(value == 1) %>%
  ggplot(aes(x = reorder(variable, percentage), y = percentage, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Λόγοι Επιλογής Οικολογικού Μέσου Μεταφοράς", x = "", y = "Ποσοστό (%)") +
  scale_fill_brewer(palette = "YIGn") +
  scale_x_discrete(labels = custom_labels) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, size = 5, fontface = "bold")


# Filter the dataset for the specified transport modes
filtered_pani <- pani %>%
  filter(transport %in% c("Αεροπλάνο", "Αυτοκίνητο", "Μηχανή/μηχανάκι"))
library(RColorBrewer)
# Reshape the dataset and create the plot
filtered_pani %>%
  gather(key = "variable", value = "value", Convenience, Safety, Reliability, Time, Money, Easier) %>%
  group_by(variable) %>%
  count(value) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  filter(value == 1) %>%
  ggplot(aes(x = reorder(variable, percentage), y = percentage, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Λόγοι Επιλογής Μη Οικολογικού Μέσου Μεταφοράς", x = "", y = "Ποσοστό (%)") +
  scale_fill_brewer(palette = "Reds") +
  scale_x_discrete(labels = custom_labels) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, size = 5, fontface = "bold")


#piechart


table(pani$transport_change)

pani %>%
  filter(!is.na(transport_change)) %>%
  count(transport_change) %>%
  mutate(perc = round(n / sum(n) * 100, 1)) %>%  # Calculate percentages
  ggplot(aes(x = "", y = n, fill = transport_change)) +
  geom_bar(stat = "identity", width = 1, color = NA) +  # Remove border lines
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(perc, "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5, fontface = "bold") +  # Add percentage labels
  labs(title = "Διάθεση για αλλαγή τρόπου μεταφοράς", fill = "Θα επιλέγατε πιο οικολογικό τρόπο μεταφοράς στο γήπεδο;") +  # Rename legend title
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 14, face = "bold")
  )




############ Use the dataset with greek zip codes ################
library(readr)
library(tidygeocoder)
library(geosphere)
library(dplyr)

pani$field <- "15123"

# Step 1: Load Greek ZIP code dataset
greek_zip_data <- read_csv("https://raw.githubusercontent.com/MentatInnovations/grpostcodes/master/data/postcode_lat_long_output_file.csv")

# Rename columns for easier use
greek_zip_data <- greek_zip_data %>%
  rename(zip_code =tk, lat = lat, lon = lon)

# Convert zip codes to character (important for merging)
greek_zip_data <- greek_zip_data %>%
  mutate(zip_code = as.character(zip_code))

greek_zip_data$zip_code

# Step 2: Merge coordinates into `pani` for both viewers' home and stadium
pani <- pani %>%
  mutate(zip_code = as.character(zip_code), field = as.character(field)) %>% # Ensure both are character
  left_join(greek_zip_data, by = c("zip_code" = "zip_code")) %>%
  rename(home_lat = lat, home_lon = lon) %>%  # Rename for clarity
  left_join(greek_zip_data, by = c("field" = "zip_code")) %>%
  rename(stadium_lat = lat, stadium_lon = lon)  # Rename for clarity

# Step 3: Calculate distance (in km) between home and stadium
pani <- pani %>%
  mutate(distance_km = distHaversine(cbind(home_lon, home_lat), 
                                     cbind(stadium_lon, stadium_lat)) / 1000)  # Convert meters to km
#############fix distances##############
table(pani$distance_km)
pani$territory.x[which(pani$distance_km>30)]
pani$territory.x[which(pani$distance_km == 190.114991659918)]
library(dplyr)

pani <- pani %>%
  mutate(distance_km = case_when(
    territory.x == "ΚΑΛΛΙΘΕΑ ΠΛΑΤΕΙΑ ΚΑΛΛΙΘΕΑ" ~ 32,
    territory.x == "ΜΑΓΟΥΛΑ" ~ 32,
    territory.x == "ΑΜΠΕΛΟΚΗΠΟΙ" ~ 10,
    territory.x == "ΑΝΤΙΜΑΧΕΙΑ" ~ 23,
    territory.x == "ΛΙΝΔΟΣ" ~ 200,
    territory.x == "ΩΡΕΟΙ" ~ 190,
    territory.x == "ΚΑΡΔΑΜΥΛΑ" ~ 300,
    territory.x == "ΚΙΑΤΟ" ~ 115,
    distance_km == 0 ~ 2,  # Replace all 0 km distances with 2 km
    TRUE ~ distance_km  # Keep original values for everything else
  ))


# Step 4: Define emission factors
emission_factors <- tibble(
  transport = c("Αυτοκίνητο", "Λεωφορείο", "Μετρό", "Μηχανή/μηχανάκι", 
                "Πεζή", "Ποδήλατο"),
  emission_factor = c(139, 75, 64, 113, 0, 0)  # g CO2-e per km
)

# Step 5: Merge emission factors based on transport mode
pani <- pani %>%
  left_join(emission_factors, by = "transport")

# Step 6: Calculate Eco-Footprint (round trip included)
pani <- pani %>%
  mutate(co2_emissions = distance_km * emission_factor * 2)  # Multiply by 2 for round-trip

# Step 7: Check the dataset
head(pani)

pani$co2_emissions


######calculate the total carbon footprint for this match####
# Sum total carbon emissions for this match
total_carbon_match <- sum(pani$co2_emissions, na.rm = TRUE)

# Print result
print(total_carbon_match)
# total carbon game in tons
#Convert this to tons
total_game_footprint_tons <- total_carbon_match / 1000000
print(total_game_footprint_tons)




#####extrapolate###########
# Compute average carbon footprint per spectator for this match
avg_carbon_per_spectator <- mean(pani$co2_emissions, na.rm = TRUE)
avg_carbon_per_spectator_tons <- avg_carbon_per_spectator / 1000000
avg_carbon_per_spectator_tons
# Project total carbon footprint for all home games
total_seasonal_footprint <- avg_carbon_per_spectator * 3000 * 16

# Print result
print(total_seasonal_footprint)

#Convert this to tons
total_seasonal_footprint_tons <- total_seasonal_footprint / 1000000
print(total_seasonal_footprint_tons)

avg_match_footprint_tons <- total_seasonal_footprint_tons / 16
print(avg_match_footprint_tons)

cat("Total estimated carbon footprint for the season:", total_seasonal_footprint_tons, "t CO₂-e\n")
cat("Average carbon footprint per match:", avg_match_footprint_tons, "t CO₂-e\n")


table(pani$transport)
table(pani$transport_ath)
table(pani$co2_emissions)

library(dplyr)

pani %>%
  group_by(transport) %>%
  summarise(total_co2 = sum(co2_emissions, na.rm = TRUE),
            mean_co2 = mean(co2_emissions, na.rm = TRUE),
            n = n())


library(ggplot2)
pani$transport <- as.character(pani$transport)
pani$transport_ath <- as.character(pani$transport_ath)

# Combine both transport variables into one long format
long_pani <- pani %>%
  select(co2_emissions, transport, transport_ath) %>%
  tidyr::pivot_longer(cols = c(transport, transport_ath), names_to = "segment", values_to = "mode") %>%
  filter(!is.na(mode), mode != "ΝΑ", mode != "99")  # Remove missing or meaningless values

# Group by transport mode and sum co2
mode_summary <- long_pani %>%
  group_by(mode) %>%
  summarise(total_emissions = sum(co2_emissions, na.rm = TRUE)) %>%
  mutate(percentage = total_emissions / sum(total_emissions) * 100)

# Optional: reorder for better plotting
mode_summary <- mode_summary %>%
  arrange(desc(percentage)) %>%
  mutate(mode = factor(mode, levels = mode))

# Plot
ggplot(mode_summary, aes(x = mode, y = percentage, fill = mode)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Carbon Footprint Contribution by Transport Mode",
    x = "Transport Mode",
    y = "Percentage of Total CO₂ Emissions"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



library(ggplot2)
library(dplyr)
library(tidyr)

# Step 1: Combine and clean data
long_pani <- pani %>%
  select(co2_emissions, transport, transport_ath) %>%
  pivot_longer(cols = c(transport, transport_ath), names_to = "segment", values_to = "mode") %>%
  filter(!is.na(mode), mode != "ΝΑ", mode != "99")

# Step 2: Group and calculate percentage
mode_summary <- long_pani %>%
  group_by(mode) %>%
  summarise(total_emissions = sum(co2_emissions, na.rm = TRUE)) %>%
  mutate(percentage = total_emissions / sum(total_emissions) * 100) %>%
  arrange(desc(percentage))

library(ggplot2)
library(dplyr)

# Recreate simpler label with only percentage
mode_summary <- mode_summary %>%
  mutate(label = paste0(round(percentage, 2), "%"))


# Pie chart with percentage labels centered in slices
ggplot(mode_summary, aes(x = "", y = percentage, fill = mode)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4) +
  labs(
    title = "Συνεισφορά Μέσου Μεταφοράς στο Συνολικό Αποτύπωμα CO2",
    fill = "Μέσο Μεταφοράς"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )



library(ggplot2)


library(ggplot2)
library(dplyr)
library(tidyr)

# Step 1: Combine and clean data
long_pani <- pani %>%
  select(co2_emissions, transport, transport_ath) %>%
  pivot_longer(cols = c(transport, transport_ath), names_to = "segment", values_to = "mode") %>%
  filter(!is.na(mode), mode != "ΝΑ", mode != "99")

# Step 2: Group and calculate percentage
mode_summary <- long_pani %>%
  group_by(mode) %>%
  summarise(total_emissions = sum(co2_emissions, na.rm = TRUE)) %>%
  mutate(percentage = total_emissions / sum(total_emissions) * 100) %>%
  filter(percentage > 0) %>%              # <---- Exclude 0% modes
  arrange(desc(percentage)) %>%
  mutate(label = paste0(round(percentage, 2), "%"))  # Add labels

# Step 3: Pie chart
ggplot(mode_summary, aes(x = "", y = percentage, fill = mode)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4) +
  labs(
    title = "Συνεισφορά Μέσου Μεταφοράς στο Συνολικό Αποτύπωμα CO2",
    fill = "Μέσο Μεταφοράς"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )









