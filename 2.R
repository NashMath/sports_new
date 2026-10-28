#################panathinaikos
###########Insert dataset############
library(readxl)
library(dplyr)
library(tidyr)
volos<-read_xlsx("C:/Users/user/Desktop/volos.xlsx")
#chelsea<-read.csv("C:/Users/user/Desktop/chelsea.csv")

############change variables names in both datasets
######merge datasets
######## rename variables ###########
######## drop unwanted columns########
names(volos)
volos = volos[,2:9]



#volos
colnames(volos)[1] = "gender"
colnames(volos)[2] = "age"
colnames(volos)[3] = "transport"
colnames(volos)[4] = "veh_passengers"
colnames(volos)[5] = "zip_code"
colnames(volos)[6] = "reason"
colnames(volos)[7] = "env_cons"
colnames(volos)[8] = "interests"


##### merge datasets
pani <-volos
rm(volos)

head(pani)
#remove NAs
pani <- pani %>% drop_na()

######## rename values and fix type #############
table(pani$age)
pani$age <- factor(pani$age,order=TRUE, levels = c("<18", "18-24", "25-34","35-44", "45-54", "55-64", ">64"),
                   labels = c("<18", "18-24", "25-34","35-44", "45-54", "55-64", ">64"))
table(pani$gender)
library(dplyr)

# Remove rows where gender is empty (""), "Άλλο", or NA
pani <- pani %>%
  filter(!gender %in% c("", "Άλλο") & !is.na(gender))

pani$gender <- factor (pani$gender, order=FALSE, labels =c("Άνδρας","Γυναίκα"))
table(pani$gender)

table(pani$transport)

library(dplyr)
library(stringr)  # For better text handling

# Define a regex pattern to detect Gmail addresses
gmail_pattern <- ".*@gmail\\.com"

# Standardize transport column (trim spaces, fix encoding issues)
pani <- pani %>%
  mutate(transport = str_trim(transport)) %>%  # Remove leading/trailing spaces
  filter(transport != "Δεν ήρθα") %>%  # Remove "Δεν ήρθα"
  filter(transport != "sakellarisr10") %>%
  filter(!str_detect(transport, gmail_pattern)) %>%  # Remove Gmail responses
  mutate(transport = case_when(
    str_to_lower(transport) %in% c("taxi", "ταξί", "ταξι") ~ "Αυτοκίνητο",
    str_to_lower(transport) %in% c("ηλεκτρικο", "ηλεκτρικό", "τρενο", "τρένο", "προαστιακό", "μετρο") ~ "Μετρό",
    str_to_lower(transport) %in% c("βανάκι", "βανακι") ~ "Λεωφορείο",
    str_to_lower(transport) %in% c("Πεζή") ~ "Πεζός/Πεζή",
    TRUE ~ transport  # Keep other values unchanged
  ))

# Check cleaned transport values
table(pani$transport)

##################PASSENGERS################

pani$veh_passengers <- as.numeric(pani$veh_passengers)
# Replace specific values with desired replacements
pani$veh_passengers[pani$veh_passengers == 6983443396] <- NA

table(pani$veh_passengers, useNA = "ifany")
#####zip code
table(pani$zip_code)
pani$zip_code <-as.character(pani$zip_code)
zip_code_raw <-pani$zip_code

library(dplyr)
library(stringr)
library(readr)

# Load Greek ZIP code dataset
greek_zip_data <- read_csv("https://raw.githubusercontent.com/MentatInnovations/grpostcodes/master/data/postcode_lat_long_output_file.csv")

# Rename ZIP code and area columns
greek_zip_data <- greek_zip_data %>%
  rename(zip_code = tk, area = territory) %>%  # Use English area names if available
  mutate(zip_code = as.character(zip_code), area = str_to_lower(area))  # Standardize text

# Step 1: Extract numeric ZIP codes (remove decimals)
pani <- pani %>%
  mutate(
    zip_code = str_extract(zip_code, "\\d{5}"),  # Extract first 5-digit number
    zip_code = ifelse(is.na(zip_code), zip_code, as.character(as.integer(zip_code)))  # Convert to integer, then back to character
  )

# Step 2: Automatically match text-based locations to their ZIP codes
pani <- pani %>%
  mutate(
    cleaned_text = str_trim(str_to_lower(zip_code)),  # Convert to lowercase and remove spaces
    zip_code = ifelse(is.na(zip_code), 
                      greek_zip_data$zip_code[match(cleaned_text, greek_zip_data$area)], 
                      zip_code)  # Replace text with corresponding ZIP
  )

# Step 3: Remove any remaining non-ZIP values
pani <- pani %>%
  filter(!is.na(zip_code) & str_detect(zip_code, "^\\d{5}$"))  # Keep only valid 5-digit ZIP codes

# Check cleaned ZIP codes
table(pani$zip_code, useNA = "ifany")

##### dummies for environmental actions###########
library(fastDummies)
library(dplyr)
library(tidyr)
library(stringr)
library(dplyr)
library(purrr)
pani$interests <- gsub('"', '', pani$interests)

# Greek-to-English mapping
translation <- c(
  "Θα αγόραζα αναμνηστικά φτιαγμένα από ανακυκλωμένα υλικά" = "Souvenirs",
  "Θα πετούσα τα απορρίμματα μου στους ειδικούς κάδους" = "recycle",
  "Θα προτιμούσα προϊόντα vegetarian" = "vegetarian_options",
  "Θα προτιμούσα χάρτινες αντί για πλαστικές συσκευασίες" = "plastic",
  "Όλα τα παραπάνω" = "All",
  "Θα χρησιμοποιούσα φιλικούς προς το περιβάλλον τρόπους μετακίνησης για να έρθω στο γήπεδο" = "eco_transport",
  "Κανένα από τα παραπάνω" = "None"
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
table(pani$vegetarian_options)
table(pani$Souvenirs)
table(pani$plastic)
table(pani$eco_transport)
table(pani$recycle)
table(pani$None)
table(pani$All)

##### dummies for reason ###########

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

#Check for NAs in passenger number and vehiclel car
library(dplyr)

# Impute NA in veh_passengers with 2 when transport == "Αυτοκίνητο"
pani <- pani %>%
  mutate(veh_passengers = ifelse(transport == "Αυτοκίνητο" & is.na(veh_passengers), 2, veh_passengers),
         veh_passengers = ifelse(transport == "Πεζή" & is.na(veh_passengers), 1, veh_passengers),
         veh_passengers = ifelse(transport == "Μηχανή/μηχανάκι" & is.na(veh_passengers), 1, veh_passengers))
# 7 observations were imputed with 2

# Check result
summary(pani$veh_passengers)

#Remaining NAs are answers with public transport 
num<-c(which(is.na(pani$veh_passengers)))

table(pani[num,]$transport)

##################################### EDA########################################
library(ggplot2)



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

table(pani$env_cons)

# Replace NAs with "Δεν Ξέρω/Δεν απαντώ"
pani$env_cons[is.na(pani$env_cons)] <- "Δεν Ξέρω/Δεν απαντώ"

# Replace multiple values with "Ναι"
pani$env_cons[pani$env_cons %in% c("Ναι, Όχι", "Ναι;Δεν Ξέρω/Δεν απαντώ", "Ναι;Όχι", "")] <- "Ναι"

# Replace "Όχι;Δεν Ξέρω/Δεν απαντώ" with "Όχι"
pani$env_cons[pani$env_cons == "Όχι;Δεν Ξέρω/Δεν απαντώ"] <- "Όχι"

# Check the updated table
table(pani$env_cons)
clean <- pani %>% filter(!is.na(env_cons))

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


# For env_cons, the legend will not display the variable name (but will still show the answers)
create_pie_chart(pani, "env_cons", title = "Ποσοστό ενδιαφέροντος για οικολογικές δράσεις")





############################

pani %>%
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
table(pani$transport)
pani$transport[pani$transport %in% c("Πεζή")] <- "Πόδια"
# Step 1: Create a new variable to combine transportation modes into categories
pani %>%
  mutate(
    transport_group = case_when(
      transport %in% c("Λεωφορείο", "Μετρό", "Τραμ") ~ "Τραμ/Μετρό/Λεωφορείο",
      transport %in% c("Πόδια", "Ποδήλατο", "Πεζή") ~ "Πόδια/Ποδήλατο",
      transport %in% c("Μηχανή/μηχανάκι", "Αυτοκίνητο") ~ "Μηχανή/μηχανάκι, Αυτοκίνητο",
      TRUE ~ "Αεροπλάνο"  # Include any other values as a fallback category
    )
  ) %>%
  count(transport_group, transport) %>%  # Count the occurrences within each group
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate the percentage for each mode within the group
  ggplot(aes(y = reorder(transport_group, -percentage), x = percentage, fill = transport)) +
  geom_bar(stat = "identity", position = "stack") +  # Stack the bars by transport type within each group
  labs(title = "Επιλογή τρόπου μεταφοράς ανά κατηγορία περιβαλλοντικού αποτυπώματος", x = "Ποσοστό (%)", y = "Μεταφορικό Μέσο") +
  scale_fill_manual(values = c("Λεωφορείο" = "green", "Μετρό" = "seagreen3",  # Colors for Λεωφορείο/Μετρό/Τραμ
                               "Πόδια" = "darkgreen","Ποδήλατο" = "darkgreen",                     # Colors for Πόδια/Ποδήλατο
                               "Μηχανή/μηχανάκι" = "maroon", "Αυτοκίνητο" = "darkred",
                               "Αεροπλάνο" = "red")) +  # Colors for Μηχανή/μηχανάκι, Αυτοκίνητο
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
  geom_hline(aes(yintercept = as.numeric(transport_group)), color = "black", size = 0.8)  # Add horizontal lines to separate bars


#############################dummies################################
table(pani$interests)
library(dplyr)
library(ggplot2)
library(tidyr)
# Create a vector with custom labels
custom_labels <- c(
  "Souvenirs" = "Aνακυκλωμένα υλικά",
  "recycle" = "Ανακύκλωση",
  "vegetarian_options" = "Vegetarian προϊόντα",
  "plastic" = "Αποφυγή Πλαστικού",
  "All" = "Όλες",
  "eco_transport" = "Οικολογική Μετακίνηση",
  "None" = "Καμία"
)


# Reshape the dataset and create the plot
pani %>%
  gather(key = "variable", value = "value", Souvenirs, recycle, vegetarian_options, plastic, All, eco_transport, None) %>%
  group_by(variable) %>%  # Group by each variable separately
  count(value) %>%  # Count the occurrences of 1 and 0
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate the percentage within each variable
  filter(value == 1) %>%  # Keep only the rows where value is 1
  ggplot(aes(x = reorder(variable, percentage), y = percentage, fill = variable)) +
  geom_bar(stat = "identity") +  # Create bars based on the percentage
  labs(title = "Προθυμία για Οικολογικές Δράσεις", x = "", y = "Ποσοστό (%)") +
  scale_fill_brewer(palette = "Set3") +  # Choose a color palette for readability
  scale_x_discrete(labels = custom_labels) +  # Map custom labels to the x-axis
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 14),
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


table(pani$transport)
# Step 4: Define emission factors
emission_factors <- tibble(
  transport = c("Αεροπλάνο","Αυτοκίνητο", "Λεωφορείο", "Μετρό", "Μηχανή/μηχανάκι", 
                "Ποδήλατο", "Πόδια"),
  emission_factor = c(201, 139, 75, 64, 113, 0, 0)  # g CO2-e per km
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
dim(pani)
#avg_2 <- total_carbon_match/205
avg_carbon_per_spectator_tons <- avg_carbon_per_spectator / 1000000
avg_carbon_per_spectator_tons


egx<-(4281+2064+50272+5815+15624+3078+8350+2718+11701+2470+9695+2149+24279+4097+18758+4179+13151+3157)



# Project total carbon footprint for all home games
total_seasonal_footprint <- avg_carbon_per_spectator * egx

#Convert this to tons
total_seasonal_footprint_tons <- total_seasonal_footprint / 1000000
print(total_seasonal_footprint_tons)


avg_match_footprint_tons <- total_seasonal_footprint_tons / 11
print(avg_match_footprint_tons)

cat("Total estimated carbon footprint for the season:", total_seasonal_footprint_tons, "t CO₂-e\n")
cat("Average carbon footprint per match:", avg_match_footprint_tons, "t CO₂-e\n")
avg_seas_spect <- avg_carbon_per_spectator_tons * 11
avg_seas_spect

#bootstraping
(1.387+18.631)
(0.0071+0.0204)/2
(1322.642+3929.994)
(120.24+654.99)/2
654.99/120.24
(2.23+0.07)/2
2.23/0.07
