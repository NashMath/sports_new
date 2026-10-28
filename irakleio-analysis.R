#################DATA ANALYSIS PANIONIOS DATA###############################

###########Insert dataset############
library(readxl)
pani<-read_xlsx("C:/Users/user/Desktop/sports_new/irakleio.xlsx")

######## drop unwanted columns########
names(pani)
pani = pani[,2:11]

######## rename variables ###########

colnames(pani)[1] = "gender"
colnames(pani)[2] = "age"
colnames(pani)[3] = "transport"
colnames(pani)[4] = "veh_passengers"
colnames(pani)[5] = "zip_code"
colnames(pani)[6] = "reason"
colnames(pani)[7] = "transport_change"
colnames(pani)[8] = "env_cons"
colnames(pani)[9] = "soc_cons"
colnames(pani)[10] = "interests"
names(pani)
head(pani)

######## rename values and fix type #############
table(pani$age)
pani$age <- factor(pani$age,order=TRUE, levels = c("<18", "18-24", "25-34","35-44", "45-54", "55-64"), #, ">64"
                   labels = c("<18", "18-24", "25-34","35-44", "45-54", "55-64")) #, ">64"
table(pani$gender)
pani$gender <- factor (pani$gender, order=FALSE, labels =c("Άνδρας","Γυναίκα"))
table(pani$transport)

# Replace the last two values with "Τραμ"
pani$transport[pani$transport %in% c("Ταξι")] <- "Αυτοκίνητο"
pani$transport <- factor(pani$transport, order=FALSE,
                         labels = c("Αυτοκίνητο", "Λεωφορείο", "Μηχανή/μηχανάκι", "Πεζή", "Ποδήλατο" ))


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

########## zip_code ################
table(pani$zip_code, useNA = "ifany")
pani$zip_code[pani$zip_code == "70014 αναληψη"] <- "70014"
pani$zip_code[pani$zip_code == "Γαζί, 71410"] <-"71410"
pani$zip_code[pani$zip_code %in% c("Ηρακλειο 71304", "Ηρακλειου", "ηράκλειο", "Ηράκλειο")] <- "71304"
pani$zip_code[pani$zip_code %in% c("Χανιά" , "Χανιά 73100")] <- "73100"
# pani$zip_code[pani$zip_code %in% c("Παγκράτι")] <- "11633"
# pani$zip_code[pani$zip_code %in% c("Παλαιο φαληρο 17563")] <- "17563"
# pani$zip_code[pani$zip_code %in% c("Άλιμο")] <- "17456"
# pani$zip_code[pani$zip_code %in% c("νέα μάκρη")] <- "19005"
# pani$zip_code[pani$zip_code %in% c("Νέο κοσμο")] <- "11744"
pani$zip_code <- as.numeric(pani$zip_code)
table(pani$zip_code, useNA = "ifany")

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

# Create a bar plot for the 'transport' variable
pani %>%
  count(transport) %>%
  ggplot(aes(x = transport, y = n, fill = transport)) +
  geom_bar(stat = "identity") +
  labs(title = "Πλήθος Μεταφορικών Μέσων", x = "Μεταφορικό Μέσο", y = "Αριθμός") +
  scale_fill_brewer(palette = "Set3") +  # Choose color palette for readability
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none"
  ) +
  geom_text(aes(label = n), vjust = -0.5)  # Add count labels above the bars


############################

# Create a bar plot for the 'transport' variable with percentages
pani %>%
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
#table(pani$transport)
#pani$transport[pani$transport %in% c("Πεζή")] <- "Πόδια"

# Step 1: Create a new variable to combine transportation modes into categories
pani %>%
  mutate(
    transport_group = case_when(
      transport %in% c("Λεωφορείο") ~ "Λεωφορείο",
      transport %in% c("Ποδήλατο", "Πεζή") ~ "Πεζοί/Ποδήλατο",
      transport %in% c("Μηχανή/μηχανάκι", "Αυτοκίνητο") ~ "Μηχανή/μηχανάκι, Αυτοκίνητο",
      TRUE ~ "Άλλο"  # Include any other values as a fallback category
    )
  ) %>%
  count(transport_group, transport) %>%  # Count the occurrences within each group
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate the percentage for each mode within the group
  ggplot(aes(y = reorder(transport_group, -percentage), x = percentage, fill = transport)) +
  geom_bar(stat = "identity", position = "stack") +  # Stack the bars by transport type within each group
  labs(title = "Ποσοστό Μεταφορικών Μέσων", x = "Ποσοστό (%)", y = "Μεταφορικό Μέσο") +
  scale_fill_manual(values = c("Λεωφορείο" = "seagreen",  # Colors for Λεωφορείο/Μετρό/Τραμ
                               "Πεζή" = "darksalmon", "Ποδήλατο" = "darksalmon",                     # Colors for Πόδια/Ποδήλατο
                               "Μηχανή/μηχανάκι" = "orchid2", "Αυτοκίνητο" = "orchid4"
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
    "Other" = "Άλλο (παρακαλώ προσδιορίστε)"
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

pani$zip_code
pani$zip_code[pani$zip_code %in% c(8, 85200)] <- NA

#TK of 
pani$field <- "71303"
#pani$field_pagkrhtio <- "71303"

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
pani$territory.x[which(pani$distance_km>115)]
which(pani$distance_km>115)

pani$distance_km[pani$distance_km > 115] <- NA


library(dplyr)



# Step 4: Define emission factors
emission_factors <- tibble(
  transport = c("Αυτοκίνητο", "Λεωφορείο", "Μηχανή/μηχανάκι", 
                "Πεζή", "Ποδήλατο"),
  emission_factor = c(139, 75, 113, 0, 0)  # g CO2-e per km
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


0.007973179*18930


library(ggplot2)

# Summarize total emissions by transport mode

transport_emissions <- pani %>%
  group_by(transport) %>%
  summarise(total_emissions = sum(co2_emissions, na.rm = TRUE)) %>%
  filter(total_emissions > 0) %>%
  mutate(percentage = total_emissions / sum(total_emissions) * 100)

# Create pie chart
ggplot(transport_emissions, aes(x = "", y = percentage, fill = transport)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5), size = 5) +
  labs(title = "Συνεισφορά κάθε μέσου μεταφοράς στο αποτύπωμα άνθρακα",
       fill = "Μέσο Μεταφοράς") +
  theme_void() +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))


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


#############plots################
library(ggplot2)

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





























