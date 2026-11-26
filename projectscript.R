# --------------------------------------
# DATA CLEANING: EDITING AND IMPUTATION
# --------------------------------------

# 1. Loading the required packages
library(readr)      # For reading data files
library(validate)   # For validation rules
library(errorlocate) # For error localization
library(VIM)        # For missing data visualization and imputation
library(dplyr)      # For data manipulation
library(ggplot2)    # For visualization
library(deducorrect) # For deductive correction

# 2. Downloading and extracting the dataset from UCI repository
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00360/AirQualityUCI.zip"
temp <- tempfile()  # Creating the temporary file
download.file(url, temp)  # Downloading the zip file
unzip(temp, files = "AirQualityUCI.csv")  # Extracting the CSV format
file.remove(temp)   # Cleaning up the temporary file

# 3. Reading the data (handling European decimal commas and -200 as NA)
aq <- read_delim("AirQualityUCI.csv", 
                 delim = ";", 
                 na = c("-200", "NA"),
                 locale = locale(decimal_mark = ","))

# 4. Removing the empty rows and columns at the end as the dataset has many blank rows and two blank columns 
aq <- aq[1:9355,1:15]  

# 5. Cleaning column names by removing spaces and special characters to make it legible
names(aq) <- make.names(names(aq))

aq <- aq %>%
  mutate(PT08.S1.CO. = ifelse(runif(n()) < 0.05, NA, PT08.S1.CO.))
aq <- aq %>%
  mutate(PT08.S2.NMHC. = ifelse(runif(n()) < 0.08, NA, PT08.S2.NMHC.))
aq <- aq %>%
  mutate(PT08.S3.NOx. = ifelse(runif(n()) < 0.03, NA, PT08.S3.NOx.))
aq <- aq %>%
  mutate(PT08.S4.NO2. = ifelse(runif(n()) < 0.07, NA, PT08.S4.NO2.))
aq <- aq %>%
  mutate(PT08.S5.O3. = ifelse(runif(n()) < 0.06, NA, PT08.S5.O3.))
aq <- aq %>%
  mutate(T = ifelse(runif(n()) < 0.10, NA, T))


# 1. DATA INSPECTION PHASE
# Viewing the structure and some summary statistics
str(aq)
dim(aq)
summary(aq)
View(aq)

# 2. Checking for missing values (just to be sure that the previous -200 values have been correctly transformed into NA values)
colSums(is.na(aq))

# 3. DEFINITION OF THE VALIDATION RULES
# the rules will be based on physical/chemical constraints
range(aq$AH, na.rm = TRUE)
range(aq$NO2.GT., na.rm = TRUE)
range(aq$CO.GT., na.rm = TRUE)
range(aq$NMHC.GT., na.rm = TRUE)
range(aq$NOx.GT., na.rm = TRUE)
range(aq$C6H6.GT., na.rm = TRUE)
range(aq$RH, na.rm = TRUE)
range(aq$T, na.rm = TRUE)
range(aq$PT08.S1.CO., na.rm = TRUE)
boxplot(aq$NMHC.GT., na.rm = TRUE)
boxplot(aq$NOx.GT., na.rm = TRUE)

rules <- validator(
  # Gas concentrations ranges appropriate for Rome (can't be negative)
  CO.GT. <= 35, 
  NMHC.GT. <= 200,
  C6H6.GT. <= 20, # Cap at 20 µg/m³ for extreme cases
  NO2.GT. <= 200, # Align with EU standards
  NOx.GT. >= NO2.GT. & NOx.GT. <= 300, #that's because NOX = NO + NO2
  
  # Temperature constraints (reasonable range for Rome taking into account extreme situations)
  T >= -2 &  T <= 45,
  
  # Relative humidity constraints (reasonable range for Rome), from the meteo
  RH >= 20 &  RH <= 80, #Hourly dips below 20% RH are unlikely except in rare heatwaves.
  
  # Absolute humidity reasonable range given RH and T as kg/m³
  # s.t. if for g/m³ we would have had a range like : AH >= 3.7 &  AH <= 27, now we deal with 
  #AH <= 0.027, but still doesn't make much sense
  
  # Sensor responses cannot be negative as they register the hourly average concentration
  PT08.S1.CO. > 0, #& PT08.S1.CO. <= 1500,
  PT08.S2.NMHC. > 0, # & PT08.S2.NMHC. <= 2000,
  PT08.S3.NOx. > 0, #& PT08.S3.NOx. <= 1000,
  PT08.S4.NO2. > 0, #& PT08.S4.NO2. <= 2000,
  PT08.S5.O3. > 0 #& PT08.S5.O3. <= 1500
)
rules
#Rule-based corrections like CO := 0 if CO < 0 or NOx := NO2 if NOx < NO2 are examples of deductive editing, since they are applied deterministically based on domain-specific knowledge about the physical or chemical meaning of the variables.

# 4. VALIDATING THE DATA QUALITY
# need to check which records violate which rules
validation <- confront(aq, rules)
validation
val_summary <- summary(validation)
val_summary
#interesting to notice that many variables present the exact same number of missing

detect_boundary_num(rules)

# in order to visualize it graphically, let's convert to dataframe for plotting
val_df <- as.data.frame(val_summary)
val_df$rule <- rownames(val_df)

# Plot
ggplot(val_df, aes(x = reorder(rule, -fails), y = fails)) +
  geom_bar(stat = "identity", fill = "tomato") +
  labs(
    title = "Validation Rule Violations",
    x = "Rule",
    y = "Number of Failing Records",
    caption = paste("Total records:", nrow(aq))
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
#so there're 5 rules that are not verified, and they regard the following variables
#nice to see that the deterministic input has been failed
table(aq$NOx.GT. >= aq$NO2.GT.)
#FALSE  TRUE 
#1274  6439

#alternatively 
plot(validation)

aggregate(validation) #rule-wise aggregation


# 5. Locating the errors in the dataset
errors <- locate_errors(aq, rules)
errors
#located  5813 error(s).
#located  16242  missing value(s)

summary(errors) #in this way we can retrieve the errors

# Estrai le regole violate per ogni unità con 2 errori
error_counts <- rowSums(!is.na(values(validation)) & !values(validation))  # Conta errori per riga
units_2_errors <- which(error_counts == 2)  # 973 unità
rule_pairs <- apply(values(validation)[units_2_errors, ], 1, function(x) {
  paste(names(which(!x)), collapse = " & ")  # Es: "V04 & V05"
})

# Frequenza delle coppie di regole
table(rule_pairs) %>% sort(decreasing = TRUE)


# 6. Visualizing missing data pattern (red cells as missing values)
aggr(aq, numbers = TRUE, prop = FALSE)
# alternatively
VIM::aggr(aq, numbers = TRUE, prop = FALSE) 
#looking at whether some units are item non response or unit non response
na_patterns <- apply(is.na(aq), 2, paste, collapse = "")
table(na_patterns)

#checking for which units we get all true
row_patterns <- apply(is.na(aq), 1, paste, collapse = "")
table(row_patterns)

# Extract the validation results matrix (TRUE=pass, FALSE=fail, NA=missing)
validation_results <- values(validation)

# Get row indices of NAs for each rule
na_indices <- lapply(1:ncol(validation_results), function(i) {
  which(is.na(validation_results[, i]))
})
names(na_indices) <- colnames(validation_results)  # Name by rule (V01, V02, etc.)

# Example: Find rows where V02, V04, and V05 are all NA
rules_to_check <- c("V06", "V07", "V08", "V09", "V10", "V11", "V12")  
# Get the NA mask (logical matrix where TRUE = NA)
na_mask <- is.na(validation$._value)
if (length(rules_to_check) > 0) {
  common_na_rows <- Reduce(intersect, lapply(rules_to_check, function(rule) {
    which(na_mask[, rule])  # Estrai gli indici delle righe NA per ogni regola
  }))
  print(common_na_rows)  # Righe con NA in tutte le regole
} else {
  message("Nessuna regola valida selezionata.")
}
# Print overlapping NA rows
print(common_na_rows)

# Optional: View the original data for these rows
View(aq[common_na_rows, ])

# 7. Performing hot-deck imputation (donor-based imputation)
# by using 'Time' as domain variable to find similar time points
aq_imputed <- VIM::hotdeck(aq, 
                           domain_var = "Time",
                           imp_var = FALSE)
aq_imputed


# 6. CORRECTING INVALID VALUES
# Applying simple rule-based corrections:
aq_cleaned <- aq_imputed %>%
  mutate(
    # Replacing the negative concentrations with 0
    CO.GT. = ifelse(CO.GT. < 0, 0, ifelse(CO.GT. > 20, 20, CO.GT.)),
    C6H6.GT. = ifelse(C6H6.GT. < 0, 0, ifelse(C6H6.GT. > 20, 20, C6H6.GT.)),
    NO2.GT. = ifelse(NO2.GT. < 0, 0, ifelse(NO2.GT. > 200, 200, NO2.GT.)),
    NOx.GT. = ifelse(NOx.GT. < NO2.GT., NO2.GT., ifelse(NOx.GT. > 300, 300, NOx.GT.)),
    
    # Ensuring T stays within the range previously established
    T = ifelse(T < 2.5, 2.5, ifelse(T > 42, 42, T)),
    
    # Ensuring RH stays within the range previously established
    RH = ifelse(RH < 40, 40, ifelse(RH > 80, 80, RH)),
    
    # Ensuring AH stays within the range previously established
    AH = ifelse(AH < 3.7, 3.7, ifelse(AH > 27, 27, AH)),
    
    # Ensuring the sensors values are within range
    PT08.S1.CO. = ifelse(PT08.S1.CO. < 0, 0, ifelse(PT08.S1.CO. > 1500, 1500, PT08.S1.CO.)),
    PT08.S2.NMHC. = ifelse(PT08.S2.NMHC. < 0, 0, ifelse(PT08.S2.NMHC. > 2000, 2000, PT08.S2.NMHC.)),
    PT08.S3.NOx. = ifelse(PT08.S3.NOx. < 0, 0, ifelse(PT08.S3.NOx. > 1000, 1000, PT08.S3.NOx.)),
    PT08.S4.NO2. = ifelse(PT08.S4.NO2. < 0, 0, ifelse(PT08.S4.NO2. > 2000, 2000, PT08.S4.NO2.)),  
    PT08.S5.O3. = ifelse(PT08.S5.O3. < 0, 0, ifelse(PT08.S5.O3. > 1500, 1500, PT08.S5.O3.))
    )


# 7. FINAL VALIDATION CHECK
# Verifying that all rules are now satisfied
final_check <- confront(aq_cleaned, rules)
summary(final_check)  
# shows 0 fails
aggr(aq_cleaned, numbers = TRUE, prop = FALSE)
#no more red cells --> no more missings 
