# Student: Gabriella Delgado
# Student Number: EDUV4815041
# Module: ITBDA4-14
# PROJECT 2
# QUESTION 4

# Load libraries
library(rvest)
library(httr)

# Question 4.1
# Web Scraping and Regex Analysis

# Read the webpage
url <- "https://www.scrapethissite.com/pages/simple/"
page <- read_html(url)

# Extract the records
countries <- html_elements(page, ".country")

if (length(countries) < 20) {
  stop("Fewer than 20 records were extracted.")
}

# Create a raw data frame
raw_df <- data.frame(
  Country_Raw = html_text(html_element(countries, ".country-name")),
  Capital_Raw = html_text(html_element(countries, ".country-capital")),
  Population_Raw = html_text(html_element(countries, ".country-population")),
  Area_Raw = html_text(html_element(countries, ".country-area"))
)

# Extract numerical values using regex
raw_df$Population_Extracted <- as.numeric(
  gsub("[^0-9]", "", raw_df$Population_Raw)
)

raw_df$Area_Extracted <- as.numeric(
  gsub("[^0-9.eE+-]", "", raw_df$Area_Raw)
)

# Display the raw data
str(raw_df)
print(head(raw_df, 10))
cat("Records extracted:", nrow(raw_df), "\n")

# Display records with missing information
print(raw_df[
  tolower(trimws(raw_df$Capital_Raw)) %in% c("", "none", "n/a"),
])

# Display inconsistent numerical formatting
print(raw_df[
  grepl("[eE][+-]?[0-9]+", raw_df$Area_Raw),
])


# Question 4.2
# Data Cleaning, Denoising and Missing-Value Imputation

# Denoise text by removing extra whitespace and line breaks
country_clean <- trimws(gsub("[[:space:]]+", " ", raw_df$Country_Raw))
capital_clean <- trimws(gsub("[[:space:]]+", " ", raw_df$Capital_Raw))

# Create a cleaned data frame
clean_df <- data.frame(
  country = country_clean,
  capital = capital_clean,
  population = raw_df$Population_Extracted,
  area_km2 = raw_df$Area_Extracted
)

# Remove empty country names and duplicate records
clean_df <- clean_df[!is.na(clean_df$country) & nzchar(clean_df$country), ]

clean_df <- clean_df[!duplicated(clean_df$country), ]

# Identify and handle the missing capital information
clean_df$capital_imputed <- is.na(clean_df$capital) | tolower(clean_df$capital) %in% c("", "none", "n/a")

clean_df$capital[clean_df$capital_imputed] <- "Not provided"

# Identify missing or invalid numerical values
clean_df$population_imputed <- is.na(clean_df$population)
clean_df$area_imputed <- is.na(clean_df$area_km2) | clean_df$area_km2 <= 0

# Treat the invalid area values as missing
clean_df$area_km2[clean_df$area_imputed] <- NA

# Impute missing numerical values using the median
if (any(clean_df$population_imputed)) {
  clean_df$population[clean_df$population_imputed] <-
    round(median(clean_df$population, na.rm = TRUE))
}

if (any(clean_df$area_imputed)) {
  clean_df$area_km2[clean_df$area_imputed] <-
    median(clean_df$area_km2, na.rm = TRUE)
}

# Display the cleaned data and data quality checks
print(head(clean_df, 10))
print(colSums(is.na(clean_df)))

cat("Missing capitals handled:", sum(clean_df$capital_imputed), "\n")
cat("Populations imputed:", sum(clean_df$population_imputed), "\n")
cat("Areas imputed:", sum(clean_df$area_imputed), "\n")
cat("Cleaned records:", nrow(clean_df), "\n")

# Display records with imputed area values
print(clean_df[clean_df$area_imputed, c("country", "area_km2")])

# Export the cleaned dataset as a CSV file
write.csv(clean_df, "/Users/gaby/Desktop/Countries_Clean_Data.csv", row.names = FALSE)