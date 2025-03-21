library(tidyverse)



getwd()


setwd("/Users/apple/Downloads/RestaurantData")
zomato <- read_csv("zomato.csv")

head(zomato)

ncol(zomato) 
nrow(zomato) 
names(zomato)
hist(zomato$`Aggregate rating`, 
     main = "Histogram of Restaurant Ratings", 
     xlab = "Aggregate Rating", 
     ylab = "Frequency", 
     col = "skyblue", 
     border = "black")

# Loade the country code mapping dataset
country_codes <- read_csv("country_codes.csv")
# Merge the datasets
zomato <- zomato %>%
  left_join(country_codes, by = c("Country Code" = "Numeric_Code_Column"))

# Define the function
best <- function(country, pricerange) {
  
  
  # Check if the country is valid
  valid_countries <- unique(zomato$`3lettercode`)
  if (!(country %in% valid_countries)) {
    stop("invalid country")
  }
  
  # Check if the price range is valid
  if (!(pricerange %in% c(1, 2, 3, 4))) {
    stop("invalid price range")
  }
  
  # Filter data for the specified country and price range
  filtered_data <- zomato %>%
    filter(`3lettercode` == country, `Price range` == pricerange)
  
  # Check if there is any data for the specified price range in the country
  if (nrow(filtered_data) == 0) {
    return(tibble(`Restaurant Name` = character(), City = character()))
  }
  
  # Sort by ranking (descending) and handle ties by sorting alphabetically
  sorted_data <- filtered_data %>%
    arrange(desc(`Aggregate rating`), `Restaurant Name`) %>%
    slice_head(n = 3) %>%
    select(`Restaurant Name`, City)
 
   return(sorted_data)
}









