library(tidyverse)

# Load the zomato dataset
zomato <- read_csv("zomato.csv")

# Load the country code mapping dataset
country_codes <- read_csv("country_codes.csv")

# Merge the datasets to add the 3lettercode column
zomato <- zomato %>%
  left_join(country_codes, by = c("Country Code" = "Numeric_Code_Column"))

write.csv(zomato, "zomato1.csv", row.names = FALSE)

# Define the function
rankrestaurant <- function(country, pricerange, num) {
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
    return(tibble(`Restaurant Name` = NA, City = NA))
  }
  
  # Sort by ranking (descending) and handle ties by sorting alphabetically
  sorted_data <- filtered_data %>%
    arrange(desc(`Aggregate rating`), `Restaurant Name`) %>%
    mutate(rank = row_number())  # Add a rank column
  
  # Determine the row to return based on the num argument
  if (num == "best") {
    row_index <- 1
  } else if (num == "worst") {
    row_index <- nrow(sorted_data)
  } else if (is.numeric(num)) {
    if (num > nrow(sorted_data)) {
      return(tibble(`Restaurant Name` = NA, City = NA))
    }
    row_index <- num
  } else {
    stop("invalid num argument")
  }
  
  # Extract the restaurant name and city for the specified rank
  result <- sorted_data %>%
    slice(row_index) %>%
    select(`Restaurant Name`, City)
  
  return(result)
}