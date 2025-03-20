library(tidyverse)

# Define the function
rankall <- function(pricerange, num = "best") {
  # Read the zomato1 data
  zomato <- read_csv("zomato1.csv")
  
  # Check if the price range is valid
  if (!(pricerange %in% c(1, 2, 3, 4))) {
    stop("invalid price range")
  }
  
  # Filter data for the specified price range
  filtered_data <- zomato %>%
    filter(`Price range` == pricerange)
  
  # Check if there is any data for the specified price range
  if (nrow(filtered_data) == 0) {
    return(tibble(restaurant = NA, country = NA))
  }
  
  # Sort by ranking (descending) and handle ties by sorting alphabetically
  sorted_data <- filtered_data %>%
    arrange(desc(`Aggregate rating`), `Restaurant Name`) %>%
    group_by(`3lettercode`) %>%  
    mutate(rank = row_number())  # Add a rank column within each country
  
  # Determine the row to return based on the num argument
  if (num == "best") {
    ranked_data <- sorted_data %>%
      filter(rank == 1)
  } else if (num == "worst") {
    ranked_data <- sorted_data %>%
      group_by(`3lettercode`) %>%  
      filter(rank == max(rank))
  } else if (is.numeric(num)) {
    ranked_data <- sorted_data %>%
      filter(rank == num)
  } else {
    stop("invalid num argument")
  }
  
  # Create a tibble with restaurant names and country codes
  result <- ranked_data %>%
    select(`Restaurant Name`, `3lettercode`) %>% 
    rename(restaurant = `Restaurant Name`, country = `3lettercode`)  
  
  # Ensure all countries are included, even if they have no matching restaurants
  all_countries <- unique(zomato$`3lettercode`)  
  result <- tibble(country = all_countries) %>%
    left_join(result, by = "country") %>%
    select(restaurant, country)
  
  return(result)
}