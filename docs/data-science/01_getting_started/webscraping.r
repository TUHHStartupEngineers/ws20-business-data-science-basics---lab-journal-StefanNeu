library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing

# home urls
og_url_home <- "https://www.rosebikes.com"
url_home <- "https://www.rosebikes.com/bikes"

# access raw html data for home url
html_home <- read_html(url_home)

# first we need every url for all models by accessing their hrefs and glueing them into urls
model_urls <- html_home %>%

   html_nodes(css = ".catalog-navigation__link") %>%
   
   # get the specific html attribute
   html_attr('href') %>%
   
   # remove unwanted product families
   discard(.p = ~stringr::str_detect(.x,"sale|kids")) %>%

   # convert vector to tibble
   enframe(name = "Name", value = "subdirectory") %>%

   # final glueing of urls
   mutate(
    url = glue("https://www.rosebikes.com{subdirectory}")
   )


# function for acquiring bike data for every model
get_bike_data <- function(url) {
  
  bike_html <- read_html(url)
  
  # 1. obtain list of model names
  model_name_lst <- bike_html %>%
    
    html_nodes(css = ".catalog-category-bikes__title-text") %>%
    
    html_text() %>%
    
    str_remove(pattern="\\n") %>%
    str_remove(pattern="\\n") 
  
  # 2. make list of model type (which is the same for this url)
  model_type <- bike_html %>%
    html_nodes(css = ".catalog-breadcrumb__list-item-link") %>%
    html_attr("title")
  
  model_type <- model_type[length(model_type)]
  
  model_types_lst = rep(model_type, length(model_name_lst))
  
  # 3. obtain prices for every bike
  prices_lst <- bike_html %>%
    
    html_nodes(css = ".catalog-category-bikes__price-title") %>%
    
    html_text() %>% 
    
    str_remove_all(pattern="\\n") 
    
    #str_remove(pattern="from €") %>% 
    
  # return merged tibble
  return(tibble(model_types_lst, model_name_lst, prices_lst))
}

url_vector <- model_urls$url

# use map to execute function for every url in the vector
bike_data_lst <- map(url_vector, get_bike_data)

bike_data_tbl <- bind_rows(bike_data_lst)

bike_data_tbl

