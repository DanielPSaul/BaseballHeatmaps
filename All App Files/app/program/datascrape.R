## Function to scrape data from a public GitHub repo

# Load packages
library(dplyr)
library(rvest)

# Input: url of a github repo folder with a list of CSVs
# Output: DataFrame

data_scrape <- function(url) {
  url %>%
    read_html() %>%
    html_nodes(xpath = '//*[@role="rowheader"]') %>%
    html_nodes('span a') %>%
    html_attr('href') %>%
    #head %>% # <- remove this line to read all the files. 
    sub('blob/', '', .) %>%
    paste0('https://raw.githubusercontent.com', .) %>%
    purrr::map_df(read.csv) ->  combined_data
  return(combined_data)
}

# Example:
ds_2022_data <- data_scrape('https://github.com/chrisgaut/data/tree/main/reg_season_2022')
# The result of this call is ds_2022_data being a DataFrame of data similar to the
# master dataframes in server.R for 2020-2022
