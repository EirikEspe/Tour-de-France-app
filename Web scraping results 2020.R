#####################################################
#
# Shiny app
# Scraping results from 2020 edition
#
#####################################################

# Load packages
library(rvest)
library(stringr)
library(dplyr)


# Starting with scraping the first stage

## Url we want to scrape
url <- "https://www.procyclingstats.com/race/tour-de-france/2020/stage-1"


# Scrape name of winner of first stage
read_html(url) %>% 
  html_node("table.basic.results a") %>% 
  html_text() %>% 
  # Remove whitespace
  str_trim() 

# Scrape first stage
stage1 <- tibble(edition = 107,
                 
                 year = read_html(url) %>% 
                   html_nodes("span.yearmob") %>% 
                   html_text() %>% as.numeric(),
                 
                 stage_results_id = str_extract(url, "stage-.+"),
                 
                 rank = read_html(url) %>% 
                   html_nodes("div:nth-child(3) td:nth-child(1)") %>% 
                   html_text(),
                 
                 # The full pre-processing of the time column is outlined below.
                 # This is just the output of the scraping
                 time = read_html(url) %>% 
                   html_nodes("div:nth-child(3) span.timelag") %>% 
                   html_text(),
                 
                 rider = read_html(url) %>% 
                   html_nodes("div:nth-child(3) td:nth-child(5) a") %>% 
                   html_text() %>% str_trim(),
                 
                 age = read_html(url) %>% 
                   html_nodes("div:nth-child(3) td:nth-child(6)") %>% 
                   html_text() %>% as.numeric(),
                 
                 team = read_html(url) %>% 
                   html_nodes("div:nth-child(3) td:nth-child(5) span.teammob") %>% 
                   html_text(),
                 
                 points = read_html(url) %>% 
                   html_nodes("div:nth-child(3) td:nth-child(9)") %>% 
                   html_text() %>%
                   as.numeric(),
                 
                 # Elapsed time is here shown as NA for simplicity.
                 # This column requires pre-processing of the time column and is therefore
                 # left blank in this initial scrape of the first stage
                 elapsed = NA,
                 
                 bib_number = read_html(url) %>% 
                   html_nodes("div:nth-child(3) td:nth-child(4)") %>% 
                   html_text() %>% 
                   {if_else(. == "1", TRUE, NA)})



# Process for scraping time and conversion from character to period object

## First, scrape time. This is a character vector.
time <- read_html(url) %>% 
  html_nodes("div:nth-child(3) span.timelag") %>% 
  html_text() %>% 
  replace(., . %in% c(",,", "0:00"), NA) %>% as_tibble()


# Create a character column for time, where we make the values more consistent

## 1. The first row is the time taken for stage winner to reach the finish line.
## 2. The following rows show the time difference between the winner and the competitors.
##      If the stage winner arrived at the finish line in a group with other 
##      riders, the whole group will get the same time. In this case we will write the time 
##      difference equal to 0, because all the riders get the same race time as the winner. 
##      For more information about timekeeping in TdF, 
##      see https://inrng.com/2013/02/cycling-race-three-kilometre-rule-history/
## 3. Use last observation carried forward (from zoo package) on all riders that were not 
##      in the same group as the winner, i.e. finished behind the winner
time <- time %>% mutate(timestr = case_when(value == first(value) ~ value, 
                                            row_number() < which(!is.na(value))[2] ~ "0",
                                            TRUE ~ zoo::na.locf(value)))

# Converting the time format from character to period
time %>% 
  mutate(
    # This column shows the finishing time of the stage winner, while for the other
    # competitors this is the time difference to the winner.
    ## Convert from character to period object
    time = case_when(
      # If value has format H:M:S then use the converter hms
      str_detect(timestr, ":\\d+:") ~ lubridate::hms(timestr),
      # If value has format M:S then use the converter ms
      str_detect(timestr, ":") ~ lubridate::ms(timestr),
      timestr == "-" ~ lubridate::as.period(NA),
      TRUE ~ lubridate::seconds(0)),
    # Create a column for total time taken to reach the finish line
    total_time = time[1] + 
      case_when(
        time == first(time) ~ lubridate::seconds(0),
        TRUE ~ time),
    # Make seconds above 60s into additional minute, using the roll parameter
    total_time = lubridate::hms(as.character(total_time), roll = TRUE)) %>% 
  View("time")


# Create function for scraping results from a stage
get_results <- function(url) {
  stage_results <- tibble(edition = 107,
                          
                          year = read_html(url) %>% 
                            html_nodes("span.yearmob") %>% 
                            html_text() %>% as.numeric(),
                          
                          stage_results_id = str_extract(url, "stage-.+"),
                          
                          rank = read_html(url) %>% 
                            html_nodes("div:nth-child(3) td:nth-child(1)") %>% 
                            html_text(),
                          
                          time = read_html(url) %>% 
                            html_nodes("div:nth-child(3) span.timelag") %>% 
                            html_text() %>% 
                            replace(., . %in% c(",,", "0:00"), NA) %>% as_tibble() %>% 
                            mutate(timestr = case_when(value == first(value) ~ value, 
                                                       row_number() < which(!is.na(value))[2] ~ "0",
                                                       TRUE ~ zoo::na.locf(value)),
                                   time = case_when(
                                     # If value has format H:M:S then use the converter hms
                                     str_detect(timestr, ":\\d+:") ~ lubridate::hms(timestr),
                                     # If value has format M:S then use the converter ms
                                     str_detect(timestr, ":") ~ lubridate::ms(timestr),
                                     timestr == "-" ~ lubridate::as.period(NA),
                                     TRUE ~ lubridate::seconds(0))) %>% pull(time),
                          
                          rider = read_html(url) %>% 
                            html_nodes("div:nth-child(3) td:nth-child(5) a") %>%
                            html_text() %>% str_trim(),
                          
                          age = read_html(url) %>% 
                            html_nodes("div:nth-child(3) td:nth-child(6)") %>% 
                            html_text() %>% as.numeric(),
                          
                          team = read_html(url) %>% 
                            html_nodes("div:nth-child(3) td:nth-child(5) span.teammob") %>% 
                            html_text(),
                          
                          points = read_html(url) %>% 
                            html_nodes("div:nth-child(3) td:nth-child(9)") %>% 
                            html_text() %>%
                            as.numeric(),
                          
                          elapsed = case_when(
                            time == first(time) ~ time[1],
                            TRUE ~ time[1] + time) %>%
                            # When adding up time we might get output above 60 seconds.
                            # In order to roll seconds above 60 over to minutes, and
                            # minutes above 60 to hours, we will use the roll argument
                            # in the hms function (notice that hms takes
                            # character or numeric vectors and input with the specified
                            # number of hours, minutes, and seconds. Hence the 
                            # as.character conversion). 
                            {if_else(lubridate::hour(.) == 0, 
                                     str_c("0H ", .), 
                                     as.character(.))} %>%
                            lubridate::hms(roll = TRUE),
                          
                          bib_number = read_html(url) %>% 
                            html_nodes("div:nth-child(3) td:nth-child(4)") %>% 
                            html_text() %>% 
                            {if_else(. == "1", TRUE, NA)})
  
  return(stage_results)
}



# Urls for first five stages
urltest <- glue::glue("https://www.procyclingstats.com/race/tour-de-france/2020/stage-{1:5}")

# Test function on the first 5 stages
purrr::map_dfr(urltest, get_results) %>% View("testFunction")
## The output looks like what we want


# Provide the urls we want to scrape
urls <- glue::glue("https://www.procyclingstats.com/race/tour-de-france/2020/stage-{1:21}")


# Creating an empty list
stage_results <- list()


# Adding elements to avoid bot detection and apply the scrape function on all urls
lapply(seq_along(urls), function(i){
  
  # Message telling which page is being scraped
  message("Getting page ", i, " of ", length(urls))
  # Take a three seconds break between each page
  Sys.sleep(3) 
  
  # Take an extra break after every third page, and print "Taking a break..." on your console
  if ((i %% 3) == 0) {
    message("Taking a break...")
    Sys.sleep(2)  # Take an additional two seconds break
    }
  
  # Apply scrape function and save the results to a list
  stage_results[[i]] <- get_results(url = urls[[i]])
  
  }) %>% 
  
  # Bind elements of the list into a dataframe
  bind_rows(stage_results) -> stage_data2020


# Save dataframe as .rds
saveRDS(stage_data2020, "stage_data2020.rds")



