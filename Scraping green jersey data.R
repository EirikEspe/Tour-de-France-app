#####################################################
#
# Shiny app
# Scraping data about the green jersey
#
#####################################################

# Load packages
library(rvest)
library(stringr)
library(dplyr)


# Urls we want to scrape for points per stage in 2020 edition
points_url <- glue::glue(
  "https://www.procyclingstats.com/race/tour-de-france/2020/stage-{1:20}-points")
## Add url for points of stage 21
points_url <- append(points_url, "https://www.procyclingstats.com/race/tour-de-france/2020/points")


# Create a scraping function for points classification in Tour de France 2020
green_jersey <- function(url) {
  green_jersey <- tibble(edition = 107,
                         
                         year = read_html(url) %>% 
                           html_nodes("span.yearmob") %>% 
                           html_text() %>% as.numeric(),
                         
                         stage_results_id = if_else(str_detect(url, "stage"), 
                                                    str_extract(url, "stage-\\d+"), "stage-21"),
                         
                         rank = read_html(url) %>% 
                           html_nodes("div:nth-child(5) td:nth-child(1)") %>% 
                           html_text() %>% as.numeric(),
                         
                         rider = read_html(url) %>% 
                           html_nodes(if_else(url == points_url[1],
                                              "div:nth-child(5) td:nth-child(3) a", 
                                              "div:nth-child(5) td:nth-child(5) a")) %>%
                           html_text() %>% str_trim(),
                         
                         age = read_html(url) %>% 
                           html_nodes(if_else(url == points_url[1], 
                                              "div:nth-child(5) td:nth-child(4)", 
                                              "div:nth-child(5) td:nth-child(6)")) %>% 
                           html_text() %>% as.numeric(),
                         
                         team = read_html(url) %>% 
                           html_nodes(if_else(url == points_url[1], 
                                              "div:nth-child(5) td:nth-child(3) span.teammob", 
                                              "div:nth-child(5) td:nth-child(5) span.teammob")) %>% 
                           html_text(),
                         
                         points = read_html(url) %>% 
                           html_nodes(case_when(
                             url == points_url[1] ~ "div:nth-child(5) td:nth-child(7)", 
                             url == points_url[2:20] ~ "div:nth-child(5) td:nth-child(9)",
                             url == points_url[21] ~ "div:nth-child(5) td:nth-child(11)")) %>% 
                           html_text() %>% as.numeric())
  
  return(green_jersey)
  }


# Creating an empty list
points_list <- list()


# Adding elements to avoid bot detection and apply the scrape function on all urls
lapply(seq_along(points_url), function(i){
  
  # Message telling which page is being scraped
  message("Getting page ", i, " of ", length(points_url))
  # Take a three seconds break between each page
  Sys.sleep(3) 
  
  # Take an extra break after every third page, and print "Taking a break..." on your console
  if ((i %% 3) == 0) {
    message("Taking a break...")
    Sys.sleep(2)  # Take an additional two seconds break
  }
  
  # Apply scrape function and save the results to a list
  points_list[[i]] <- green_jersey(url = points_url[[i]])
  
}) %>% 
  
  # Bind elements of the list into a dataframe
  bind_rows(points_list) -> green_jersey2020



# Read vector with urls for points per stage from 2000 to 2020
## (urls with points for each stage between 2000 and 2020)
points_url <- readRDS("url_vector.rds")


# Create a scraping function for points classification 2000-2020
points_classification <- function(url) {
  points <- tibble(edition = read_html(url) %>% 
                     html_nodes("span.yearmob") %>% 
                     html_text() %>% as.numeric() - 1903 - 10,
                   
                   year = read_html(url) %>% 
                           html_nodes("span.yearmob") %>% 
                           html_text() %>% as.numeric(),
                   
                   stage_results_id = case_when(str_detect(url, "prologue") ~ "prologue", 
                                                str_detect(url, "stage") ~ 
                                                  str_extract(url, "stage-\\d+"), 
                                                str_detect(url, "200(1|2|3|4|6|7)|2010|2012") ~ 
                                                  "stage-20",
                                                TRUE ~ "stage-21"),
                   
                   rank = read_html(url) %>% 
                     html_nodes("div:nth-child(5) td:nth-child(1)") %>% 
                     html_text() %>% as.numeric(),
                   
                   rider = read_html(url) %>% 
                     html_nodes(if_else(url %in% points_url[c(1, 22, 43, 64, 85, 106, 
                                                              127, 148, 169, 190, 211,
                                                              232, 253, 274, 295, 316,
                                                              337, 358, 379, 400, 421)],
                                        "div:nth-child(5) td:nth-child(3) a", 
                                        "div:nth-child(5) td:nth-child(5) a")) %>%
                     html_text() %>% str_trim(),
                   
                   age = read_html(url) %>% 
                     html_nodes(if_else(url %in% points_url[c(1, 22, 43, 64, 85, 106, 
                                                              127, 148, 169, 190, 211,
                                                              232, 253, 274, 295, 316,
                                                              337, 358, 379, 400, 421)], 
                                        "div:nth-child(5) td:nth-child(4)", 
                                        "div:nth-child(5) td:nth-child(6)")) %>% 
                     html_text() %>% as.numeric(),
                   
                   team = read_html(url) %>% 
                     html_nodes(if_else(url %in% points_url[c(1, 22, 43, 64, 85, 106, 
                                                              127, 148, 169, 190, 211,
                                                              232, 253, 274, 295, 316,
                                                              337, 358, 379, 400, 421)], 
                                        "div:nth-child(5) td:nth-child(3) span.teammob", 
                                        "div:nth-child(5) td:nth-child(5) span.teammob")) %>% 
                     html_text(),
                   
                   points = read_html(url) %>% 
                     html_nodes(case_when(
                       ## Url indexes for prologue or first stage
                       url %in% points_url[c(1, 22, 43, 64, 85, 106, 
                                             127, 148, 169, 190, 211,
                                             232, 253, 274, 295, 316,
                                             337, 358, 379, 400, 421)] ~ "div:nth-child(5) td:nth-child(7)",
                       ## Url index for 2005 stage 8 (the page did not list any points for this stage)
                       url == points_url[113] ~ "div:nth-child(5) td:nth-child(8)",
                       ## Url indexes for stages between second stage and second last stage
                       url %in% points_url[c(2:20, 23:41, 44:62, 65:83,
                                             86:104, 107:125, 128:146,
                                             149:167, 170:188, 191:209,
                                             212:230, 233:251, 254:272,
                                             275:293, 296:314, 317:335,
                                             338:356, 359:377, 380:398,
                                             401:419, 422:440)] ~ "div:nth-child(5) td:nth-child(9)",
                       ## Url indexes for last stage (2000-2016)
                       url %in% points_url[c(21, 42, 63, 84, 105, 126, 147, 168, 189,
                                             210, 231, 252, 273, 294, 315, 336, 357)] ~ "div:nth-child(5) td:nth-child(10)",
                       ## Url indexes for last stage (2017-2020)
                       url %in% points_url[c(378, 399, 420, 441)] ~ "div:nth-child(5) td:nth-child(11)")) %>% 
                     html_text() %>% as.numeric())
  
  return(points)
}


# Find index of the first stage of each edition from 2000 to 2020
tibble(year = 2000:2020, 
       edition = seq_along(year), 
       stage1_index = lag(edition, default = 0) * 21 + 1) %>% View("indexFirstStage")



# Using lapply to iterate the scraping function on all urls, to collect points per stage. 
# In addition, we add elements to avoid bot detection.
lapply(seq_along(points_url), function(i){
  
  # Message telling which page is being scraped
  message("Getting page ", i, " of ", length(points_url))
  # Take a three seconds break between each page
  Sys.sleep(3) 
  
  # Take an extra break after every third page, and print "Taking a break..." on your console
  if ((i %% 3) == 0) {
    message("Taking a break...")
    Sys.sleep(2)  # Take an additional two seconds break
  }
  
  # Apply scrape function and save the results to a list
  points_list[[i]] <- points_classification(url = points_url[[i]])
  
}) %>% 
  
  # Bind elements of the list into a dataframe
  bind_rows(points_list) -> green_jersey


## Correct points for stage 9 in 2005. The website is listing aggregated points as the 
## stage points for this stage.
### First, we'll find the correct number of points 
points_stage9 <- green_jersey %>% 
  # Filter the year and stage
  filter(year == 2005 & stage_results_id == "stage-9") %>% 
  # Selecting columns and renaming points because this is the aggregate points after this stage
  select(rider, agg_points = points) %>% 
  # Join data for the sum of points for stage 1-7
  left_join(green_jersey %>% 
              filter(year == 2005 & str_detect(stage_results_id, "stage-[1-7]$")) %>% 
              group_by(rider) %>% 
              summarise(sum_points = sum(points, na.rm = TRUE)) %>% 
              ungroup(), by = "rider") %>%
  # Subtract the sum of points for stage 1-7 from the aggregate points
  mutate(points = agg_points - sum_points)


### Next, we're replacing the aggregated points for stage 9 with the calculated points
green_jersey$points[with(green_jersey, year == 2005 & 
                           stage_results_id == "stage-9")] <- points_stage9$points


# Save dataframe as .rds
saveRDS(green_jersey, "points_classification2000_2020.rds")


## In 2011, some riders lost points as a penalty for finishing outside of the time limit.
## This happened on stage 18 and 19, and the riders were deducted 20 points for each stage
### Below you can find a dataframe for total points in 2011 with correction of 26 riders.
green_jersey %>% 
  filter(year == 2011) %>% 
  group_by(rider) %>% 
  summarise(total_points = case_when(
    rider %in% "Cavendish Mark" ~ sum(points, na.rm = TRUE) - 43, 
    rider %in% c("Hushovd Thor", "Greipel André", "Farrar Tyler", 
                 "Delage Mickaël", "Božič Borut", "Hinault Sébastien", 
                 "Petacchi Alessandro", "Martin Tony", "Oss Daniel",
                 "Ventoso Francisco José", "Engoulvent Jimmy", 
                 "Goss Matthew", "Turgot Sébastien", "Ciolek Gerald",
                 "Renshaw Mark", "Marcato Marco") ~ sum(points, na.rm = TRUE) - 40,
    rider %in% c("Rojas José Joaquín", "Gilbert Philippe", "Boasson Hagen Edvald", 
                 "Roy Jérémy", "Casar Sandy", "El Fares Julien", "Costa Rui",
                 "Pérez Rubén", "Devenyns Dries") ~ sum(points, na.rm = TRUE) - 20,
    TRUE ~ sum(points, na.rm = TRUE))) %>% summarise(total_points = first(total_points)) %>%  
  ungroup() %>% arrange(desc(total_points)) %>% View("points")


