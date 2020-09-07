#####################################################
#
# Shiny app
# Tour de France plots to consider
#
#####################################################

# Calling on libraries
library(shiny)
library(ggplot2)
library(stringr)
library(forcats)
library(dplyr)
library(treemapify)
library(gganimate)
library(gifski)


# Loading the Tour de France data
tdf_winners <- readr::read_csv(paste('https://raw.githubusercontent.com/',
                                     'rfordatascience/tidytuesday/master/',
                                     'data/2020/2020-04-07/tdf_winners.csv', sep = ""))

stage_data <- readr::read_csv(paste('https://raw.githubusercontent.com/',
                                    'rfordatascience/tidytuesday/master/',
                                    'data/2020/2020-04-07/stage_data.csv', sep = ""))

tdf_stages <- readr::read_csv(paste('https://raw.githubusercontent.com/',
                                    'rfordatascience/tidytuesday/master/',
                                    'data/2020/2020-04-07/tdf_stages.csv', sep = ""))


# Load tdf stages data for 2018 and 2019.
# Convert Stage column to character and Date column to date using col_types, 
# to comply with tdf_stages
tdf_stages2018_2019 <- readr::read_csv2("tdf_stages2018-2019.csv", 
                                        col_types = readr::cols(
                                          Stage = readr::col_character(),
                                          Date = readr::col_date("%d.%m.%Y")))


# Update tdf_stages with data for 2018 and 2019
tdf_stages1903_2019 <- bind_rows(tdf_stages2018_2019, tdf_stages)


# Check that the data from 1903-2017 is identical
identical(tdf_stages, tdf_stages1903_2019[43:nrow(tdf_stages1903_2019), ])
## FALSE
all.equal(tdf_stages, tdf_stages1903_2019[43:nrow(tdf_stages1903_2019), ])
## [1] "Attributes: < Length mismatch: comparison on first 2 components >"                   
## [2] "Attributes: < Component “class”: Lengths (4, 3) differ (string compare on first 3) >"
## [3] "Attributes: < Component “class”: 3 string mismatches >"

## Some attributes of the dataframes are slightly different. The "spec" component 
## shows that the date formats of the 'Date' column are not identical. 
## (But both columns have class "Date")

## In addition the "class" component shows that a class have been dropped in the 
## tdf_stages1903_2019 subset (this happens when subsetting using bracket notation).
## The tdf_stages1903_2019 subset from 1903 to 2017 does not include the "spec_tbl_df" class.

## However, the content in the dataframes are the same as shown here:
identical(tdf_stages[, 1:8], tdf_stages1903_2019[43:nrow(tdf_stages1903_2019), 1:8])
## TRUE



# Number of stages per year including a column with number of split stages 
# per year (stage numbers that ends with a letter)
stage_data %>% 
  group_by(year) %>%
  summarise(stages = n_distinct(stage_results_id),
            # Counting unique values when the ending of the stage number variable
            # is a letter. This is done by subsetting the stage_results_id variable, into
            # TRUE values based on whether the stage number ends with a letter.
            split_stages = n_distinct(stage_results_id[
              str_ends(stage_results_id, pattern = "[[:alpha:]]")]) / 2) %>%
  # Dividing by 2 because all split stages are denoted by 'a' and 'b', and
  # I only want to count a pair of 'a' and 'b' as one split stage.
  ungroup() %>% 
  # Add a variable for grouping in ggplot, in order to make the years
  # when Tour de France was stopped because of WW1 and WW2 as blank lines
  mutate(period = case_when(year <= 1914  ~ "1903-1914",
                            between(year, 1919, 1939) ~ "1919-1939",
                            TRUE ~ "1947-2019")) %>%
  
  # Plot the output
  ggplot(aes(x = year)) + 
  geom_line(aes(y = stages, group = period), colour = "dodgerblue3") + 
  geom_col(aes(y = split_stages), fill = "dodgerblue3") + 
  labs(title = "Number of stages for each edition of Tour de France 1903 - 2019",
       x = "", y = "Number of stages", 
       caption = paste("Between 1934 and 1985, some stages were split in two.", 
                       "Often into a standard mass-start stage and\ntime trial stage.",
                       "The split stages explain some of the spikes in the total number of",
                       "stages in the line chart.", sep = " ")) + 
  annotate(geom = "label", x = 1953, y = 11, 
           label = "Split stages", 
           fontface = "bold", size = 5, label.size = 0.2) + 
  theme_light() + 
  # Left align caption
  theme(plot.caption = element_text(hjust = 0))


# Create a data frame with the years for the min and max values for 
# distance (after 1905) to be used in a line plot of distance
min_max_dist <- tdf_winners %>% 
  filter(lubridate::year(start_date) > 1905) %>% 
  slice(which.min(distance), which.max(distance)) %>% 
  select(start_date, distance) %>% 
  mutate(period = case_when(lubridate::year(start_date) <= 1914 ~ "1903-194", 
                            between(lubridate::year(start_date), 1919, 1939) ~ "1919-1939", 
                            TRUE ~ "1947-2019"))

# Plot of total distance per year
tdf_winners %>% 
  # Adding a variable to create gaps in the plot for displaying the pause during
  # WW1 and WW2. This variable is used with group argument in ggplot.
  mutate(period = case_when(lubridate::year(start_date) <= 1914 ~ "1903-1914", 
                            between(lubridate::year(start_date), 1919, 1939) ~ "1919-1939", 
                            TRUE ~ "1947-2019")) %>% 
  # Plot
  ggplot(aes(x = start_date, y = distance, group = period)) + 
  geom_line(colour = "dodgerblue3") +
  geom_point(data = min_max_dist, aes(x = start_date, y = distance), 
             colour = "goldenrod", alpha = 0.9, size = 1) + 
  geom_text(data = min_max_dist, aes(label = lubridate::year(start_date)), 
            size = 3, hjust = -0.1) +
  labs(title = "Total tour distance for every edition of Tour de France 1903 - 2019", 
       x = NULL, y = "Distance (km)") + 
  theme_light()


# Total number of races from 1903 - 2019
stage_data %>% distinct(year, stage_results_id) %>% count(name = "number_of_races")


# Overall time used by the winner
tdf_winners %>% ggplot(aes(x = start_date, y = time_overall)) + 
  geom_line(colour = "lightsalmon") +
  labs(title = "Time used to complete the tour for all editions of Tour de France 1903 - 2019",
       x = NULL, y = "Time (hours)") +
  theme_light()

## Suggestion make this plot, with y variable as selectable input

# Average speed of the overall winner
tdf_winners %>% 
  mutate(speed = distance / time_overall) %>% 
  
  ggplot(aes(x = start_date, y = speed)) + 
  geom_line(colour = "lightseagreen") +
  labs(title = "Average speed of the winner for all editions of Tour de France 1903 - 2019",
       x = NULL, y = "Speed (km / h)")


# Finding number of riders who started at 'stage-0', 'stage-1' and 'stage-1a' for each edition
stage_data %>% 
  group_by(year) %>% 
  summarise(stage0 = sum(stage_results_id == "stage-0"), 
            stage1 = sum(stage_results_id == "stage-1"), 
            stage1a = sum(stage_results_id == "stage-1a"),
            nstarted = pmax(stage0, stage1, stage1a)) %>% 
  ungroup() %>% View(title = "n_started")


# Overview of what was the first stage using the tdf_stages dataframe
tdf_stages %>% 
  group_by(year = lubridate::year(Date)) %>% 
  summarise(StageP = sum(Stage == "P"),
            Stage1 = sum(Stage == "1"),
            Stage1a = sum(Stage == "1a")) %>% 
  ungroup() %>% View(title = "FirstStageCounts")


# Data frame with riders at each editions first stage
# (Putting exceptions at the top of the case_when statement, where either 'stage-0' 
#   were unofficial (1988) or stage-0 was a team time trial (1971).  
#   In 1955, 1960, 1961 and 1965 the first stage was labelled 'stage-1a')
stage_data %>% 
  group_by(year) %>% 
  filter(case_when(year == 1988 ~ stage_results_id == "stage-1",
                   year %in% c(1955, 1960, 1961, 1965, 1971) ~ stage_results_id == "stage-1a",
                   
                   "stage-0" %in% stage_results_id ~ stage_results_id == "stage-0",
                   TRUE ~ stage_results_id == "stage-1")) -> racers_at_startline

## Using case_when, you can vectorise multiple if_else() statements.


# Count the number of starting riders for each edition
startlists <- racers_at_startline %>% count(year, name = "Riders_starting")



# Finding how many riders who completed each edition
nCompleted <- stage_data %>%  
  # Extract string after "stage-" in stage_results_id variable, 
  # to make it easier to apply numeric function max() to the data
  mutate(extract = str_extract(stage_results_id, pattern = "\\b\\w+$")) %>% 
  group_by(year) %>%
  # Using filter to only look at the last stage of each edition. 
  # For stages that are labelled with 'a' or 'b' (split stages, for example '20b'),
  # I remove the letter before applying the max function.
  filter(str_remove(extract, "[:alpha:]") == 
           max(as.numeric(str_remove(extract, "[:alpha:]")))) %>%
  # Create one record for each edition, with year, stage_results_id and number of
  # riders completed
  summarise(stage_results_id = last(stage_results_id),
            riders_completed = max(as.numeric(rank), na.rm = TRUE)) %>% 
  ungroup() 
  
  

# Plot percentage of riders who completed the tour
nCompleted %>% 
  mutate(percentage = riders_completed / startlists$Riders_starting) %>% 
  
  ggplot(aes(x = year, y = percentage)) + 
  geom_line(colour = "lightcyan3") + 
  labs(title = "Percentage of riders who completed the race", 
       x = NULL, y = "Percentage completed") + 
  scale_y_continuous(labels = scales::percent) + 
  theme_light()


# Crete a dataframe of last stages
last_stages <- tdf_stages1903_2019 %>% 
  group_by(year = lubridate::year(Date)) %>% 
  # Keep the last record from each tour
  summarise_all(., last) %>% 
  ungroup()
## There are some stages missing in the stage_data dataframe (last stages)


# Number of riders that did not finish the race
dropouts <- stage_data %>% 
  group_by(year) %>% 
  summarise(DNS = sum(rank == "DNS"), 
            DNF = sum(rank == "DNF"), 
            OTL = sum(rank == "OTL"),
            DSQ = sum(rank == "DSQ"),
            NQ = sum(rank == "NQ")) %>% 
  ungroup()

# Plotting dropouts
dropouts %>% ggplot(aes(x = year, y = rowSums(dropouts[, 2:5]))) + 
  geom_line(colour = "darkorchid4") + 
  labs(title = "Number of racers that did not complete the race",
       x = NULL, y = "Number of riders")

#Plot with dropouts split by category and as a percentage of number of riders at the startline
dropouts %>% 
  # Add number of riders that started
  mutate(nstarted = startlists$Riders_starting) %>% 
  # Convering the data to long format
  tidyr::pivot_longer(cols = DNS:NQ, names_to = "status", values_to = "total") %>% 
  
  # Create the plot
  ggplot(aes(x = year, y = total / nstarted)) +
  geom_line(aes(colour = status), show.legend = FALSE) +
  labs(title = "Percentage of riders that did not complete the race",
       x = NULL, y = "Percentage of riders") + 
  facet_grid(~status) + 
  scale_y_continuous(labels = scales::percent) + 
  theme_light()


# Plot proportion of riders that did not finish the tour
stage_data %>% 
  filter(rank %in% c("DNS", "DNF", "OTL", "DSQ", "NQ")) %>% 
  ggplot(aes(x = year, y = rank)) + 
  geom_count(aes(size = after_stat(prop), group = 1), colour = "darkred") + 
  labs(title = "Proportion of riders that did not finish the tour", 
       x = NULL, y = "Criteria") + 
  scale_size_area(name = "Prop", max_size = 8,
                  # Set legend as percentage and use one decimal place
                  labels = scales::label_percent(accuracy = 0.1)) + 
  theme_light()


# Create data frame with start cities (Grands Départs)
stage1 <- tdf_stages1903_2019 %>% 
  group_by(lubridate::year(Date)) %>% 
  filter(case_when(
    lubridate::year(Date) %in% c(1955, 1960, 1961, 1965, 1967, 1968) ~ Stage == "1a",
    "P" %in% Stage ~ Stage == "P", 
    TRUE ~ Stage == "1"))

### First attempt, using if statements inside filter
# stage1 <- tdf_stages1903_2019 %>% 
#   group_by(lubridate::year(Date)) %>% 
#   filter(if (lubridate::year(Date) %in% c(1955, 1960, 1961, 1965, 1967, 1968)) Stage == "1a" 
#          else if ("P" %in% Stage) Stage == "P" 
#          else Stage == "1")


# Grands Départs outside of France
intGD <- stage1 %>% filter(lubridate::year(Date) %in% 
                                          c(1954, 1958, 1965, 1973,
                                            1975, 1978, 1980, 1982,
                                            1987, 1989, 1992, 1996,
                                            1998, 2002, 2004, 2007,
                                            2009, 2010, 2012, 2014,
                                            2015, 2017, 2019))

# Plot Grands Départs outside of France
intGD %>% count(Origin, sort = TRUE) %>% 
  
  ggplot(aes(x = fct_rev(Origin), y = n)) + 
  geom_col() + 
  scale_y_continuous(breaks = function(x) pretty(x, n = 3)) + 
  labs(title = "Tour de France starts outside of France", x = "Start city", y = NULL) + 
  coord_flip()

# Including French Grands Départs with more than one start
stage1 %>% ungroup() %>% 
  count(Origin, sort = TRUE) %>% ungroup() %>%
  mutate(France = ifelse(Origin %in% intGD$Origin, FALSE, TRUE)) %>% 
  filter(n > 1 | France == FALSE) %>% 
  
  ggplot(aes(x = fct_reorder(fct_rev(Origin), n, max), y = n)) + 
  geom_col(aes(fill = France)) + 
  labs(title = "Tour de France starts", x = "Start city", y = "Number of starts") + 
  scale_fill_manual(name = "Starts", labels = c("International", "France"), 
                    values = c("lightblue4", "lightblue")) + 
  coord_flip()



# Data frame where international cities hosted the Grand Départ,
# (i.e., the tours first stage). Creating a column whichYear, with (<year1>, <year2>).
intGD %>% ungroup() %>% 
  group_by(Origin) %>% 
  summarise(n = n(), 
            whichYear = paste("(", toString(sort(`lubridate::year(Date)`)), ")", 
                              sep = "")) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% head()


# Youngest stage winners per edition 1903 - 2019
youngest_stage_winner <- stage_data %>% 
  filter(as.numeric(rank) == 1) %>% 
  group_by(year) %>% 
  slice(which(age == min(age, na.rm = TRUE))) %>% 
  ungroup()


# Plot the number of days in yellow jersey per tdf_winner
tdf_winners %>% 
  group_by(decade = (lubridate::year(start_date) %/% 10) * 10) %>% 
  summarise(avg_stages_led = mean(stages_led)) %>% 
  ungroup() %>% 
  
  ggplot(aes(x = decade, y = avg_stages_led)) + 
  geom_line() + 
  labs(title = "Number of days in yellow jersey for Tour winner", 
       x = NULL, y = "Days as race leader") +
  expand_limits(y = 0) +
  theme_light()
## No trend


# Plot stage wins per tdf_winner
tdf_winners %>% 
  group_by(decade = (lubridate::year(start_date) %/% 10) * 10) %>% 
  summarise(avg_stage_wins = mean(stage_wins)) %>% 
  ungroup() %>% 
  
  ggplot(aes(x = decade, y = avg_stage_wins)) + 
  geom_line() +
  labs(title = "Number of stage wins for Tour winner", 
       x = NULL, y = "Average number of stage wins")


# Riders with most stage wins per year
tdf_stages1903_2019 %>% 
  group_by(year = lubridate::year(Date)) %>% 
  count(Winner) %>% 
  slice(which(n == max(n))) %>% 
  View("Most stage wins")
## In case of several riders with equal number of wins for a specific year,
## we can use  'slice(which.max(n))' for just the first occurrence.


# Cleaning up the stage type variable, using case_when
tdf_stages1903_2019 <- tdf_stages1903_2019 %>% 
  mutate(Category = case_when(
    Type %in% c("Plain stage", "Flat stage", "Flat Stage",
                "Flat cobblestone stage", "Plain stage with cobblestones") ~ "Flat stage",
    
    Type %in% c("Stage with mountain(s)", "Mountain stage", 
                "High mountain stage", "Medium mountain stage", 
                "Mountain Stage", "Stage with mountain") ~ "Mountain stage",
    
    Type %in% "Hilly stage" ~ "Hilly stage",
    
    Type %in% c("Individual time trial", "Mountain time trial") ~ "Individual time trial",
    
    Type %in% "Team time trial" ~ "Team time trial",
    
    Type %in% "Half Stage" ~ "Flat stage",
    
    Type %in% "Transition stage" ~ ifelse(Date == as.Date("2008-07-10"),
                                          "Mountain stage", "Hilly stage"),
    Type %in% "Intermediate stage" ~ ifelse(Date == as.Date("2007-07-20"),
                                            "Flat stage", "Hilly stage")))


# Recode values in the 'Winner_Country' column
### URS is Soviet Union, GDR is East Germany, FRG is West Germany
# tdf_stages1903_2019 %>% 
#   mutate(Winner_Country = recode(Winner_Country, URS = "RUS")) -> Collapsetest
# Collapsetest %>% filter(Winner_Country == "RUS") %>% count(Winner_Country)


# Plot with most winning countries per stage type
# Showing the five countries with most stage wins, France, Belgium, Italy, 
# the Netherlands and Spain. Other countries are merged into the labet "Other". 
tdf_stages1903_2019 %>%
  filter(!is.na(Winner_Country)) %>% 
  # Using fct_infreq to sort from most frequent to least frequent, and then using
  # fct_lump with n = 5 to merge less frequent countries into "Other", for a more 
  # readable plot
  mutate(Winner_Country = fct_lump(fct_infreq(Winner_Country), n = 5)) %>% 
  group_by(Category, Winner_Country) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(Category, desc(n)) %>% 
  # Set Category variable to ordered factor in order to control the order of
  # the panels in facet_grid.
  mutate(Category = factor(Category, 
                           levels = c("Flat stage", "Hilly stage", 
                                      "Mountain stage", "Individual time trial",
                                      "Team time trial"))) %>% 
  
  ggplot(aes(x = Winner_Country, y = n)) + 
  geom_col(aes(fill = Category), position = "dodge", colour = "grey") + 
  labs(title = "Most winning countries per stage type", 
       x = "Country", y = "Number of stage wins") + 
  theme_light()



# Plot of tour distance + speed of winner
tdf_winners %>% 
  mutate(speed = distance / time_overall) %>% 
  
  ggplot(aes(x = start_date)) + 
  geom_line(aes(y = distance, colour = "Distance")) + 
  geom_line(aes(y = speed*100, colour = "Speed")) +  
  labs(title = "Tour distance for each edition 1903-2021",  
       x = NULL, y = "Distance travelled (km)") + 
  scale_y_continuous(sec.axis = sec_axis(~./100, 
                                         name = "Speed of tour winner (km/h)")) + 
  scale_colour_manual(values = c("darkslateblue", "darksalmon"), name = NULL)


# Plot of nationalities of tour winners, with panels for editions before and after 1980
tdf_winners %>% 
  mutate(era = ifelse(edition <= 66, 
                      "Before 1980", "From 1980 and after"),
         # Using fct_lump to group nationalities with 5 or less tour wins into an
         # 'Other' category
         nationality = fct_lump(fct_infreq(nationality), n = 6)) %>% 
  group_by(nationality, era) %>% 
  summarise(n = n()) %>% 
  
  ggplot(aes(x = nationality, y = n)) + 
  geom_col() + 
  labs(title = "Nationalities of tour winners", 
       x = "Nationality", y = "Number of tour wins") + 
  facet_grid(era ~ .) + theme_light()


# Horizontal barplot of stage winners with 10 or more wins
tdf_stages1903_2019 %>% 
  group_by(Winner) %>% filter(n() >= 10) %>% 
  ungroup() %>% 
  
  ggplot(aes(y = fct_rev(fct_infreq(Winner)))) + 
  geom_bar(aes(fill = (as.numeric(fct_rev(fct_infreq(Winner))) %% 2 == 0))) + 
  labs(title = "Riders with most stage wins", 
       x = "Number of stage wins", y = "Rider") + 
  scale_fill_manual(guide = "none", values = c("grey35", "lightcyan3")) + 
  theme_light()


# Riders with most starts
stage_data %>% 
  group_by(rider) %>% 
  summarise(starts = length(unique(year)), 
            finishes = starts - length(unique(year[
              # Conditional filter of rank that does contain DNS, DNF, DSQ, NQ or OTL
              str_detect(rank, pattern = "DNS|DNF|DSQ|NQ|OTL")])), 
            first = first(year), 
            last = last(year)) %>% 
  ungroup() %>% 
  arrange(desc(starts), desc(finishes)) %>% 
  View("Most starts")


# Iconic climbs
tdf_stages1903_2019 %>% 
  tidyr::pivot_longer(cols = Origin:Destination, 
                      names_to = "host", 
                      values_to = "place") %>% 
  filter(str_detect(place, "Alpe|Col |Luz|Val |Val-|Ventoux")) %>% 
  View("Climbs")


# Most points given completion of the Tour
stage_data %>% 
  group_by(year, rider) %>% 
  # Points classification started in 1953.
  # Only include riders that completed the tour. We know that after 1953 there 
  # have been at least 21 stages per edition
  filter(year >= 1953 & n() >= 21) %>% 
  summarise(points = sum(points, na.rm = TRUE), 
            completed = last(stage_results_id)) %>%
  # Only select the top 5 riders
  slice_max(points, n = 5) %>% 
  # Add a column for which rank from 1 to 5.
  # For tie on number of points for 5th place, select 5 as rank
  mutate(rank = 1:n(), .after = year, 
         rank = pmin(rank, 5)) %>% 
  ungroup()


# Countries with most stage wins
tdf_stages1903_2019 %>%
  filter(!is.na(Winner_Country)) %>%
  group_by(Winner_Country = fct_lump_n(Winner_Country, n = 10)) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(percentage_wins = prop.table(n)) %>% 

# Plot
ggplot(aes(x = n, 
           y = fct_relevel(reorder(Winner_Country, n),
                           "Other", after = 0))) + 
  geom_col(aes(fill = n), show.legend = FALSE) + 
  scale_fill_gradient(low = "#4692CD", high = "#132B43") +
  labs(title = "Countries with most stage wins", 
       x = "Number of stages won", y = "Country") + 
  geom_text(aes(label = paste(round(percentage_wins, digits = 2)*100, "%")), 
            hjust = -0.2, colour = "#01452c") + 
  expand_limits(x = 725) + 
  theme_light()


# Draft for treemap. Aspects to fix: 

tdf_stages1903_2019 %>% 
  filter(!is.na(Winner_Country)) %>% 
  # Merge winners for stages with shared wins (eg. c("FRA", "FRA"))
  mutate(Winner_Country = case_when(
    Winner_Country == "c(\"FRA\", \"FRA\")" ~ "FRA",
    Winner_Country == "c(\"BEL\", \"BEL\")" ~ "BEL",
    Winner_Country == "c(\"BEL\", \"GER\")" ~ "BEL/GER",
    TRUE ~ Winner_Country)) %>% 
  group_by(Winner_Country, Category) %>% 
  summarise(Wins = n()) %>% ungroup() %>% 
  # Alternative - Creating a variable for percentage of stage wins,
  #               to be used for the colour scale 
  #group_by(Category) %>% 
  #mutate(percentage = prop.table(Wins)) %>% View("inspect2")
  
  ggplot(aes(area = Wins, fill = Wins, subgroup = Category), start = "topleft") + 
  geom_treemap(start = "topleft") + 
  geom_treemap_subgroup_border(start = "topleft", colour = "cornsilk3") + 
  geom_treemap_subgroup_text(place = "centre", alpha = 0.8, 
                             colour = "cornsilk3", fontface = "italic", 
                             min.size = 0, reflow = TRUE, start = "topleft") + 
  geom_treemap_text(aes(label = Winner_Country), colour = "gray91", start = "topleft") + 
  scale_fill_continuous(high = "#132B43", low = "#56B1F7", name = "# of wins") +
  labs(title = "Treemap of number of stage wins per country")



# Duration of the tour
lengthDays <- tdf_stages1903_2019 %>% 
  group_by(year = lubridate::year(Date)) %>% 
  summarise(firstStage = first(Stage), 
            start = first(Date), 
            lastStage = last(Stage), 
            finish = last(Date), 
            duration = (finish - start) + 1) %>% 
  ungroup()

# Plot the number of days for each tour and number of stages
lengthDays %>% 
  inner_join(stage_data %>%
               group_by(year) %>%
               summarise(stages = n_distinct(stage_results_id),
                         split_stages = n_distinct(stage_results_id[
                           # where string ends with a character and dividing by two
                           # because all split stages are denoted by 'a' and 'b', and
                           # I only want to count a pair of 'a' and 'b' as 
                           # one split stage
                           str_ends(stage_results_id, pattern = "[[:alpha:]]")]) / 2) %>%
               ungroup(),
             # join by year column
             by = "year") %>% 
  
  mutate(period = case_when(year <= 1914 ~ "1903-1914", 
                            between(year, 1919, 1939) ~ "1919-1939",
                            TRUE ~ "1947-2019")) %>% 
  
  ggplot(aes(x = year, group = period)) + 
  geom_line(aes(y = duration, colour = "Days")) + 
  geom_line(aes(y = stages, colour = "Stages")) + 
  scale_colour_manual(name = " Number of", 
                      values = c("deepskyblue4", "darkorange")) +
  labs(title = "Duration of the Tour de France editions ", 
       x = NULL, y = "Number of days and stages") + 
  theme_light()


# Get the 10 riders with most points in 2017
selection2017 <- stage_data %>%
  filter(year == 2017) %>% 
  group_by(rider) %>% 
  summarise(total_points = sum(points, na.rm = TRUE)) %>% 
  ungroup() %>% 
  slice_max(total_points, n = 10) %>% pull(rider)


# Animation of points, adding up per stage, for the riders with most points in 2017 
points_anim <- stage_data %>% 
  filter(year == 2017 & 
           rider %in% selection2017) %>% 
  group_by(rider) %>% 
  mutate(stage = as.integer(str_extract(stage_results_id, "[[:digit:]]+")), 
         points = coalesce(points, 0), 
         cumulative_points = cumsum(points)) %>% 
  group_by(stage) %>% 
  mutate(ordering = rank(-cumulative_points, ties.method = "first") * 1) %>% 
  ungroup() %>% 
  
  ggplot(aes(x = ordering, group = rider)) + 
  geom_tile(aes(y = cumulative_points/2, height = cumulative_points, 
                width = 0.9, fill = rider), alpha = 0.9, show.legend = FALSE) + 
  geom_text(aes(y = 0, label = paste(rider, " ")), vjust = 0.2, hjust = 1) + 
  coord_flip(clip = "off", expand = FALSE) + 
  scale_x_reverse() +
  labs(title =  "Tour de France 2017 - Stage: {closest_state}", 
       x = NULL, y = "Points earned") + 
  scale_fill_manual(values = c("#6D7696", "#59484F", "#455C4F", "#8A6E64",
                               "#803018", "#CC5543", "#E87F60", "#EDB579", 
                               "#E3CCA1", "#DBE6AF")) +
  theme_light() + 
  theme(axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), 
        plot.margin = margin(1, 1, 1, 4, "cm")) + 
  transition_states(stage, transition_length = 4, state_length = 1) + 
  ease_aes('cubic-in-out')

animate(points_anim, fps = 25, duration = 20, 
        width = 620, height = 450, renderer = gifski_renderer())
 


# Creating a map?

## Get the data for ggplot
#### Requires the maps package
FranceMap <- map_data("world", c("France", "Spain", "Switzerland", "Belgium", "Germany"))

## Set the longitude and latitude for the country labels approximately in 
## the middle of the country, using the mean
countryLabels <- FranceMap %>% 
  group_by(region) %>% 
  summarise(long = mean(long), 
            lat = mean(lat)) %>% 
  ungroup()

## Plot
ggplot(FranceMap, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group, fill = region), show.legend = FALSE) + 
  geom_text(data = countryLabels, aes(label = region)) + 
  coord_fixed(1.3) + 
  theme_void()


# Horizontal stacked barplot of host cities
## First, find the 10 most visited cities.
## This can be done by converting the Origin and Destination columns to long format 
## and count the number of total visits
tdf_stages1903_2019 %>% 
  tidyr::pivot_longer(cols = Origin:Destination, 
                      names_to = "Host", values_to = "City") %>% 
  count(City, sort = TRUE, name = "visits") %>% 
  ungroup() %>%
  # Slice the 10 most visited host cities
  slice_max(visits, n = 10) %>% 
   
  ### Now we have the 10 most visited cities, but we would like to add
  ### how many times each city hosted starts and finishes
  
  # Concatenate number of times as a start city
  inner_join(tdf_stages1903_2019 %>% 
               count(Origin, sort = TRUE, name = "start") %>% 
               ungroup() %>% 
               slice_max(start, n = 10), by = c("City" = "Origin")) %>% 
  
  # Concatenate number of times the city hosted the finish
  inner_join(tdf_stages1903_2019 %>% 
               count(Destination, sort = TRUE, name = "finish") %>% 
               ungroup() %>% 
               slice_max(finish, n = 10), by = c("City" = "Destination")) %>%
  
  # Remove the total number of visits and convert the data to long format
  select(-visits) %>% 
  tidyr::pivot_longer(cols = start:finish, 
                      names_to = "Host", values_to = "Visits") %>% 
  
  # Plot as a horizontal stacked barplot
  ggplot(aes(x = Visits, y = reorder(City, Visits))) + 
  geom_col(aes(fill = Host)) + 
  labs(title = "Cities that have hosted most stage starts and finishes", 
       x = "Number of times as a host city", y = "City") + 
  scale_fill_manual(values = c("#627083", "#92A1B8"), 
                    guide = guide_legend(reverse = TRUE)) + 
  theme_light()


# Number of stages for different stage types per decade
tdf_stages1903_2019 %>% 
  group_by(decade = (lubridate::year(Date) %/% 10) * 10, 
           Category) %>% 
  summarise(n_stages = n()) %>% 
  ungroup() %>% 
  
  # Convert to wide format for easy comparison within decades
  tidyr::pivot_wider(names_from = Category, 
                     values_from = n_stages) %>% 
  rename_with(~str_replace_all(., pattern = " ", replacement = "_"), 
              .cols = ends_with(c("stage", "trial"))) %>% View("stageTypeDistribution")
