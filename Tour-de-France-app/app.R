#####################################################
#
# Shiny app
# Tour de France
#
#####################################################


library(shiny)
library(ggplot2)
library(stringr)
library(forcats)
library(dplyr)
library(shinythemes)
library(treemapify)
library(gganimate)
library(gifski)
library(shinycssloaders)


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


# Creating an alternative function for 'dateRangeInput()', to add the opportunity
# to allow for selecting view mode when picking a date. 
# E.g to allow for the opportunity to just select year
dateRangeInput2 <- function(inputId, label, minview = "days", maxview = "decades", ...) {
    d <- shiny::dateRangeInput(inputId, label, ...)
    d$children[[2L]]$children[[1]]$attribs[["data-date-min-view-mode"]] <- minview
    d$children[[2L]]$children[[3]]$attribs[["data-date-min-view-mode"]] <- minview
    d$children[[2L]]$children[[1]]$attribs[["data-date-max-view-mode"]] <- maxview
    d$children[[2L]]$children[[3]]$attribs[["data-date-max-view-mode"]] <- maxview
    d
}



# Define UI for application
ui <- fluidPage(
    
    theme = shinytheme("sandstone"),

    # Create top level navigation bar, and set the application title 
    navbarPage("Tour de France 1903 - 2019",
               
               
#--- Navbar Winners----  

    # Navbar tab names 'winners'
    tabPanel("Winners",
    # Sidebar with a slider input for number of bins,
    # calendar input for years and drop-down list of countries.         
    sidebarLayout(
        sidebarPanel(
            
            #sliderInput("bins",
            #            "Number of bins:",
            #            min = 1,
            #            max = 50,
            #            value = 30),
            
            dateRangeInput2("year", "Select year",
                            start = min(tdf_winners$start_date),
                            end = max(tdf_winners$start_date),
                            min = min(tdf_winners$start_date),
                            max = max(tdf_winners$start_date) + 1,
                            format = "yyyy",
                            minview = "years",
                            maxview = "decades"),

            
            selectInput("country", "Select country",
                        choices = c("All", sort(str_trim(tdf_winners$nationality)))),
            
            width = 3),

        # Setting up the main panel with tabs
        mainPanel(
            tabsetPanel(
                tabPanel("Most winning cyclists",
                         fluidRow(h3("Cyclists with most Tour de France wins"),
                                  paste("This plot shows the cyclists with most",
                                  "overall wins in Tour de France. As we can see,",
                                  "Lance Armstrong has won most times with",
                                  "7 Tour de France titles. However, he was stripped",
                                  "of his titles (as well as stage wins), for his use",
                                  "of banned performance-enhancing substances.",
                                  "The race organisers ASO decided not to reallocate",
                                  "the titles won in the years Armstrong won.")),
                         tags$br(),
                         fluidRow(paste("The graph shows overall winners with more", 
                                        "than 1 title. The graph can be filtered using",
                                        "the year selector and the nationality",
                                        "dropdown list in the sidebar.")),
                         tags$br(),
                         plotOutput("tourWins")),
                tabPanel("Nationalities",
                         fluidRow(
                             h3("From which countries are the overall winners coming from?"),
                             paste("In the first plot, you will find the nationalities of",
                                   "the most winning tour winners. The second plot shows", 
                                   "the number of wins before and after the last 40 tours.", 
                                   "You can change the year with the slider, to compare", 
                                   "different periods.")),
                         tags$br(),
                         plotOutput("winnerNationalities"),
                         tags$br(),
                         fluidRow(HTML(paste("As we can see from this graph,",
                                        "France has the greatest number of",
                                        "Tour de France titles. Together with Belgium",
                                        "they have won over 50 % of the",
                                        "Tour de France titles. <br /> In the graph below",
                                        "we can take a look at the 6 most winning",
                                        "countries and the distribution of their",
                                        "wins before and after a given year."))),
                         fluidRow(tags$br(),
                                  sliderInput("winnerYear", 
                                              "Select year for splitting into periods",
                                              min = min(lubridate::year(
                                                  tdf_winners$start_date)), 
                                              max = max(lubridate::year(
                                                  tdf_winners$start_date)), 
                                              value = 1980,
                                              sep = ""),
                                  plotOutput("winnerNationalitiesPeriod")),
                         fluidRow(paste("If we look at the last 40 years, we can", 
                                        "see that the overall Tour de France victories", 
                                        "have been more evenly distributed. From 1980", 
                                        "to 2019, France has won 5 tours, and Belgium",
                                        "has won 0, while other countries have been",
                                        "taking over. Last time a rider from", 
                                        "France won the title was in 1985."))),
                tabPanel("Distance and time used", 
                         fluidRow(tags$br(), 
                                  paste("The first plot shows the distance covered,",
                                  "in kilometres, across the entire tour.",
                                  "You can filter by time, using the year-input",
                                  "in the sidebar. From the graph, we can see that",
                                  "the total distance was around 5000 kilometers",
                                  "between 1910 and 1930. In the years after,", 
                                  "the distance has become shorter and stabilized",
                                  "at around 3500 kilometers.")),
                         tags$br(),
                         checkboxInput("avgSpeed", "Add line for average speed"),
                         plotOutput("tourDistance"),
                         tags$br(),
                         fluidRow(paste("While the total distance have been getting",
                                        "shorter, the speed have been getting faster.")),
                         tags$br(),
                         fluidRow(paste("Below you can find plots showing the time taken", 
                                        "for the winner to complete the tour, as well as", 
                                        "plot for winning margin.")),
                         
                         # Create tabs inside the 'Distance and time used' tab
                         tabsetPanel(
                             
                             tabPanel("Time overall",
                                      tags$br(),
                                      plotOutput("timeOverall")),
                             tabPanel("Time margin",
                                      fluidRow(tags$br(),
                                               paste("In the early years of Tour de", 
                                                     "France, cyclists rode individually,", 
                                                     "and were sometimes forbidden to", 
                                                     "ride together. This led to large", 
                                                     "gaps between the winner and", 
                                                     "the number two. The time gap",
                                                     "could be more than an hour.", 
                                                     "Over time, this difference has", 
                                                     "become smaller as cyclists",
                                                     "now tend to cycle in a", 
                                                     "group (peloton). The winning",
                                                     "margin now, usually originates",
                                                     "from time trials, breakaways or",
                                                     "mountain stages.")),
                                      fluidRow(tags$br(), 
                                               paste("To get more details about the",
                                                     "tightening time margin in later", 
                                                     "years, I recommend to filter", 
                                                     "the year-input after year 1928",
                                                     "and set the radio button to", 
                                                     "'Minutes'")),
                                      tags$br(),
                                      radioButtons("timescale", 
                                                   "Select how to express time on y-axis",
                                                   choices = c("Hours", "Minutes"),
                                                   selected = "Hours",
                                                   inline = TRUE),
                                      plotOutput("timeMargin")))),
                tabPanel("Yellow jersey",
                         fluidRow(tags$br(),
                                  paste("The yellow jersey in Tour de France is",
                                  "one of the most coveted item of clothing in",
                                  "professional cycling. The wearer is the rider",
                                  "with the lowest aggregate time at a given stage,",
                                  "representing the leader of the general",
                                  "classification. In the plot below you can see",
                                  "the number of days the overall winners were wearing",
                                  "the yellow jersey, and compare over time.",
                                  "The number of stages spent as the race leader",
                                  "are averaged per decade.")),
                         tags$br(),
                         plotOutput("plotYellowJersey"),
                         fluidRow(tags$br(),
                                  paste("For the tour winners, the average number of", 
                                        "days in yellow jersey have been just over",
                                        "10 days, as we can see in the graph below.")),
                         tags$br(),
                         plotOutput("densityPlotYellowJersey"),
                         fluidRow(tags$br(), 
                                  paste("In the table you can find which overall",
                                        "winners that have worn the yellow jersey",
                                        "most days.")),
                         tags$br(),
                         DT::dataTableOutput("tableYellowJersey")),
                tabPanel("Tour winners", 
                         fluidRow(h3("Table of tour winners"),
                                  tags$br(),
                                  paste("This is a table of the tour winners.",
                                        "You can filter the table using the year",
                                        "selector and the nationality dropdown list",
                                        "in the sidebar.", "You can also use the search box",
                                        "in the table, as well as clicking the column",
                                        "names to sort the columns.")),
                         tags$br(),
                         fluidRow(DT::dataTableOutput("tableWinners"))),
                tabPanel("Traits",
                         fluidRow(tags$br(),
                                  paste("The chart below shows different traits of",
                                  "the tour winners. You can select the traits with",
                                  "the dropdown list.")),
                         tags$br(),
                         tags$div(
                             selectInput("traits", "Traits",
                                         choices = c("Age", "Height", "Weight"),
                                         width = "20%")
                                  ),
                         fluidRow(plotOutput("winnerTraits")),
                         fluidRow(tags$br(),
                                  paste("As we can see from the charts, we did",
                                        "not have many records of the winners'",
                                        "height and weight before Word War II.")))
                         
                )
            )
            
            
        )
    
    ),

#--- Navbar Stages----    
    # 2nd navbar tab, for data related to stages
    tabPanel("Stages", 
             sidebarLayout(
                 sidebarPanel(
                     
                     dateRangeInput2("year2", "Select year",
                                     start = min(tdf_winners$start_date),
                                     end = max(tdf_winners$start_date),
                                     min = min(tdf_winners$start_date),
                                     # Adding 1 so max input don't crash with end input
                                     max = max(tdf_winners$start_date) + 1,
                                     format = "yyyy",
                                     minview = "years",
                                     maxview = "decades"),
                     
                     selectInput("stype", "Stage type", 
                                 choices = c("All types", "Flat stage", "Hilly stage",
                                             "Mountain stage", "Individual time trial",
                                             "Team time trial")),
                     width = 3),
                 
                 # Setting up the main panel for the Stages navbar
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Stages",
                                  fluidRow(tags$br(),
                                           paste("Over the years, the number of",
                                           "stages for Tour de France has stabilized",
                                           "at 21 day-long stages, over the courses",
                                           "of 23 days.")),
                                  tags$br(),
                                  checkboxInput("checkSplit", 
                                                "Add identifier for number split stages"),
                                  plotOutput("stageCount"),
                                  tags$br(),
                                  paste("Between 1934 and 1985, some stages were",
                                        "split in two, with two stages on the same day.", 
                                        "Often into a standard mass-start stage and", 
                                        "time trial stage. Stages split in three could", 
                                        "also happen. The split stages explain", 
                                        "some of the spikes in the total number of", 
                                        "stages in the line chart."),
                                  tags$br(),
                                  tags$br(),
                                  paste("The number of days between the first and", 
                                        "the last stage can be found in the plot below"),
                                  tags$br(), tags$br(),
                                  plotOutput("n_days")),
                         tabPanel("Stage profiles", 
                                  fluidRow(tags$br(),
                                           paste("In this plot you can find stage",
                                           "distances per stage type, over time.",
                                           "We can see that the flat-, mountain- and", 
                                           "hilly stage categories are longer stages",
                                           "than the time trials.", 
                                           "The longest distances took place in the",
                                           "earliest editions of Tour de France,",
                                           "in the years before 1930. In the years",
                                           "1919-1924 there was a stage in each", 
                                           "edition from Les Sables-d'Olonne to", 
                                           "Bayonne. This stage was 482 km!")),
                                  tags$br(),
                                  plotOutput("plotDistCat"),
                                  fluidRow(tags$br(),
                                           paste("The plot below shows a decrease",
                                                 "in percentage of flat stages",
                                                 "during the history of Tour de", 
                                                 "France. In an effort to make the",
                                                 "flat stages more competitive,",
                                                 "the team time trial was introduced",
                                                 "in 1927. Although this had not been",
                                                 "successful, the formula was repeated",
                                                 "in 1928, before most stages were run",
                                                 "in normal format in 1929.",
                                                 "Flat stages are still one of the",
                                                 "most common stage types, but the",
                                                 "category mix have become more",
                                                 "balanced in later years.")),
                                  tags$br(),
                                  plotOutput("percentageStages"),
                                  tags$br()),
                         tabPanel("Grands Départs", 
                                  fluidRow(tags$br(),
                                           paste("The first stage of the tour is known", 
                                                 "as the Grand Départ. Since the 1950s",
                                                 "it has typically taken place in",
                                                 "a different town each year, and",
                                                 "since the 1970s it has been common to",
                                                 "award the Grand Départ to cities",
                                                 "outside France as a way of increasing",
                                                 "international interest in the",
                                                 "competition and the sport.")),
                                  tags$br(),
                                  checkboxInput("intGD", "Only international Grand Départs"),
                                  fluidRow(DT::dataTableOutput("GD"))),
                         tabPanel("Host cities", 
                                  fluidRow(tags$br(),
                                           paste("Some cities and towns have hosted",
                                           "more stage starts and finishes than",
                                           "others. The plot below shows the ten cities",
                                           "that have hosted most stage starts",
                                           "and finishes.")),
                                  tags$br(),
                                  plotOutput("hostCities"),
                                  tags$br(),
                                  fluidRow(paste("The table below shows which cities",
                                                 "that have hosted most stage starts",
                                                 "and which cities have hosted most",
                                                 "finishes")),
                                  tags$br(),
                                  fluidRow(column(3, tableOutput("hostStart")),
                                           column(1),
                                           column(3, tableOutput("hostFinish"))),
                                  tags$br(),
                                  fluidRow(paste("Host city combinations that have been",
                                  "cycled most times")),
                                  tags$br(),
                                  tableOutput("stageFreq")),
                         tabPanel("Countries stage wins",
                                  fluidRow(tags$br(),
                                           HTML(paste("The following graph presents",
                                                 "the countries with most stage wins",
                                                 "in the Tour de France history.", "<br/>",
                                                 "You can filter number of countries",
                                                 "to display in the graph with the", 
                                                 "slider. The remaining countries",
                                                 "will be merged into a factor called
                                                 'Other'. You can also filter by year",
                                                 "and stage type in the sidebar."))),
                                  tags$br(),
                                  sliderInput("countriesrowselector", 
                                              paste("Select number of countries", 
                                                    "to include in the graph"),
                                              min = 5, max = 20, value = 10),
                                  plotOutput("countriesStageWins"),
                                  fluidRow(tags$br(),
                                           paste("An alternative way to plot the",
                                                 "countries with most stage wins",
                                                 "for different stage types,",
                                                 "is to use a treemap. The treemap",
                                                 "is an efficient way to show the",
                                                 "composition of a whole, when there",
                                                 "are many components.",
                                                 "With the treemap below, we can plot",
                                                 "all countries within a single plot.")),
                                  tags$br(),
                                  plotOutput("treemap")))
                 )
             )
             
             ),

#--- Navbar Riders statistics----    
    # 3rd navbar tab, for data related to riders and stage wins
    tabPanel("Rider statistics",
             sidebarLayout(
                 sidebarPanel(
                     dateRangeInput2("year3", "Select year",
                                     start = min(tdf_winners$start_date),
                                     end = max(tdf_winners$start_date),
                                     min = min(tdf_winners$start_date),
                                     # Adding 1 so max don't crash with end
                                     max = max(tdf_winners$start_date) + 1,
                                     format = "yyyy",
                                     minview = "years",
                                     maxview = "decades"),
                     width = 3),
                 
                 # Setting up the main panel for the Rider statistics navbar
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Most stage wins",
                                  fluidRow(tags$br(),
                                           paste("During the history of Tour de", 
                                                 "France, it is the Belgian rider", 
                                                 "Eddy Merckx who won most stages.",
                                                 "He also won the yellow jersey 5 times.",
                                                 "Merckx is considered one of the",
                                                 "greatest professional racers ever,", 
                                                 "and during his peak years (1969-75)",
                                                 "he won some 35 percent of the races", 
                                                 "he entered.")),
                                  tags$br(),
                                  fluidRow(paste("The plot below shows the riders",
                                                 "with most stage wins. You can filter",
                                                 "number of riders to include with", 
                                                 "the slider, as well as which years",
                                                 "you want to include with the year input",
                                                 "in the sidebar.")),
                                  tags$br(),
                                  sliderInput("nStageWinners",
                                              "Select number of riders to include in the plot",
                                              min = 5, max = 30, value = 15),
                                  plotOutput("mostStageWins")),
                         tabPanel("Most wins per year",
                                  fluidRow(
                                  h3("Table of riders with most stage wins per year"),
                                  tags$br(),
                                  paste("This table shows the riders with most",
                                        "stage wins for each edition of the tour.",
                                        "You can filter with the year input in the sidebar",
                                        "and search names, stage wins or a specific", 
                                        "year with the search box. Notice that this", 
                                        "table only includes the racers with most", 
                                        "stage wins for a given year. In case of", 
                                        "equal number of stage wins all riders", 
                                        "are included.")),
                                  tags$br(),
                                  fluidRow(DT::dataTableOutput("WinsPerYear"))),
                         tabPanel("Tour completion",
                                  fluidRow(tags$br(), 
                                           paste("The plot below shows how many riders",
                                           "that have completed the tour for each",
                                           "edition of Tour de France.")),
                                           tags$br(),
                                           plotOutput("completion"),
                                           fluidRow(tags$br(),
                                                    paste("As we can see from the",
                                                    "plot, the number of riders that",
                                                    "make it to the finish line",
                                                    "in Paris has been increasing."))),
                         tabPanel("Animation",
                                  fluidRow(h3("Animation of points classification"),
                                           paste("In Tour de France, points are",
                                                 "awarded to the first 15 finishers,",
                                                 "and for winning intermediate",
                                                 "sprints. These points are recorded",
                                                 "in a points classification."),
                                           tags$br(), tags$br(),
                                           paste("Below you can find an animation of",
                                                 "points classification for a given", 
                                                 "year between 2000 and 2019."), tags$br(),
                                           paste("Notice that the points given here",
                                                 "are not identical to the official", 
                                                 "points classification."),tags$br(),
                                           paste("In this dataset, all stage winners,", 
                                                 "regardless of stage type, are", 
                                                 "awarded 100 points. This is not", 
                                                 "the case in the official points", 
                                                 "classification"), tags$br(), 
                                           paste("The animation can take some time",
                                                 "to render.")),
                                  tags$br(),
                                  sliderInput("animYear", 
                                              "Select year for points animation",
                                              min = 2000, max = 2019, 
                                              value = 2017, sep = ""),
                                  HTML(paste("<b>Click animate to create animation</b>")),
                                  tags$br(),
                                  actionButton("animate", "Animate"),
                                  tags$head(tags$style(type="text/css",
                                                       paste0("
                                             #loadmessage {
                                             position: statics;
                                             top: 20px;
                                             left: 0px;
                                             width: 100%;
                                             padding: 5px 0px 5px 0px;
                                             text-align: center;
                                             font-weight: bold;
                                             font-size: 100%;
                                             color: ", "#6D7696",";
                                             background-color: ", "#E3C598",";
                                             z-index: 105;
                                             }
                                             "))),
                                  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                   tags$div(paste("Creating animation.", 
                                                                  "This may take a couple", 
                                                                  "of minutes.", 
                                                                  "Please wait."),
                                                            id = "loadmessage")),
                                  shinycssloaders::withSpinner(
                                      imageOutput("animation"), type = 5, color = "#6D7696")),
                         tabPanel("Most starts",
                                  fluidRow(tags$br(),
                                           paste("The following graph shows the riders",
                                                 "with most appearances. The record is",
                                                 "held by Sylvain Chavanel, with",
                                                 "18 starts.")),
                                  tags$br(),
                                  sliderInput("startsrowselector", 
                                              "Select number of records for the plot",
                                              min = 5, max = 30, value = 20, step = 5),
                                  plotOutput("plotStartsFinishes")))
                     )
                 )
             ),

#--- Navbar About----

    # 4th navbar tab, for information about the app.
    tabPanel("About", "This is an app for the annual bicycle race Tour de France.",
             tags$br(),
             paste("Primarily held in France, the tour spans over multiple stages",
             "and consists of 21 day-long stages over the course of 23 days."),
             fluidRow(tags$br()),
             HTML(paste("<b>", "Note:", "</b>", "the data in this app is based", 
                        "on data from the", "<em>", "tdf", "</em>", "package and data",
                        "from Wikipedia. The data in the", "<em>", "tdf", "</em>", 
                        "package retains the winning times of banned, disqualified and",
                        "otherwise sanctioned riders for the purposes of data analysis.",
                        "The overall standings are as they would have appeared on",
                        "the final day of the race - therefore please note that",
                        "the officially recognised winner of a particular edition",
                        "may not be the rider shown in this app.")))
    
    )
    

)



# Define server logic
server <- function(input, output, session) {


    # Reactive expressions
    ## Filter time period by selected year-inputs
    winners_interval <- reactive({
        tdf_winners %>% 
            filter(between(lubridate::year(start_date),
                           lubridate::year(input$year[[1]]),
                           lubridate::year(input$year[[2]])))
    })
    
    #Reactive expression for stage type
    stage_type <- reactive({
        tdf_stages1903_2019 %>% 
            mutate(Category = case_when(
                Type %in% c("Plain stage", "Flat stage", "Flat Stage",
                        "Flat cobblestone stage", 
                        "Plain stage with cobblestones") ~ "Flat stage",
                
                Type %in% c("Stage with mountain(s)", "Mountain stage", 
                        "High mountain stage", "Medium mountain stage", 
                        "Mountain Stage", "Stage with mountain") ~ "Mountain stage",
                
                Type %in% "Hilly stage" ~ "Hilly stage",
            
                Type %in% c("Individual time trial", 
                            "Mountain time trial") ~ "Individual time trial",
            
                Type %in% "Team time trial" ~ "Team time trial",
            
                Type %in% "Half Stage" ~ "Flat stage",
                
                Type %in% "Transition stage" ~ ifelse(Date == as.Date("2008-07-10"),
                                                      "Mountain stage", "Hilly stage"),
                
                Type %in% "Intermediate stage" ~ ifelse(Date == as.Date("2007-07-20"),
                                                        "Flat stage", "Hilly stage")))
    })
    
    # Reactive expression for riders at first stage
    first_stage <- reactive({
        stage_data %>% 
            group_by(year) %>% 
            filter(case_when(year == 1988 ~ stage_results_id == "stage-1",
                             year %in% c(1955, 1960, 
                                         1961, 1965, 1971) ~ stage_results_id == "stage-1a",
                             
                             "stage-0" %in% stage_results_id ~ stage_results_id == "stage-0",
                             TRUE ~ stage_results_id == "stage-1"))
    })
    
    # Reactive expression for most starts
    mostStarts <- reactive({
        stage_data %>% 
            filter(between(year, 
                           lubridate::year(input$year3[[1]]),
                           lubridate::year(input$year3[[2]]))) %>% 
            group_by(rider) %>% 
            summarise(starts = length(unique(year)), 
                      finishes = starts - length(unique(year[
                          # Conditional filter of rank that does contain 
                          # DNS, DNF, DSQ, NQ or OTL
                          str_detect(rank, pattern = "DNS|DNF|DSQ|NQ|OTL")])), 
                      first = first(year), 
                      last = last(year)) %>% 
            ungroup() %>% arrange(desc(starts), desc(finishes)) %>% 
            # Let user have some control on how many records to show in the plot,
            # using a sliderInput.
            slice(1:input$startsrowselector) %>%
            # Convert rider variable to factor, in order to preserve the sorting 
            # (the order created by arrange()) when creating the plot
            mutate(rider = factor(rider, levels = rider)) %>% 
            tidyr::pivot_longer(cols = starts:finishes, 
                                names_to = "participation", 
                                values_to = "appearances")
    })
    
    # Reactive expression for group variable to use in line plots,
    # to create empty space for years with no race (during World War I and II)
    period_group <- reactive({
        winners_interval() %>% 
            mutate(period = case_when(
                lubridate::year(start_date) <= 1914 ~ "1903-1914",
                between(lubridate::year(start_date), 1919, 1939) ~ "1919-1939",
                TRUE ~ "1947-2019"))
    })
        
    
    
    # Cleveland dot plot of riders with most Tour de France titles 
    output$tourWins <- renderPlot({
        winners_interval() %>% 
            filter(if(input$country != "All") 
                str_trim(nationality) == input$country else TRUE) %>% 
            group_by(winner_name) %>% 
            mutate(n = n()) %>% 
            filter(case_when(input$country == "All" & 
                                 input$year[[2]] - input$year[[1]] <= (20*365) ~ TRUE,
                             input$country == "All" ~ n > 1,
                             TRUE ~ TRUE)) %>% ungroup() %>% 
            
            ggplot(aes(x = n, y = reorder(winner_name, n))) +
            geom_point(aes(colour = n), show.legend = FALSE) +
            scale_colour_gradient(low = "#4692CD", high = "#132B43") +
            labs(title = "Overall winners who has won Tour the France most times", 
                 x = "Number of Tour wins", y = "Cyclist") +
            theme_light()
    }, res = 96)
    
    
    
    output$winnerNationalities <- renderPlot({
        tdf_winners %>% 
            ggplot(aes(y = fct_relevel(
                # Set y-axis for horizontal barplot. 
                # fct_infreq changes the order of levels by number of observations.
                # fct_rev makes the most frequent observations appear at the top instead of
                # at the bottom.
                # fct_lump_min will label records with less than 2 observations as 
                # 'Other nationalities'
                fct_lump_min(
                    fct_rev(fct_infreq(str_trim(nationality))), 
                    min = 2, other_level = "Other\nnationalities"), 
                # fct_relevel will put 'Other nationalities' at the bottom, 
                # using the after argument 
                "Other\nnationalities", after = 0))) + 
            geom_bar() + 
            labs(title = "Tour de france winners", 
                 x = "Number of tour wins", y = "Nationality") +
            theme_light()
    }, res = 96)
    
    output$winnerNationalitiesPeriod <- renderPlot({
        tdf_winners %>% 
            mutate(era = ifelse(lubridate::year(start_date) < input$winnerYear, 
                                paste("Before", input$winnerYear),
                                paste("From", input$winnerYear, "and after"))) %>% 
            ggplot(aes(y = fct_relevel(
                fct_lump_min(
                    fct_rev(fct_infreq(str_trim(nationality))), 
                    min = 6, other_level = "Other\nnationalities"), 
                "Other\nnationalities", after = 0))) + 
            geom_bar() + labs(title = "Tour de france winners",
                              x = "Number of tour wins", y = "Nationality") + 
            facet_grid(~ era) + 
            theme_light()
    }, res = 96)
    
    output$tourDistance <- renderPlot({
        
        if (input$avgSpeed == FALSE) {
                plotDist <- period_group() %>%
                    
                    ggplot(aes(x = start_date)) + 
                    geom_line(aes(y = distance, group = period), colour = "darkslateblue") +
                    labs(title = "Tour distance over time",
                         x = NULL, y = "Distance travelled (km)") + 
                    scale_colour_manual(values = c("darkslateblue", "darksalmon"), 
                                name = NULL) + 
                    theme_light()
        }
        
        if(input$avgSpeed) {
                plotDist <- period_group() %>% 
                    mutate(speed = distance / time_overall) %>% 
                    
                    ggplot(aes(x = start_date)) + 
                    geom_line(aes(y = distance, colour = "Distance", group = period)) +
                    geom_line(aes(y = speed*100, colour = "Speed")) +
                    labs(title = "Tour distance over time",
                         x = NULL, y = "Distance travelled (km)") + 
                    scale_y_continuous(sec.axis = 
                                           sec_axis(~./100, 
                                                    name = "Speed of tour winner (km/h)")) +
                    scale_colour_manual(values = c("darkslateblue", "darksalmon"), 
                                        name = NULL) + 
                    theme_light()
            }
        plotDist
            
    }, res = 96)
    
    
    output$timeOverall <- renderPlot({
        winners_interval() %>% 
            
            ggplot(aes(x = start_date)) + 
            geom_line(aes(y = time_overall), colour = "darkgoldenrod3") +
            labs(title = "Time taken for winner to complete the tour",
                 x = NULL, y = "Time used (hours)") +
            theme_light()
    }, res = 96)
    
    
    output$timeMargin <- renderPlot({
        winners_interval() %>%
            mutate(time_margin = case_when(
                input$timescale == "Minutes" ~ time_margin * 60,
                TRUE ~ time_margin)) %>% 
            
            ggplot(aes(x = start_date)) +
            geom_line(aes(y = time_margin), colour = "lightpink3") +
            labs(title = "Time margin between winner and runner up", 
                 x = NULL, 
                 y = paste0("Difference in total time (", 
                            tolower(input$timescale), ")")) +
            theme_light()
    }, res = 96)
        
        
    output$plotYellowJersey <- renderPlot({
        winners_interval() %>%
            group_by(decade = (lubridate::year(start_date) %/% 10) * 10) %>% 
            summarise(avg_stages_led = mean(stages_led)) %>% 
            ungroup() %>% 
            
            ggplot(aes(x = decade, y = avg_stages_led)) + 
            geom_line(colour = "darkslategray") + 
            labs(title = "Number of days in yellow jersey for Tour winner", 
                 x = NULL, y = "Days as race leader") +
            expand_limits(y = 0) +
            theme_light()
    }, res = 96)
    
    output$densityPlotYellowJersey <- renderPlot({
        winners_interval() %>% 
            ggplot(aes(x = stages_led)) + 
            geom_density() + 
            labs(title = "Density of number of days in yellow jersey", 
                 x = "Days as race leader") + 
            geom_vline(aes(xintercept = mean(stages_led)), 
                       colour = "aquamarine3", linetype = "dashed") + 
            annotate(geom = "text", 
                     x = mean(winners_interval()$stages_led) + 0.5, y = 0.005, 
                     hjust = 0, 
                     label = "Average", angle = 90, colour = "aquamarine3") +
            theme_light() 
    }, res = 96)
    
    output$tableYellowJersey <- DT::renderDataTable({
        winners_interval() %>% 
            group_by(winner_name) %>% 
            summarise(n_days = sum(stages_led),
                      Years = paste("(", 
                                    toString(sort(lubridate::year(start_date))), 
                                    ")", sep = "")) %>% 
            arrange(desc(n_days)) %>% 
            ungroup()
    }, 
    colnames = c("Winner name", "Days in yellow", "Years"),
    options = list(pageLength = 10,
                   columnDefs = list(list(className = 'dt-center', targets = 2)))
    )
    
    
    output$tableWinners <- DT::renderDataTable({
        winners_interval() %>% 
            mutate_at(vars(edition, stage_wins, stages_led, weight, age),
                      as.integer) %>%
            
            filter(if (input$country != "All") 
                str_trim(nationality) == input$country else TRUE) %>% 
            DT::datatable(colnames = c("Edition", "Start date", "Winner name", 
                                       "Team", "Distance", "Time overall (hours)",
                                       "Time margin (hours)", "Stage wins", 
                                       "Stages led", "Height (m)", "Weight (kg)", 
                                       "Age", "Born", "Died", "Full name", 
                                       "Nickname", "Birth town","Birth country", 
                                       "Nationality"), 
                          rownames = FALSE) %>% 
            DT::formatRound(c("time_overall", "time_margin"), digits = 4) %>% 
            DT::formatStyle(columns = c(1:2, 5:19), 'text-align' = 'center')
        })
    
    output$winnerTraits <- renderPlot({
        winners_interval() %>% 
            group_by(decade = (lubridate::year(start_date) %/% 10) * 10) %>% 
            # Summarise the drop-down list variable traits per decade.
            # Since the list contains variables with capitalized letters,
            # we have to use tolower() to match with the column names in the dataframe.
            summarise(avg_input = mean(.data[[tolower(input$traits)]], na.rm = TRUE)) %>% 
            ungroup() %>% 
        
            ggplot(aes(x = decade, y = avg_input)) + 
            geom_line(colour = case_when(
                input$traits == "Age" ~ "coral4",
                input$traits == "Height" ~ "darkcyan",
                input$traits == "Weight" ~ "darkmagenta")) +
            labs(title = paste(input$traits, "of tour winners"),
                 x = "Decade", y = case_when(
                    input$traits == "Age" ~ 
                        "Average age of tour winner per decade",
                    input$traits == "Height" ~ 
                        "Average height of tour winner per decade (meters)",
                    input$traits == "Weight" ~ 
                        "Average weight of tour winner per decade (kg)")) +
            expand_limits(y = 0) +
            theme_light()
    }, res = 96)
    
    
    output$stageCount <- renderPlot({
        # Plot of stage counts dependent on checkbox
        if (input$checkSplit == FALSE) {
            
            plotStage <- stage_type() %>%
                filter(between(lubridate::year(Date),
                               lubridate::year(input$year2[[1]]),
                               lubridate::year(input$year2[[2]])) &
                           
                           if(input$stype != "All types") (Category == input$stype) else
                               TRUE) %>% 
                
                group_by(year = lubridate::year(Date)) %>% 
                summarise(stages = n_distinct(Stage),
                          # Counting unique Dates when the ending of the
                          # Stage variable is a letter. Split stages are stages
                          # that are split into two parts (sometimes three), where 
                          # more than one stage takes place on the same day.
                          split_stages = n_distinct(
                              # Do not include stages denoted with P (Prologue)
                              Date[str_ends(Stage, pattern = "[^P][[:alpha:]]")])) %>%
                ungroup() %>% 
                mutate(period = case_when(year <= 1914  ~ "1903-1914",
                                          between(year, 1919, 1939) ~ "1919-1939",
                                          TRUE ~ "1947-2019")) %>%
                
                # Plot the output
                ggplot(aes(x = year)) + 
                geom_line(aes(y = stages, group = period), colour = "dodgerblue3") +
                labs(title = paste("Number of stages for each edition", 
                                   case_when(
                                       input$stype == "All types" ~ " (All stage types)",
                                       TRUE ~ paste0(" ", "(", input$stype, "s", ")"))),
                     x = "Year", y = "Number of stages") + 
                expand_limits(y = 0) +
                theme_light()
        }
        
        if (input$checkSplit) {
            
            plotStage <- stage_type() %>%
                group_by(year = lubridate::year(Date)) %>% 
                summarise(stages = n_distinct(Stage),
                          # Counting unique Dates when the ending of the
                          # Stage variable is a letter. Split stages are stages
                          # that are split into two parts (sometimes three), where 
                          # more than one stage takes place on the same day.
                          split_stages = n_distinct(
                              # Do not include stages denoted with P (Prologue)
                              Date[str_ends(Stage, pattern = "[^P][[:alpha:]]")])) %>%
                ungroup() %>% 
                mutate(period = case_when(year <= 1914  ~ "1903-1914",
                                          between(year, 1919, 1939) ~ "1919-1939",
                                          TRUE ~ "1947-2019")) %>% 
                
                # Plot the output
                ggplot(aes(x = year)) + 
                geom_line(aes(y = stages, group = period), colour = "dodgerblue3") + 
                geom_col(aes(y = split_stages), fill = "dodgerblue3") +
                labs(title = paste("Number of stages for each edition of",
                                   "Tour de France 1903 - 2019"), 
                     x = "Year", y = "Number of stages") +
                annotate(geom = "label", x = 1953, y = 11, 
                         label = "Split stages", 
                         fontface = "bold", size = 5, label.size = 0.2) + 
                theme_light()
        }
        plotStage
        
    }, res = 96)
    
    
    output$n_days <- renderPlot({
        tdf_stages1903_2019 %>% 
            group_by(year = lubridate::year(Date)) %>% 
            summarise(days = as.integer(last(Date) - first(Date) + 1)) %>% 
            ungroup() %>% 
            
            ggplot(aes(x = year, y = days)) + 
            geom_line(colour = "lightpink3") +
            labs(title = "Number of days for each edition", 
                 x = "Year", y = "Number of days between first and last stage") + 
            expand_limits(y = 0) +
            theme_light()
    }, res = 96)
    
    # Plot of stage distances per stage types
    output$plotDistCat <- renderPlot({
        stage_type() %>%
            # Set Category variable to ordered factor in order to control the order of
            # the panels in facet_grid.
            mutate(Category = factor(Category, 
                                     levels = c("Flat stage", "Hilly stage", 
                                                "Mountain stage", "Individual time trial",
                                                "Team time trial"))) %>% 
            
            ggplot(aes(x = Date, y = Distance)) +
            geom_point(aes(colour = Category, alpha = 0.4), show.legend = FALSE) + 
            facet_grid(~Category) + 
            labs(title = "Stage distances per category over time", 
                 x = "Year", y = "Distance (km)") + 
            theme_light()
    }, res = 96)
    
    # Plot number of stages per stage type
    output$percentageStages <- renderPlot({
        stage_type() %>% 
            # Count number of stages per category
            group_by(year = lubridate::year(Date), Category) %>% 
            summarise(n_stages = n()) %>%
            # Calculate percentages from the counts, and order the Category factor
            # as in previous plot
            mutate(percentage = prop.table(n_stages), 
                   Category = factor(Category, 
                                     levels = c("Flat stage", "Hilly stage", 
                                                "Mountain stage", "Individual time trial", 
                                                "Team time trial"))) %>% 
            
            ggplot(aes(x = year, y = percentage)) + 
            geom_line(aes(colour = Category), show.legend = FALSE) + 
            facet_grid(~Category) +
            scale_x_continuous(breaks = c(1900, 1950, 2000)) +
            scale_y_continuous(labels = scales::percent) + 
            labs(title = "Stage type distribution", 
                 x = "Year", y = "Number of stages") + 
            theme_light()
        
    }, res = 96)
    
    
    # Table with Grands Départs
    output$GD <- DT::renderDataTable({
        tdf_stages1903_2019 %>% 
            group_by(year = lubridate::year(Date)) %>% 
            filter(case_when(
                year %in% c(1955, 1960, 1961, 1965, 1967, 1968) ~ Stage == "1a",
                "P" %in% Stage ~ Stage == "P", 
                TRUE ~ Stage == "1")) %>% ungroup() %>%
             
            filter(if (input$intGD) 
                lubridate::year(Date) %in% 
                    c(1954, 1958, 1965, 1973, 1975, 1978, 1980, 1982, 1987, 1989, 
                      1992, 1996, 1998, 2002, 2004, 2007, 2009, 2010, 2012, 2014,
                      2015, 2017, 2019) else TRUE) %>%
            
            group_by(Origin) %>% 
            summarise(n_starts = n(),
                      years = if_else(n_starts > 5,"Early years",
                                      paste("(", toString(sort(year)), ")", 
                                            sep = ""))) %>% 
            ungroup() %>% 
            arrange(desc(n_starts))
        
    }, colnames = c("City", "Number of starts", " Years"),
    options = list(columnDefs = list(list(className = 'dt-center', targets = 2))))
    
    
    output$hostCities <- renderPlot({
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
        
    }, res = 96)
    
    output$hostStart <- renderTable({
        tdf_stages %>% 
            count(Origin, sort = TRUE) %>% 
            slice(1:10) %>%
            ungroup() %>% 
            rename("Start" = Origin)
    })
    
    output$hostFinish <- renderTable({
        tdf_stages %>% 
            count(Destination, sort = TRUE) %>% 
            slice(1:10) %>% 
            ungroup() %>% 
            rename("Finish" = Destination)
    })
    
    output$stageFreq <- renderTable({
        tdf_stages %>% 
            count(Origin, Destination, sort = TRUE) %>% 
            slice(1:10) %>% 
            ungroup() %>% 
            rename("Start" = Origin, "Finish" = Destination)
    })
    
    output$countriesStageWins <- renderPlot({
        stage_type() %>%
            filter(!is.na(Winner_Country) &
                       
                       between(lubridate::year(Date),
                               lubridate::year(input$year2[[1]]),
                               lubridate::year(input$year2[[2]])) &
                       
                       if(input$stype != "All types") (Category == input$stype) else
                           TRUE) %>%
            
            group_by(Winner_Country = fct_lump_n(Winner_Country, 
                                                 n = input$countriesrowselector)) %>% 
            summarise(n = n()) %>% 
            ungroup() %>% 
            mutate(percentage_wins = prop.table(n)) %>% 
            
            # Plot
            ggplot(aes(x = n, 
                       y = fct_relevel(reorder(Winner_Country, n),
                                       "Other", after = 0))) + 
            geom_col(aes(fill = n), show.legend = FALSE) + 
            scale_fill_gradient(low = "#4692CD", high = "#132B43") +
            labs(title = paste("Countries with most stage wins", 
                               case_when(input$stype == "All types" ~ " (All stage types)",
                                         TRUE ~ paste0(" ", "(", input$stype, "s", ")"))),
                 x = "Number of stages won", y = "Country") + 
            geom_text(aes(label = paste(round(percentage_wins, digits = 2)*100, "%")), 
                      hjust = -0.2) + 
            expand_limits(x = 725) + 
            theme_light()
    }, res = 96)
    
    # Treemap for stage wins
    output$treemap <- renderPlot({
        stage_type() %>% 
            filter(!is.na(Winner_Country) &
                       between(lubridate::year(Date),
                               lubridate::year(input$year2[[1]]),
                               lubridate::year(input$year2[[2]]))) %>%
        
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
            #mutate(percentage = prop.table(Wins)) %>% 
            
            ggplot(aes(area = Wins, fill = Wins, subgroup = Category), start = "topleft") + 
            geom_treemap(start = "topleft") + 
            geom_treemap_subgroup_border(start = "topleft", colour = "cornsilk3") + 
            geom_treemap_subgroup_text(place = "centre", alpha = 0.8, 
                                       colour = "cornsilk3", fontface = "italic", 
                                       min.size = 0, reflow = TRUE, start = "topleft") + 
            geom_treemap_text(aes(label = Winner_Country), 
                              colour = "gray91", start = "topleft") + 
            scale_fill_continuous(high = "#132B43", low = "#56B1F7", name = "# of wins") +
            labs(title = "Treemap of number of stage wins per country")
    }, res = 96)
    
    output$mostStageWins <- renderPlot({
        tdf_stages1903_2019 %>%
            filter(between(lubridate::year(Date),
                           lubridate::year(input$year3[[1]]),
                           lubridate::year(input$year3[[2]]))) %>% 
            group_by(Winner) %>%
            summarise(wins = n()) %>% 
            ungroup() %>% 
            slice_max(wins, n = input$nStageWinners, with_ties = FALSE) %>% 
                     
            ggplot(aes(x = wins, y = reorder(Winner, wins))) +
            geom_point(aes(colour = wins), show.legend = FALSE) +
            scale_colour_gradient(low = "#4692CD", high = "#132B43") +
            labs(title = paste0("Riders with most stage wins ", "(", 
                                lubridate::year(input$year3[[1]]), "-", 
                                lubridate::year(input$year3[[2]]), ")"), 
                 x = "Number of stage wins", y = "Rider") +
            theme_light()
    }, res = 96)
    
    output$WinsPerYear <- DT::renderDataTable({
        tdf_stages1903_2019 %>% 
            group_by(Year = lubridate::year(Date)) %>%
            filter(between(lubridate::year(Date),
                           lubridate::year(input$year3[[1]]),
                           lubridate::year(input$year3[[2]]))) %>% 
            count(Winner) %>% 
            slice(which(n == max(n))) %>% 
            ungroup() %>%
            mutate(Year = as.integer(Year)) %>% 
            rename("Stage wins" = n)
    }, rownames = FALSE)
    
    output$completion <- renderPlot({
        stage_data %>% 
            # Join column with number of racers at the startline for each edition
            inner_join(first_stage() %>% 
                           count(year, name = "riders_started"),
                       by = "year") %>%
            # Add a filter for the years
            filter(between(year, 
                           lubridate::year(input$year3[[1]]),
                           lubridate::year(input$year3[[2]]))) %>% 
            # Group by year
            group_by(year) %>%
            # Using filter to only look at the last stage of each edition. 
            filter(stage_results_id == last(stage_results_id)) %>%
            # Create one record for each edition, with year, stage_results_id, 
            # number of riders completed and percentage of riders that completed
            summarise(stage_results_id = last(stage_results_id), 
                      riders_completed = max(as.numeric(rank), na.rm = TRUE),
                      percentage = riders_completed / riders_started) %>% 
            ungroup() %>% 
            
            # And plot the completion rate
            ggplot(aes(x = year, y = percentage)) + 
            geom_line(colour = "lightcyan3") + 
            labs(title = "Percentage of riders who completed the race", 
                 x = NULL, y = "Percentage completed") + 
            scale_y_continuous(labels = scales::percent) + 
            theme_light()
    }, res = 96)
    
    
    # Get the 10 riders with most points for a given year between 2000 and 2019
    selection <- reactive({ stage_data %>% 
        filter(year == input$animYear) %>% 
        group_by(rider) %>% 
        summarise(total_points = sum(points, na.rm = TRUE)) %>% 
        ungroup() %>% 
        slice_max(total_points, n = 10) %>% pull(rider)
        
        })
    
    
    # Animation of points, adding up per stage. Requires action Button to be clicked
    points_anim <- eventReactive(input$animate, { 
        
        # A temp file to save the output
        outfile <- tempfile(fileext = '.gif')
        
        # Make the animation
        stage_data %>% 
            filter(year == input$animYear & rider %in% selection()) %>% 
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
            labs(title =  paste("Tour de France", input$animYear, "- Stage: {closest_state}"), 
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
        
        
        })
        
        output$animation <- renderImage({
            
            anim_save("outfile.gif", 
                      animate(points_anim(), fps = 25, duration = 20,
                              width = 650, height = 500, renderer = gifski_renderer()))
        # Return a list containing the filename
        list(src = "outfile.gif",
             contentType = 'image/gif')
        
        }, deleteFile = TRUE)
    
    output$plotStartsFinishes <- renderPlot({
        # Cleveland dot plot of riders with most starts and number of finishes
        mostStarts() %>% 
            ggplot(aes(x = appearances, y = fct_rev(rider))) + 
            geom_point(aes(colour = participation), alpha = 0.6) + 
            geom_line(aes(group = rider), alpha = 0.3) + 
            scale_colour_manual(values = c(rgb(0.7, 0.2, 0.1, 0.5), 
                                           rgb(0.2, 0.7, 0.1, 0.5)), 
                                guide = guide_legend(reverse = TRUE)) + 
            labs(title = "Riders with most starts", 
                 x = "Number of appearances", y = "Rider", colour = NULL) +
            expand_limits(y = 0) +
            theme_light()
    }, res = 96)

}

# Run the application 
shinyApp(ui = ui, server = server)
