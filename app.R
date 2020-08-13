
# load necessary packages
library(leaflet)    
library(shiny)
library(shinydashboard)
library(rgdal)
library(tidyverse)
library(readxl)
library(viridis)
library(plotly)
library(treemapify)
#library(hrbrthemes)

#import font
#hrbrthemes::import_roboto_condensed()
#define custom colours
custom_red <- rgb(187,55,84, maxColorValue = 255, names = "custom_red")


# import area boundaries
euro_countries <- readOGR(dsn = "shape_files/europe_uk", 
                          layer = "europe_uk3", 
                          stringsAsFactors = FALSE) 

#filter countries which were in the tournament (at least 5 times)
euro_countries@data$NAME <- c("Albania",                 "Andorra",                
                                                "Austria",                 "Belgium",                
                                                "Bosnia Herzegovina",      "Croatia",                
                                                "Czech Republic",          "Denmark",                
                                                "Estonia",                 "Finland",                
                                                "France",                  "Germany",                
                                                "Gibraltar (UK)",          "Greece",                 
                                                "Guernsey (UK)",           "Hungary",                
                                                "Ireland",                 "Isle of Man (UK)",       
                                                "Italy",                   "Jersey (UK)",            
                                                "Latvia",                  "Liechtenstein",          
                                                "Lithuania",               "Luxembourg",             
                                                "Macedonia",               "Malta",                  
                                                "Monaco",                  "Montenegro",             
                                                "Netherlands",             "Norway",                 
                                                "Poland",                  "Portugal",               
                                                "San Marino",              "Serbia",                 
                                                "Slovakia",                "Slovenia",               
                                                "Spain",                   "Sweden",                 
                                                "Switzerland",             "United Kingdom",         
                                                "Armenia",                 "Azerbaijan",             
                                                "Belarus",                 "Bulgaria",               
                                                "Faeroe Islands (Denmark)","Georgia",                
                                                "Iceland",                 "Jan Mayen (Norway)",     
                                                "Moldova",                 "Romania",                
                                                "Svalbard (Norway)",       "Turkey",                 
                                                "Ukraine",                 "Russia",                 
                                                "England",                 "Northern Ireland",       
                                                "Scotland",                "Wales")        
euro_countries <- euro_countries[#euro_countries@data$NAME== "Wales"|
#                               euro_countries@data$NAME== "Ukraine"|
#                               euro_countries@data$NAME== "Turkey"|
#                               euro_countries@data$NAME== "Switzerland"|
                               euro_countries@data$NAME== "Sweden"|
                               euro_countries@data$NAME== "Spain"|
#                               euro_countries@data$NAME== "Slovenia"|
#                               euro_countries@data$NAME== "Slovakia"|
                               euro_countries@data$NAME== "Serbia"|
#                               euro_countries@data$NAME== "Scotland"|
                               euro_countries@data$NAME== "Russia"|
                               euro_countries@data$NAME== "Romania"|
                               euro_countries@data$NAME== "Portugal"|
#                               euro_countries@data$NAME== "Poland"|
#                               euro_countries@data$NAME== "Norway"|
#                               euro_countries@data$NAME== "Northern Ireland"|
                               euro_countries@data$NAME== "Netherlands"|
#                               euro_countries@data$NAME== "Latvia"|
                               euro_countries@data$NAME== "Italy"|
#                               euro_countries@data$NAME== "Ireland"|
#                               euro_countries@data$NAME== "Iceland"|
#                               euro_countries@data$NAME== "Hungary"|
#                               euro_countries@data$NAME== "Greece"|
#                               euro_countries@data$NAME== "Albania"|
                               euro_countries@data$NAME== "France"|
                               euro_countries@data$NAME== "England"|
                               euro_countries@data$NAME== "Denmark"|
                               euro_countries@data$NAME== "Czech Republic"|
                               euro_countries@data$NAME== "Croatia"|
#                               euro_countries@data$NAME== "Bulgaria"|
                               euro_countries@data$NAME== "Belgium"|
#                               euro_countries@data$NAME== "Austria"|
                               euro_countries@data$NAME== "Germany", ]
      
#load football data
#load data
euro_data <- read_xlsx("euro_data.xlsx")

euro_data_summary <- euro_data %>%
  group_by(Country) %>%
  summarise(
    tournaments_played = n(),
    games_played = sum(Played),
    av_games_played = mean(Played),
    Wins = sum(`Games Won`),
    Draws = sum(`Games Drawn`),
    Defeats = sum(`Games Lost`),
    bottom_fill = (32 - Defeats) - Draws / 2,
    top_fill = (32 - Wins) - Draws / 2,
    game_points = Wins * 3 + Draws,
    goals_for = sum(`Goals For`),
    goals_against = sum(`Goals Against`),
    goal_ratio = goals_for/goals_against,
    goal_difference = goals_for - goals_against,
    av_goals_for = goals_for/games_played,
    av_goals_against = goals_against/games_played,
    Champion = sum(Champion),
    `Runner Up` = sum(`Runner Up`),
    tournament_points = Champion * 3 + `Runner Up`
  ) #%>% 
#filter(tournaments_played > 4)

podium_data <- euro_data_summary %>% 
  select(Country, Champion, `Runner Up`) %>%
  filter(Country %in% c("Germany", "France", "Spain")) %>%
  pivot_longer(cols = c(Champion, `Runner Up`), names_to = "Rank", values_to = "Titles") %>% 
  mutate(
    Country = factor(Country,
                     levels = c("Spain", "Germany", "France"),
                     ordered = TRUE)
  )


game_statistics <- euro_data_summary %>% 
  select(Country, bottom_fill, Defeats, Draws, Wins, top_fill, tournaments_played, game_points) %>%
  filter(tournaments_played > 4) %>% 
  pivot_longer(cols = c(bottom_fill, Defeats, Draws, Wins, top_fill), names_to = "Game Result", values_to = "Number") %>% 
  mutate(
    `Game Result` = factor(`Game Result`,
                           levels = c("bottom_fill", "Defeats", "Draws", "Wins", "top_fill"),
                           ordered = TRUE),
    Country = reorder(Country, game_points)
  ) 

tree_data <- euro_data_summary %>% 
  mutate(
    `Goals For`= goals_for,
    `Goals For Label` = paste(Country, goals_for, sep = "\n"),
    Wins = Wins,
    `Wins Label` = paste(Country, Wins, sep = "\n"),
    `Goals Against`= goals_against,
    `Goals Against Label` = paste(Country, goals_against, sep = "\n"),
    Defeats = Defeats,
    `Defeats Label` = paste(Country, Defeats, sep = "\n"),
    Draws = Draws,
    `Draws Label` = paste(Country, Draws, sep = "\n"),
    tree_full = inferno(33),
    tree_grey = "grey"
  )

plot_data <- euro_data_summary %>%
  filter(tournaments_played > 4) %>%
  mutate(`Goal Ratio` = goal_ratio, 
         `Goal Difference` = goal_difference, 
         `Goals per Game` = av_goals_for, 
         `Goals against per Game` = av_goals_against)

# create the UI
ui <- fluidPage(
  
  fluidRow(
    h1("Summary of all European Football Championship Tournaments since 1960", align = "center"),
    br()
  ),

    fluidRow(
        column(width = 8,
               wellPanel(style = "padding: 5px;",
                 
                 
                 leaflet::leafletOutput(outputId = "myMap", height = 850),
                 
                 actionButton( inputId = "clearHighlight"
                             , icon = icon( name = "eraser")
                             , label = "Clear the Map"
                             , width = "100%"
                             , style = "color: #fff; background-color: #BB3754FF; border-color: #BB3754FF"
      )
  
      )
  ),
  column(width = 4,
         wellPanel(style = "padding: 25px;",
                   h3("Exploring Football Data", align = "center"),
                   br(),
                   p("This dashboard allows you to compare how well different nations have done over the last decades 
                     in European Football. Just click on the countries on the map you are interested in and scroll down 
                     to view the graphs. Unfortunately, you can only select countries which have taken part in at least 
                     5 tournaments. I hope nobody takes any offence/offense given this highly emotional topic (no pun 
                     intended, I genuinely had to google the right spelling)."),
                   p("Hover over graphs or use the drop down menus to view different variables to find interesting facts. 
                     For example: While Germany scored the most goals across all tournaments it also conceded the most."),
                   hr(),
                   plotlyOutput("podium", height = 495)
                   )
         ),
  #column(width = 4,
  #       wellPanel(style = "padding: 5px;",
  #       plotlyOutput("games", height = 900)))
  ),
  fluidRow(
    column(width = 8, 
           wellPanel(style = "padding: 5px;",
                     selectInput("lollipop_variable", "Different Goal Metrics by Country", 
                                 choices = c("Goal Ratio", "Goal Difference", "Goals per Game", "Goals against per Game")),
                     plotOutput("lollipop", height = 380),
                     hr(),
                     selectInput("tree_variable", "Country share of overall Wins/Goals", 
                                 choices = c("Wins", "Draws", "Defeats","Goals For", "Goals Against")),
                     
                     plotOutput("tree", height = 380)
           )
           ),
    column(width = 4,
           wellPanel(style = "padding: 5px;",
                     
           plotlyOutput("games", height = 960)
           )
           )
  )
) # end of fluid page

# create the server
server <- function( input, output, session ){
  
  # create foundational map
  foundational.map <- shiny::reactive({
    leaflet() %>%
      addTiles( urlTemplate = "https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png") %>%
      setView( lng = 	18.682127
               , lat = 55.110924
               , zoom = 4 ) %>%
      addPolygons( data = euro_countries
                   , fillOpacity = 0
                   , opacity = 0.2
                   , color = "#BB3754FF"
                   , weight = 2
                   , layerId = euro_countries$NAME
                   , group = "click.list"
      )
  })
  
  output$myMap <- renderLeaflet({
    
    foundational.map()
    
  }) # end of leaflet::renderLeaflet({})
  
  # store the list of clicked polygons in a vector
  click.list <- shiny::reactiveValues( ids = vector() )
  
  # observe where the user clicks on the leaflet map
  # during the Shiny app session
  # Courtesy of two articles:
  # https://stackoverflow.com/questions/45953741/select-and-deselect-polylines-in-shiny-leaflet
  # https://rstudio.github.io/leaflet/shiny.html
  shiny::observeEvent( input$myMap_shape_click, {
    
    # store the click(s) over time
    click <- input$myMap_shape_click
    
    # store the polygon ids which are being clicked
    click.list$ids <- c( click.list$ids, click$id )
    
    # filter the spatial data frame
    # by only including polygons
    # which are stored in the click.list$ids object
    lines.of.interest <- euro_countries[ which( euro_countries$NAME %in% click.list$ids ) , ]
    
    # if statement
    if( is.null( click$id ) ){
      # check for required values, if true, then the issue
      # is "silent". See more at: ?req
      req( click$id )
      
    } else if( !click$id %in% lines.of.interest@data$id ){
      
      # call the leaflet proxy
      leaflet::leafletProxy( mapId = "myMap" ) %>%
        # and add the polygon lines
        # using the data stored from the lines.of.interest object
        addPolylines( data = lines.of.interest
                      , layerId = lines.of.interest@data$id
                      , color = "#BB3754FF"    
                      , weight = 5
                      , opacity = 1
        ) 
      
    } # end of if else statement
    
    
  }) # end of shiny::observeEvent({})
  
  
  # Create the logic for the "Clear the map" action button
  # which will clear the map of all user-created highlights
  # and display a clean version of the leaflet map
  shiny::observeEvent( input$clearHighlight, {
    
    # recreate $myMap
    output$myMap <- leaflet::renderLeaflet({
      
      # first
      # set the reactive value of click.list$ids to NULL
      click.list$ids <- NULL
      
      # second
      # recall the foundational.map() object
      foundational.map()
      
    }) # end of re-rendering $myMap
    
  }) # end of clearHighlight action button logic
  
  output$podium <- renderPlotly({
    podium_plot <- podium_data %>%
      ggplot(aes(Country, Titles, fill = Rank)) +
      geom_bar(position="stack", stat="identity") +
      #ggtitle("Most Successful Countries") +
      scale_fill_manual(values = c("#781C6DFF", "#FCB519FF")) +
      #theme_ipsum() +
      #theme_bw() +
      theme(legend.position = "none", 
            panel.grid.major = element_line(colour = "black"),
            panel.grid.major.x = element_blank(),
            plot.margin = margin(1, 0.5, 0, 0, "cm"),
            panel.background = element_blank(),
            axis.ticks = element_blank()
            ) +
      labs(x = "", y = "Number of Titles \n") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) 

    ggplotly(podium_plot) %>%
      layout(title = "Most successful Countries")
  })
  
  output$games <-renderPlotly({
    game_plot <- game_statistics %>%
      filter(Country %in% click.list$ids) %>%
      ggplot(aes(Number, Country, fill = `Game Result`)) +
      geom_bar(position="stack", stat="identity") +
      scale_fill_manual(values = c("#00000000", "#000004FF", "#FCB519FF", "#781C6DFF", "#00000000")) +
      ggtitle("Countries by Game Results") +
      #theme_ipsum() +
      theme(legend.position = "none", 
            panel.grid.major = element_line(colour = "black"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(hjust = 1),
            plot.margin = margin(1, 0.5, 0, 0, "cm"),
            panel.background = element_blank(),
            axis.ticks = element_blank()
            # legend.text = element_text(c("", "Wins", "Draws", "Defeats", ""))
      ) +
      coord_cartesian(xlim = c(0,50)) +
      labs(x = "", y = "")
    
    ggplotly(game_plot) %>%
      layout(title = "Countries by Game Results")
    
  })
  
  output$tree <-renderPlot({
    tree_data %>% 
      ggplot(aes(area = eval(as.symbol(input$tree_variable)), fill = Country, subgroup = Country, label = eval(as.symbol(paste0(input$tree_variable, " Label")))), colour = "white") +
      geom_treemap(show.legend = FALSE) + 
      scale_fill_manual(values = ifelse(tree_data$Country %in% click.list$ids, tree_data$tree_full, tree_data$tree_grey)) +
      geom_treemap_text(color="white", place = "centre") +
      geom_treemap_subgroup_border(colour = "white", size = 3) +
      theme(legend.position = "none")
  })
  
  output$lollipop <-renderPlot({
    ggplot(plot_data, aes(reorder(Country, eval(as.symbol(input$lollipop_variable))), eval(as.symbol(input$lollipop_variable)))) +
      geom_segment(aes(x = reorder(Country, eval(as.symbol(input$lollipop_variable))), xend = Country, y = 0, yend = eval(as.symbol(input$lollipop_variable))), colour=ifelse(plot_data$Country %in% click.list$ids, custom_red, "grey"), size=ifelse(plot_data$Country %in% click.list$ids, 2, 1) ) +
      geom_point(colour=ifelse(plot_data$Country %in% click.list$ids, custom_red, "grey"), size=ifelse(plot_data$Country %in% click.list$ids, 8, 4) ) +
      #theme_ipsum() +
      theme(
        legend.position="none", 
        panel.grid.major = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(1,0.5,0,0, "cm"),
        panel.background = element_blank(),
        text = element_text(size=20),
        axis.ticks = element_blank()
      ) +
      #ggtitle(paste("Countries by", input$lollipop_variable))+
      labs(x = "", y = "")
  })
} # end of server

## run shinyApp ##
shiny::shinyApp( ui = ui, server = server)

# end of script #