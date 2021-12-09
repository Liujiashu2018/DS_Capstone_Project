library(shiny)
library(rgdal)
library(dygraphs)
library(sf)
library(dplyr)
library(leaflet)
library(ggplot2)
library(reshape2)
library(readxl)
library(plotly) 
library(janitor)
library(DT)
library(tidyverse)    
library(lubridate)
library(bslib)

Total_state_2019 <- read_excel("/Users/jiashuliu/Desktop/Advanced Data Science in R/Advanced-Data-Science-in-R/DS_Capstone_Project/ACS Data/Total_state_2019.xlsx")
state<-st_read("/Users/jiashuliu/Desktop/tl_2019_us_state", quiet = TRUE)
state_map<-
  state %>%
  mutate(State=NAME) %>%
  relocate(State, .before=NAME) %>%
  select(-NAME) %>%
  left_join(Total_state_2019, by="State") %>% 
  select("State", "INTPTLAT", "INTPTLON", "FIPS", "Total population", "Hispanic or Latino (of any race)", 
         "White alone", "Black or African American alone", "American Indian and Alaska Native alone", "Asian alone",
         "Native Hawaiian and Other Pacific Islander alone", "High school graduate or higher", 
         "Unemployment_rate", "Per capita income (dollars)", "Civilian noninstitutionalized population",
         "No health insurance coverage") %>% 
  mutate(Hispanic_p=(`Hispanic or Latino (of any race)`/`Total population`),
         White_p=(`White alone`/`Total population`),
         Black_p=(`Black or African American alone`/`Total population`),
         Asian_p=(`Asian alone`/`Total population`),
         Native_p=(`American Indian and Alaska Native alone`/`Total population`),
         Hawaiian_p=(`Native Hawaiian and Other Pacific Islander alone`/`Total population`), 
         Highschool_rate=(`High school graduate or higher`/`Total population`), 
         Uninsured_rate=(`No health insurance coverage`/`Civilian noninstitutionalized population`)) %>% 
  clean_names()

display_table<-state_map %>% 
  select(state, hispanic_p,white_p, black_p, asian_p, native_p, hawaiian_p) %>% 
  clean_names()
  
ui <- fluidPage(
  titlePanel(p("My Map!", style = "color:#3474A7")),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "variableselected",
        label = "Select variable",
        choices = c("total_population", "highschool_rate", "uninsured_rate", "per_capita_income_(dollars)")
      )
    ),
    
    mainPanel(
      fluidRow(
        column(
          width = 10,
          leafletOutput(outputId = "map", width="100%")
        ),
        column(
          width = 10,
          height = 12, 
          plotlyOutput("plot")
        )
      ),
      DT::dataTableOutput("mytable")
    )
  )
)

server <- function(input, output){
  
  output$mytable = DT::renderDataTable({
    datatable(display_table) %>% 
      formatPercentage(c('hispanic_p','white_p', 'black_p', 'asian_p', 'native_p', 'hawaiian_p'), 2)
    })
  
  output$plot <- renderPlotly({
    p<-ggplot( data = state_map, aes(x=(Uninsured_rate), 
                                  y= fct_reorder(state, 
                                                 Uninsured_rate,
                                                 .desc=FALSE)), fill= "#DAA520")+
      geom_col() + 
      labs(x="Uninsured Rate",
           y="State",
           title = "2019 Uninsured Rate by State")
    
    ggplotly(p)
    
  })
  
  output$map <- renderLeaflet({
    
    #map$variableplot <- as.numeric(
    #state_map[, input$variableselected]
    #)
    #labels <- sprintf("%s: %g", map$State, map$variableplot) %>%
    #lapply(htmltools::HTML)
    pos <- match(input$variableselected,names(state_map))
    # find the column number of the colorvariable
    
    pal <- colorNumeric("viridis", domain = state_map %>% pull(pos))
    #domain = state_map %>% pull(.data[[input$variableselected]]))
    #domain = map$variableselected) 
    popup_sb <- paste0(state_map$state, sep = ": ", as.character(state_map %>% pull(pos)))
    
    leaflet(state_map) %>% 
      addTiles() %>% 
      addPolygons(
        #skips drawing the borders:
        stroke = FALSE, 
        #fills according to variable of hex colors:
        fillColor = ~pal(state_map %>% pull(pos)), #~pal(total_population),
        #~pal(.data[[input$variableselected]]), 
        #changes transparency, like alpha in ggplot
        fillOpacity = 0.7, 
        #how much to simplify the plot when zooming:
        smoothFactor = 0.5, 
        #changes what happens to the shape when we mouse over it
        highlight = highlightOptions(weight = 5, 
                                     color = "black",
                                     fillOpacity = 0.9,
                                     bringToFront = FALSE),
        popup=~popup_sb) %>% 
    leaflet::addLegend(
    pal = pal, values = ~state_map %>% pull(pos),
    opacity = 0.7, title = NULL
    ) 
    
  })}
shinyApp(ui=ui, server=server)