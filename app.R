##### COVID-19 Tracker | Ethiopia #####
## Nina Tabinaeva

## includes code adapted from the following sources:
# https://github.com/DrFabach/Corona/blob/master/app.r
# https://gist.github.com/GuillaumePressiat/0e3658624e42f763e3e6a67df92bc6c5
# https://github.com/fverkroost/RStudio-Blogs/blob/master/interactive_worldmap_shiny_app.R
# https://github.com/eparker12/nCoV_tracker/blob/master/app.R


# install packages and libraries
devtools::install_github("ropenscilabs/rnaturalearthdata")
if(!require(rnaturalearthhires)) install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "source")
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")

library(sp)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(rgeos)
library(ggspatial)
library(tidyverse)
library(shiny)
library(leaflet)
library(RColorBrewer)


### DATA PROCESSING ###

# download WHO COVID-19 global data
url <- "https://covid19.who.int/WHO-COVID-19-global-data.csv"
dest_file <- "data/dailydata.csv"
download.file(url, destfile=dest_file)


# import  data and extract regional and ethiopia data
data_raw <- read.csv("data/dailydata.csv")
data_raw$Country <- as.character(data_raw$Country)

data_eth <- data_raw %>% filter(Country=="Ethiopia")
data_dji <- data_raw %>% filter(Country=="Djibouti")
data_ssu <- data_raw %>% filter(Country=="South Sudan")
data_eri <- data_raw %>% filter(Country=="Eritrea")
data_sud <- data_raw %>% filter(Country=="Sudan")
data_ken <- data_raw %>% filter(Country=="Kenya")
data_uga <- data_raw %>% filter(Country=="Uganda")
data_som <- data_raw %>% filter(Country=="Somalia")

data_hoa <- rbind(data_dji, data_eri, data_eth, data_ken, data_som, data_ssu, data_sud, data_uga)
data_hoa$Date_reported = as.Date(data_hoa$Date_reported) # format dates

# load regional map and population data
map_hoa <- ne_countries(country = c("Ethiopia", "Sudan", "Kenya", "Uganda", "Djibouti",
                                "South Sudan", "Eritrea", "Somalia"), returnclass= "sf") 

map_hoa <- map_hoa %>% rename(Country=name) # to match with data_hoa
map_hoa$Country[map_hoa$Country == "S. Sudan"] <- "South Sudan"
map_hoa$pop_est[map_hoa$pop_est == "516055"] = "973560" #Djibouti (source: WB, 2019)
map_hoa$pop_est[map_hoa$pop_est == "5647168"] = "3213972" #Eritrea (source: WB, 2011)
map_hoa$pop_est[map_hoa$pop_est == "85237338"] = "112078730" #Ethiopia (source: WB, 2019)
map_hoa$pop_est[map_hoa$pop_est == "39002772"] = "52573973" #Kenya (source: WB, 2019)
map_hoa$pop_est[map_hoa$pop_est == "25946220"] = "42813238" #Sudan (source: WB, 2019)
map_hoa$pop_est[map_hoa$pop_est == "10625176"] = "11062113" #South Sudan (source: WB, 2019)
map_hoa$pop_est[map_hoa$pop_est == "9832017"] = "15442905" #Somalia (source: WB, 2019)
map_hoa$pop_est[map_hoa$pop_est == "32369558"] = "44269594" #Uganda (source: WB, 2019)

map_hoa$pop_est <- as.integer(map_hoa$pop_est)

map_hoa <- arrange(map_hoa, Country)

map_hoa <- map_hoa %>% select(Country,pop_est,geometry)

# set CRS
map_hoa <- st_set_crs(map_hoa, "+proj=longlat +datum=WGS84")

# create longitude and latitude
map_hoa <- cbind(map_hoa, st_coordinates(st_centroid(map_hoa$geometry)))
map_hoa <- map_hoa %>% rename(longitude=X)
map_hoa <- map_hoa %>% rename(latitude=Y)
map_hoa_clean <- map_hoa %>% select(Country, longitude, latitude, geometry)

# merge data frames 
full_data <- merge(map_hoa, data_hoa, by = "Country")

# set dates
full_data$Date_reported = as.Date(full_data$Date_reported)
min_date = as.Date(min(full_data$Date_reported), "%Y-%m-%d")
current_date = as.Date(max(full_data$Date_reported),"%Y-%m-%d")
current_date_clean = format(as.POSIXct(current_date), "%d %B %Y")  

data_eth$Date_reported = as.Date(data_eth$Date_reported)

# create data - cases and deaths per 100,000
full_data <- full_data %>%
  mutate(Cases_per=full_data$Cumulative_cases/(full_data$pop_est/100000))
full_data <- full_data %>%
  mutate(Deaths_per=full_data$Cumulative_deaths/(full_data$pop_est/100000))
full_data$Cases_per = as.numeric(round(full_data$Cases_per, digits = 1))
full_data$Deaths_per = as.numeric(round(full_data$Deaths_per, digits = 1))

# create data frame with no spatial data
covid_data <- full_data %>% select(Country, Date_reported, New_cases, Cumulative_cases, New_deaths, Cumulative_deaths, Cases_per, Deaths_per)
covid_data <- covid_data %>% st_drop_geometry()

# create variable for today's data
full_data_today = subset(full_data, Date_reported==current_date)
covid_data_today = covid_data %>% filter(Date_reported == current_date)

# check SRC: identical(map_hoa_clean$src, full_data$src)



### MAP parameters ###

bins = c(0,10,50,100,150,200,Inf)
pal <- colorBin("PuRd", domain = covid_data$Cases_per, bins = bins)



### PLOT FUNCTIONS ###

# daily trend: new cases plot
cases_agg = aggregate(data_eth$New_cases, by=list(Category=data_eth$Date_reported), FUN=sum)
names(cases_agg) = c("date", "cases")

first_date <- as.Date("2020-03-14") # date of the first case in Ethiopia

new_cases_plot = function (cases_agg, plot_date) {
  plot_cases = subset(cases_agg, date>=first_date & date<=plot_date)
  g1 = ggplot(plot_cases, aes(x=date, y=cases, color = "#980043"), fill= "transparent") +
    geom_line() + geom_point(size =0.4, alpha = 0.8) +
    ylab(NULL) + xlab("Date") + theme_bw() +
    scale_colour_manual(values = c("#980043")) +
    #scale_y_continuous(labels = function(1) {trans = 1 / 1000; paste0(trans,"M")}) +
    theme(plot.background = element_blank(), panel.background = element_blank(), 
          legend.title = element_blank(), legend.position = "none", plot.title = element_text(size=9),
          plot.margin = margin(5,12,5,5))
  g1
}

# daily trend: new deaths plot
deaths_agg = aggregate(data_eth$New_deaths, by=list(Category=data_eth$Date_reported), FUN=sum)
names(deaths_agg) = c("date", "deaths")

new_deaths_plot = function (deaths_agg, plot_date) {
  plot_deaths = subset(deaths_agg, date>=first_date & date<=plot_date)
  g1 = ggplot(plot_deaths, aes(x=date, y=deaths, color = "#980043"),fill= "transparent")+
    geom_line() + geom_point(size =0.4, alpha = 0.8) +
    ylab(NULL) + xlab("Date") + theme_bw() +
    scale_colour_manual(values = c("#980043")) +
    #scale_y_continuous(labels = function(1) {trans = 1 / 100; paste0(trans,"M")}) +
    theme(plot.background = element_blank(), panel.background = element_blank(), 
          legend.title = element_blank(), legend.position = "none", plot.title = element_text(size=9),
          plot.margin = margin(5,12,5,5))
  g1
}

# test functions:
# new_cases_plot(cases_agg, current_date)
# new_deaths_plot(deaths_agg, current_date)



### SHINY ###

# UI #

ui <- bootstrapPage(
  
    leafletOutput("mymap", width ="100%", height = "100%"),
  
    tags$head(
    tags$style(type = "text/css", "html, body {width:100%;height:100%;}",
    HTML(".panel-default {
                    @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@100;300;400&display=swap');
                    padding: 20px;
                    h3 {font-weight: 400; font-family: 'Roboto', sans-serif;}
                    h4 {font-weight: 300; font-family: 'Roboto', sans-serif;}
                    h5 {font-weight: 300; font-family: 'Roboto', sans-serif;}
                    h6 {font-weight: 100; font-family: 'Roboto', sans-serif;}
                  
                    }"))),

  absolutePanel(class = "panel panel-default", top = 50, left = 60, 
                draggable = TRUE, width = 353, fixed = TRUE, height = "auto", 
                titlePanel(h3("COVID-19 Tracker  |  Ethiopia")),
                
                style = "background-color: white; opacity: 0.7;",
                
                h5(textOutput("reactive_ethtotal_cases"), align = "right"),
                h5(textOutput("reactive_ethtotal_deaths"), align = "right"),
                h6(textOutput("reactive_clean_date"), align = "right"),
                
                sliderTextInput("plot_date", h6("DATE"),
                                choices = format(unique(data_eth$Date_reported), "%d %b %y"),
                                selected = format(current_date,"%d %b %y"),
                                grid = FALSE,
                                width = "100%",
                                animate = animationOptions(interval = 1800, loop=FALSE)),
                
                h6("DAILY TRENDS", align = "left"),
                
                h6(textOutput("reactive_new_cases"), align = "left"),
                
                plotOutput("reactive_new_cases_plot", height = "150px", width ="100%"),
                
                h6(textOutput("reactive_new_deaths"), align = "left"),
                
                plotOutput("reactive_new_deaths_plot",height = "150px", width ="100%")
  )
)


# SERVER #

server = function(input, output, session) {
  
formatted_date <- reactive({
  format(as.Date(input$plot_date, format= "%d %b %y"), "%Y-%m-%d")
  })


# create a basemap
output$mymap <- renderLeaflet({
  leaflet(data = map_hoa_clean) %>%

        addProviderTiles("CartoDB", options = providerTileOptions(opacity=1, minZoom=2, maxZoom = 6), 
                     group = "Open Street Map") %>%
    
    setView(lng = 35, lat = 9, zoom = 6) %>%
    
    addMapPane("polygon", zIndex = 430)%>%
    addMapPane("circles", zIndex = 410)%>%
    
    addLayersControl(
      position = "bottomright",
      overlayGroups = "Deaths total per 100,000",
      options = layersControlOptions(collapsed=FALSE)) %>%
     #hideGroup("Deaths total per 100,000") %>%
    
    addLegend("topright", pal=pal, values = covid_data$Cases_per, 
              title = "<small>Cases total per 100,000</small>", opacity = 0.7)
})


reactive_covid_data <- reactive({
  full_data[full_data$Date_reported == formatted_date(), ]
  })
  
  observe({
    
      covid <- reactive_covid_data()
   
     leafletProxy("mymap", data = covid) %>%
      clearMarkers() %>%
      clearShapes() %>%
  
      addPolygons(stroke = 1, smoothFactor = 0.2, weight = 0.3, color = "#df65b0", 
                  opacity = 0.5, fillOpacity = 0.2, fillColor = ~pal(reactive_covid_data()$Cases_per),
                  
                  label = sprintf("<strong>%s </strong>(total)<br/>Cases: %g<br/>Deaths: %g<br/>Cases per 100,000: %g<br/>Deaths per 100,000: %g", 
                                  reactive_covid_data()$Country, 
                                  reactive_covid_data()$Cumulative_cases, 
                                  reactive_covid_data()$Cumulative_deaths, 
                                  reactive_covid_data()$Cases_per, 
                                  reactive_covid_data()$Deaths_per) %>% lapply(htmltools::HTML),
                  
                  labelOptions = labelOptions(
                    style = list("font-weight" = "light", padding = "2px 7px", "color" = "#444444"),
                    textsize = "13px", direction = "auto"),
                  highlight = highlightOptions(
                    color = "black",
                    bringToFront = TRUE),
                  options = leafletOptions(pane = "polygon"))%>%
      
      addCircleMarkers(data=reactive_covid_data(), lat = ~ latitude, lng = ~longitude, weight = 1, radius = ~(Deaths_per)*10,
                       fillOpacity = 0.3, color = "darkgray", group = "Deaths total per 100,000",
                       options = leafletOptions(pane = "circles"))
  })

  
  output$reactive_clean_date <- renderText({
    format(as.POSIXct(formatted_date()), "%d %B %Y")
  })
  
  reactive_data_eth <- reactive({
    data_eth %>% filter(Date_reported == formatted_date())
  })
  
  output$reactive_ethtotal_cases <- renderText({
    paste0(prettyNum(reactive_data_eth()$Cumulative_cases, big.mark=","), " cumulative cases")
  })
  
  output$reactive_ethtotal_deaths <- renderText({
    paste0(prettyNum(reactive_data_eth()$Cumulative_deaths, big.mark=","), " cumulative deaths")
  })  
  
  output$reactive_new_cases <- renderText({
    paste0(prettyNum(reactive_data_eth()$New_cases, big.mark=","), " new cases")
  })
  
  output$reactive_new_deaths <- renderText({
    paste0(prettyNum(reactive_data_eth()$New_deaths, big.mark=","), " new deaths")
  })
  
  output$reactive_new_cases_plot <- renderPlot({
    new_cases_plot(cases_agg, formatted_date())
  })
  
  output$reactive_new_deaths_plot <- renderPlot({
    new_deaths_plot(deaths_agg, formatted_date())
  })
  
}

### RUN APP ###
shinyApp(ui, server)
