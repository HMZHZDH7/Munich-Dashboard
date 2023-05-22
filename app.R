library(tidyverse)
library(shiny)
library(plotly)
#library(styler)
#library(lintr)
#library(shinylogs)
library(DT)
#library(shinydashboard)
#library(ggforce)
#library(bslib)

#Utils
#source("utils/dataHandlerQI.R", local = T)
source("utils/dataLoader.R", local = T)
source("utils/QILoader.R", local = T)

#Element modules
source("modules/plot_Expanded.R", local = T)
#If you want to have the logs folder prompted on application stop
#onStop(function() {
#  browseURL(url = "logs")
#})

#Load hospital data, check data/dataREanonymized.csv and utils/dataLoader.R to see what data is being loaded and how.
db <- dataLoader()
numVars <- db$numVars
numVars <- numVars %>% filter(YQ!="2016 Q1", YQ!="2016 Q2", YQ!="2016 Q3", YQ!="2016 Q4", 
                              YQ!="2017 Q1", YQ!="2017 Q2", YQ!="2017 Q3", YQ!="2017 Q4")
#catVars <- db$catVars

#Load QI data, check data/QI_info.csv and utils/QILoader.R to see what data is being loaded and how.
QI_db <- QILoader()

#view(numVars)
#view(catVars)
#view(QI_db)
QI_name <- reactiveVal("Door-to-imaging time")
QI_agg <- reactiveVal("median")
QI_trend <- reactiveVal(FALSE)
QI_error <- reactiveVal(FALSE)
QI_gender <- reactiveVal(c(0, 1))
QI_prenotification <- reactiveVal(c(1, 0))
QI_imaging <- reactiveVal(c(1, 0))
QI_mrs <- reactiveVal(c(0:6))
QI_filterminmax <- reactiveVal(NULL)
QI_filterquarts <- reactiveVal(NULL) 
compared_hospitals <- reactiveVal(NULL)
compare_national <- reactiveVal(TRUE)

QI_name_dist <- reactiveVal("Door-to-imaging time")
QI_mean_dist <- reactiveVal(FALSE)
QI_median_dist <- reactiveVal(FALSE)
compared_hospitals_dist <- reactiveVal(NULL)
compare_national_dist <- reactiveVal(TRUE)
QI_filterminmax_dist <- reactiveVal(NULL)
QI_filterquarts_dist <- reactiveVal(NULL) 
QI_gender_dist <- reactiveVal(c(0, 1))
QI_prenotification_dist <- reactiveVal(c(1, 0))
QI_imaging_dist <- reactiveVal(c(1, 0))
QI_mrs_dist <- reactiveVal(c(0:6))

QI_name_x_corr <- reactiveVal("Door-to-imaging time")
QI_name_y_corr <- reactiveVal("Modified ranking scale discharge")

QI_name_comp <- reactiveVal("Door-to-imaging time")

hospitals <- numVars %>% filter(site_name!="Samaritan", site_name!="Memorial", site_name!="Progress") 
hospitals <- unique(hospitals$site_name)
quarts <- numVars$YQ

ui <- fluidPage(
  plot_Expanded_UI("Dashboard", QI_db$INDICATOR, hospitals)
)

server <- function(input, output, session) {
  plot_Expanded("Dashboard",QI_db)
}

shinyApp(ui = ui, server = server)
