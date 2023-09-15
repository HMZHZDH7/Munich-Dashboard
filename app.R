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

db <- dataLoader()
numVars <- db$numVars
numVars <- numVars %>% filter(YQ!="2016 Q1", YQ!="2016 Q2", YQ!="2016 Q3", YQ!="2016 Q4", 
                              YQ!="2017 Q1", YQ!="2017 Q2", YQ!="2017 Q3", YQ!="2017 Q4")

hospitals <- numVars %>% filter(site_name!="Samaritan", site_name!="Memorial", site_name!="Progress") 
hospitals <- unique(hospitals$site_name)
#catVars <- db$catVars

#Load QI data, check data/QI_info.csv and utils/QILoader.R to see what data is being loaded and how.
QI_db <- QILoader()

quarts <- numVars$YQ

ui <- fluidPage(
  plot_Expanded_UI("Dashboard", QI_db$INDICATOR, hospitals)
)

server <- function(input, output, session) {
  #Load hospital data, check data/dataREanonymized.csv and utils/dataLoader.R to see what data is being loaded and how.
  
  #view(numVars)
  #view(catVars)
  #view(QI_db)
  session$userData$QI_reactive_values <- reactiveValues()
  
  session$userData$QI_reactive_values$QI_name <- "Door-to-imaging time"
  session$userData$QI_reactive_values$QI_agg <- "median"
  session$userData$QI_reactive_values$QI_trend <- FALSE
  session$userData$QI_reactive_values$QI_error <- FALSE
  session$userData$QI_reactive_values$QI_gender <- c(0, 1)
  session$userData$QI_reactive_values$QI_prenotification <- c(1, 0)
  session$userData$QI_reactive_values$QI_imaging <- c(1, 0)
  session$userData$QI_reactive_values$QI_mrs <- c(0:6)
  session$userData$QI_reactive_values$QI_filterminmax <- NULL
  session$userData$QI_reactive_values$QI_filterquarts <- NULL
  session$userData$QI_reactive_values$compared_hospitals <- NULL
  session$userData$QI_reactive_values$compare_national <- FALSE
  session$userData$QI_reactive_values$QI_displayaspercentage <- NULL
  session$userData$QI_reactive_values$QI_ylab <- NULL
    
  session$userData$QI_reactive_values$QI_name_dist <- "Door-to-imaging time"
  session$userData$QI_reactive_values$QI_mean_dist <- FALSE
  session$userData$QI_reactive_values$QI_median_dist <- FALSE
  session$userData$QI_reactive_values$compared_hospitals_dist <- NULL
  session$userData$QI_reactive_values$compare_national_dist <- FALSE
  session$userData$QI_reactive_values$QI_filterminmax_dist <- NULL
  session$userData$QI_reactive_values$QI_filterquarts_dist <- NULL
  session$userData$QI_reactive_values$QI_gender_dist <- c(0, 1)
  session$userData$QI_reactive_values$QI_prenotification_dist <- c(1, 0)
  session$userData$QI_reactive_values$QI_imaging_dist <- c(1, 0)
  session$userData$QI_reactive_values$QI_mrs_dist <- c(0:6)
  session$userData$QI_reactive_values$QI_xlab_dist <- NULL
    
  session$userData$QI_reactive_values$QI_name_x_corr <- "Door-to-imaging time"
  session$userData$QI_reactive_values$QI_name_y_corr <- "Modified ranking scale discharge"
  session$userData$QI_reactive_values$QI_filterminmax_x_corr <- NULL
  session$userData$QI_reactive_values$QI_filterminmax_y_corr <- NULL
  session$userData$QI_reactive_values$QI_filterquarts_corr <- NULL
  session$userData$QI_reactive_values$QI_gender_corr <- c(0, 1)
  session$userData$QI_reactive_values$QI_prenotification_corr <- c(1, 0)
  session$userData$QI_reactive_values$QI_imaging_corr <- c(1, 0)
  session$userData$QI_reactive_values$QI_mrs_corr <- c(0:6)
  session$userData$QI_reactive_values$QI_split_corr<- NULL
  session$userData$QI_reactive_values$QI_trend_corr <- FALSE
  session$userData$QI_reactive_values$QI_xlab_corr <- NULL
  session$userData$QI_reactive_values$QI_ylab_corr <- NULL
  session$userData$QI_reactive_values$QI_splitlab_corr <- NULL
    
  session$userData$QI_reactive_values$QI_name_comp <- "Door-to-imaging time"
  session$userData$QI_reactive_values$QI_split_comp<- NULL
  session$userData$QI_reactive_values$QI_filterminmax_comp <- NULL
  session$userData$QI_reactive_values$QI_filterquarts_comp <- NULL
  session$userData$QI_reactive_values$QI_ylab_comp <- NULL
  session$userData$QI_reactive_values$QI_xlab_comp <- NULL
  
  plot_Expanded("Dashboard",QI_db)
}

shinyApp(ui = ui, server = server)
