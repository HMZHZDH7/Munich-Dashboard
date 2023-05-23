plot_Expanded_UI <- function(id, database, hospitals, quarts) {
  ns <- NS(id)
  
  #Storing the plot under this variable
  plot_UI <- tagList(
    plotlyOutput(
      outputId = ns("plot")
    ),
    
    
    
    #Define a row containing dropdown menus to select variables & tickboxes for more plot options.
    fixedRow(
      column(3,
             h3("Plot characteristics"),     
             selectInput(
               inputId = ns("selected_col"),
               label = h6("Select y-axis variable"),
               choices = database,
               selected = "Door-to-imaging time"),
             
             selectInput(
               inputId = ns("selected_colx"),
               label = h6("Select aggregation type"),
               choices = c("median", "mean", "standard deviation", "minimum", "maximum"),
               selected = "median"),
          
             
             h6("Show trend"),
             checkboxInput(inputId = ns("selected_trend"), "Show trend line", value = FALSE),
             
             h6("Show error bars"),
             checkboxInput(inputId = ns("selected_error"), "Show error bar", value = FALSE),
      ),
      column(3, 
             h3("Compare"), 
             
             h6("Compare with counry"),
             checkboxInput(inputId = ns("selected_natcomparisons"), "Show national value", value = FALSE),
             
             checkboxGroupInput(
               inputId = ns("selected_comparisons"),
               label = h6("Compare with hospitals"),
               choices = hospitals,
               selected = NULL)  
      ),
      column(3, 
             h3("Filter by subgroups"), 
             
             checkboxGroupInput(
               inputId = ns("selected_genders"),
               label = h6("Genders shown in plot:"),
               choices = c("Female", "Male"),
               selected = c("Female", "Male")),  
             
             checkboxGroupInput(
               inputId = ns("selected_imagingdone"),
               label = h6("Imaging of patients shown in plot"),
               choices = c("Done", "Not done"),
               selected = c("Done", "Not done")),  
             
             checkboxGroupInput(
               inputId = ns("selected_prenotification"),
               label = h6("Prenotification of patients in plot"),
               choices = c("Prenotified", "Not prenotified"),
               selected = c("Prenotified", "Not prenotified")),
             
             checkboxGroupInput(
               inputId = ns("selected_mrs"),
               label = h6("mRS of patients in plot"),
               choices = c(0:6),
               selected = c(0:6))  
      ),
      column(3,
             h3("Filter by values"),     
             sliderInput(ns("slider_minmax"), label = h6("Filter values of y-axis variable (slider shows range of values in y-axis variable)"),
                         min = 0, max = 100, value = c(0, 100)),
             
             checkboxGroupInput(
                inputId = ns("selected_quarts"),
                label = h6("Quarters shown in plot:"),
                choices = c("2018 Q1", "2018 Q2", "2018 Q3", "2018 Q4",
                            "2019 Q1", "2019 Q2", "2019 Q3", "2019 Q4",
                            "2020 Q1", "2020 Q2", "2020 Q3", "2020 Q4",
                            "2021 Q1", "2021 Q2", "2021 Q3", "2021 Q4",
                            "2022 Q1", "2022 Q2", "2022 Q3", "2022 Q4"),
                selected = c("2018 Q1", "2018 Q2", "2018 Q3", "2018 Q4",
                             "2019 Q1", "2019 Q2", "2019 Q3", "2019 Q4",
                             "2020 Q1", "2020 Q2", "2020 Q3", "2020 Q4",
                             "2021 Q1", "2021 Q2", "2021 Q3", "2021 Q4",
                             "2022 Q1", "2022 Q2", "2022 Q3", "2022 Q4")), 
             ),
      
           
    ),
    
    
    
    
  )
  
  #Storing the datatable output under this variable
  dist_UI <- tagList(
    plotlyOutput(
      outputId = ns("distPlot")
    ),
    
    fixedRow(
      column(3,
             h3("Plot characteristics"),     
             selectInput(
               inputId = ns("selected_col_dist"),
               label = h6("Select y-axis variable"),
               choices = database,
               selected = "Door-to-imaging time"),
             
             h6("Show mean"),
             checkboxInput(inputId = ns("selected_mean_dist"), "Show mean line", value = FALSE),
             
             h6("Show median"),
             checkboxInput(inputId = ns("selected_median_dist"), "Show median line", value = FALSE),
      ),
      column(3, 
             h3("Compare"), 
             
             h6("Compare with counry"),
             checkboxInput(inputId = ns("selected_natcomparisons_dist"), "Show national value", value = FALSE),
             
             selectInput(
               inputId = ns("selected_comparisons_dist"),
               label = h6("Compare with hospitals"),
               choices = c("None", "Paradise", "Angelvale", "Rose", "General", "Mercy", "Hope"),
               selected = NULL)  
      ),
      column(3, 
             h3("Filter by subgroups"), 
             
             checkboxGroupInput(
               inputId = ns("selected_genders_dist"),
               label = h6("Genders shown in plot:"),
               choices = c("Female", "Male"),
               selected = c("Female", "Male")),  
             
             checkboxGroupInput(
               inputId = ns("selected_imagingdone_dist"),
               label = h6("Imaging of patients shown in plot"),
               choices = c("Done", "Not done"),
               selected = c("Done", "Not done")),  
             
             checkboxGroupInput(
               inputId = ns("selected_prenotification_dist"),
               label = h6("Prenotification of patients in plot"),
               choices = c("Prenotified", "Not prenotified"),
               selected = c("Prenotified", "Not prenotified")),
             
             checkboxGroupInput(
               inputId = ns("selected_mrs_dist"),
               label = h6("mRS of patients in plot"),
               choices = c(0:6),
               selected = c(0:6))  
      ),
      column(3,
             h3("Filter by values"),     
             sliderInput(ns("slider_minmax_dist"), label = h6("Filter values of x-axis variable (slider shows range of values in x-axis variable)"),
                         min = 0, max = 100, value = c(0, 100)),
             
             checkboxGroupInput(
               inputId = ns("selected_quarts_dist"),
               label = h6("Quarters shown in plot:"),
               choices = c("2018 Q1", "2018 Q2", "2018 Q3", "2018 Q4",
                           "2019 Q1", "2019 Q2", "2019 Q3", "2019 Q4",
                           "2020 Q1", "2020 Q2", "2020 Q3", "2020 Q4",
                           "2021 Q1", "2021 Q2", "2021 Q3", "2021 Q4",
                           "2022 Q1", "2022 Q2", "2022 Q3", "2022 Q4"),
               selected = c("2018 Q1", "2018 Q2", "2018 Q3", "2018 Q4",
                            "2019 Q1", "2019 Q2", "2019 Q3", "2019 Q4",
                            "2020 Q1", "2020 Q2", "2020 Q3", "2020 Q4",
                            "2021 Q1", "2021 Q2", "2021 Q3", "2021 Q4",
                            "2022 Q1", "2022 Q2", "2022 Q3", "2022 Q4")), 
      ),
      
      
      )  
    )
  
  corr_UI <- tagList(
    plotlyOutput(
      outputId = ns("corrPlot")
    ),
    
    fixedRow(
      column(3,
             h3("Plot characteristics"),     
             selectInput(
               inputId = ns("selected_colx_corr"),
               label = h6("Select x-axis variable"),
               choices = database,
               selected = "Door-to-imaging time"),
             
             selectInput(
               inputId = ns("selected_coly_corr"),
               label = h6("Select y-axis variable"),
               choices = database,
               selected = "Modified ranking scale discharge"),
             
             h6("Show trend line"),
             checkboxInput(inputId = ns("selected_trendline_corr"), "Show trend line", value = FALSE),
            
      ),
      column(3,
             h3("Split by subgroups"),
             selectInput(
               inputId = ns("selected_split_corr"),
               label = h6("Select factor to colour data"),
               choices = c("None","Gender", "mRS on discharge", "3-month mRS", "Arrival pre-notified", "Imaging done", "Physiotherapy initiated", "Test for dysphagia screen"),
               selected = NULL),
             
             ),
      column(3, 
             h3("Filter by subgroups"), 
             
             checkboxGroupInput(
               inputId = ns("selected_genders_corr"),
               label = h6("Genders shown in plot:"),
               choices = c("Female", "Male"),
               selected = c("Female", "Male")),  
             
             checkboxGroupInput(
               inputId = ns("selected_imagingdone_corr"),
               label = h6("Imaging of patients shown in plot"),
               choices = c("Done", "Not done"),
               selected = c("Done", "Not done")),  
             
             checkboxGroupInput(
               inputId = ns("selected_prenotification_corr"),
               label = h6("Prenotification of patients in plot"),
               choices = c("Prenotified", "Not prenotified"),
               selected = c("Prenotified", "Not prenotified")),
             
             checkboxGroupInput(
               inputId = ns("selected_mrs_corr"),
               label = h6("mRS of patients in plot"),
               choices = c(0:6),
               selected = c(0:6))  
      ),
      column(3,
             h3("Filter by values"),     
             sliderInput(ns("slider_minmax_x_corr"), label = h6("Filter values of x-axis variable (slider shows range of values in x-axis variable)"),
                         min = 0, max = 100, value = c(0, 100)),
             
             sliderInput(ns("slider_minmax_y_corr"), label = h6("Filter values of y-axis variable (slider shows range of values in y-axis variable)"),
                         min = 0, max = 100, value = c(0, 100)),
             
             checkboxGroupInput(
               inputId = ns("selected_quarts_corr"),
               label = h6("Quarters shown in plot:"),
               choices = c("2018 Q1", "2018 Q2", "2018 Q3", "2018 Q4",
                           "2019 Q1", "2019 Q2", "2019 Q3", "2019 Q4",
                           "2020 Q1", "2020 Q2", "2020 Q3", "2020 Q4",
                           "2021 Q1", "2021 Q2", "2021 Q3", "2021 Q4",
                           "2022 Q1", "2022 Q2", "2022 Q3", "2022 Q4"),
               selected = c("2018 Q1", "2018 Q2", "2018 Q3", "2018 Q4",
                            "2019 Q1", "2019 Q2", "2019 Q3", "2019 Q4",
                            "2020 Q1", "2020 Q2", "2020 Q3", "2020 Q4",
                            "2021 Q1", "2021 Q2", "2021 Q3", "2021 Q4",
                            "2022 Q1", "2022 Q2", "2022 Q3", "2022 Q4")), 
      ), ) )
  
  comp_UI <- tagList(
    plotlyOutput(
      outputId = ns("compPlot")
    ),
    fixedRow(
     column(3,
            h3("Plot characteristics"),     
            selectInput(
              inputId = ns("selected_col_comp"),
              label = h6("Select y-axis variable"),
              choices = database,
              selected = "Door-to-imaging time"),
            
            selectInput(
              inputId = ns("selected_split_comp"),
              label = h6("Select factor to compare data"),
              choices = c("None","Gender", "mRS on discharge", "3-month mRS", "Arrival pre-notified", "Imaging done", "Physiotherapy initiated", "Test for dysphagia screen"),
              selected = "Gender"),
            ),
     
     

            column(3,
                   h3("Filter by values"),     
                   sliderInput(ns("slider_minmax_comp"), label = h6("Filter values of y-axis variable (slider shows range of values in y-axis variable)"),
                               min = 0, max = 100, value = c(0, 100)),
                   
                   checkboxGroupInput(
                     inputId = ns("selected_quarts_comp"),
                     label = h6("Quarters shown in plot:"),
                     choices = c("2018 Q1", "2018 Q2", "2018 Q3", "2018 Q4",
                                 "2019 Q1", "2019 Q2", "2019 Q3", "2019 Q4",
                                 "2020 Q1", "2020 Q2", "2020 Q3", "2020 Q4",
                                 "2021 Q1", "2021 Q2", "2021 Q3", "2021 Q4",
                                 "2022 Q1", "2022 Q2", "2022 Q3", "2022 Q4"),
                     selected = c("2018 Q1", "2018 Q2", "2018 Q3", "2018 Q4",
                                  "2019 Q1", "2019 Q2", "2019 Q3", "2019 Q4",
                                  "2020 Q1", "2020 Q2", "2020 Q3", "2020 Q4",
                                  "2021 Q1", "2021 Q2", "2021 Q3", "2021 Q4",
                                  "2022 Q1", "2022 Q2", "2022 Q3", "2022 Q4")), 
            ),
    ))
  
  #Here we select to output the plot and table under a tabsetPanel format, where the user can choose to look at either the plot or the underlying datatable
  tabsetPanel(
    type = "tabs",
    tabPanel("Timeline", plot_UI),
    tabPanel("Distibution", dist_UI),
    tabPanel("Correlation", corr_UI),
    tabPanel("Sub-group comparisons", comp_UI)
  )
}

plot_Expanded <- function(id, df) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$selected_col,{
        index <- match(input$selected_col, df$INDICATOR)
        QI_ylab(input$selected_col)
        QI_col <- df$COLUMN[index]
        QI_name(QI_col)
        if (df$PERCENTAGE[index]==1) {
          QI_displayaspercentage(TRUE)
          updateSelectInput(session, "selected_colx", selected = "mean")
        } else {QI_displayaspercentage(FALSE)}
        QI_filt <- numVars %>% filter(QI == QI_name(), site_name=="Samaritan") %>% drop_na(Value)
        
        updateSliderInput(session, "slider_minmax", value = c(min(QI_filt$Value),max(QI_filt$Value)),
                          min = min(QI_filt$Value), max = max(QI_filt$Value), step = 1)
      })
      observeEvent(input$selected_colx,{
        if (input$selected_colx=="standard deviation") {
          QI_agg("sd")
        } else if (input$selected_colx=="minimum"){
          QI_agg("min")
        } else if (input$selected_colx=="maximum"){
          QI_agg("max")
        } else {
          QI_agg(input$selected_colx)
        }
      })
      observeEvent(input$selected_trend, {
        QI_trend(input$selected_trend)
      }, ignoreNULL=FALSE)
      observeEvent(input$selected_error, {
        QI_error(input$selected_error)
      }, ignoreNULL=FALSE)
      observeEvent(input$selected_genders, {
        sg <- input$selected_genders
        sg[sg=="Male"] <- 1
        sg[sg=="Female"] <- 0
        QI_gender(sg)
      }, ignoreNULL=FALSE)
      observeEvent(input$selected_imagingdone, {
        id <- input$selected_imagingdone
        id[id=="Done"] <- 1
        id[id=="Not done"] <- 0
        QI_imaging(id)
      }, ignoreNULL=FALSE)
      observeEvent(input$selected_prenotification, {
        p <- input$selected_prenotification
        p[p=="Prenotified"] <- 1
        p[p=="Not prenotified"] <- 0
        QI_prenotification(p)
      }, ignoreNULL=FALSE)
      observeEvent(input$selected_mrs, {
        QI_mrs(input$selected_mrs)
      }, ignoreNULL=FALSE)
      observeEvent(input$selected_comparisons,{
        compared_hospitals(input$selected_comparisons)
      }, ignoreNULL=FALSE)
      observeEvent(input$selected_natcomparisons,{
        compare_national(input$selected_natcomparisons)
      }, ignoreNULL=FALSE)
      observeEvent(input$slider_minmax, {
        QI_filterminmax(input$slider_minmax[1]:input$slider_minmax[2])
      })
      observeEvent(input$selected_quarts, {
        QI_filterquarts(input$selected_quarts)
      })
      
      
      
      observeEvent(input$selected_col_dist,{
        QI_xlab_dist(input$selected_col_dist)
        index <- match(input$selected_col_dist, df$INDICATOR)
        QI_col <- df$COLUMN[index]
        QI_name_dist(QI_col)
        QI_filt <- numVars %>% filter(QI == QI_name_dist(), site_name=="Samaritan") %>% drop_na(Value)
        updateSliderInput(session, "slider_minmax_dist", value = c(min(QI_filt$Value),max(QI_filt$Value)),
                          min = min(QI_filt$Value), max = max(QI_filt$Value), step = 1)
      })
      observeEvent(input$selected_comparisons_dist,{
        compared_hospitals_dist(input$selected_comparisons_dist)
      })
      observeEvent(input$selected_natcomparisons_dist,{
        compare_national_dist(input$selected_natcomparisons_dist)
      }, ignoreNULL=FALSE)
      observeEvent(input$selected_genders_dist, {
        sg_dist <- input$selected_genders_dist
        sg_dist[sg_dist=="Male"] <- 1
        sg_dist[sg_dist=="Female"] <- 0
        QI_gender_dist(sg_dist)
      }, ignoreNULL=FALSE)
      observeEvent(input$selected_imagingdone_dist, {
        id_dist <- input$selected_imagingdone_dist
        id_dist[id_dist=="Done"] <- 1
        id_dist[id_dist=="Not done"] <- 0
        QI_imaging_dist(id_dist)
      }, ignoreNULL=FALSE)
      observeEvent(input$selected_prenotification_dist, {
        p_dist <- input$selected_prenotification_dist
        p_dist[p_dist=="Prenotified"] <- 1
        p_dist[p_dist=="Not prenotified"] <- 0
        QI_prenotification_dist(p_dist)
      }, ignoreNULL=FALSE)
      observeEvent(input$selected_mrs_dist, {
        QI_mrs_dist(input$selected_mrs_dist)
      }, ignoreNULL=FALSE)
      observeEvent(input$slider_minmax_dist, {
        QI_filterminmax_dist(input$slider_minmax_dist[1]:input$slider_minmax_dist[2])
      })
      observeEvent(input$selected_quarts_dist, {
        QI_filterquarts_dist(input$selected_quarts_dist)
      })
      observeEvent(input$selected_mean_dist, {
        QI_mean_dist(input$selected_mean_dist)
      }, ignoreNULL=FALSE)
      observeEvent(input$selected_median_dist, {
        QI_median_dist(input$selected_median_dist)
      }, ignoreNULL=FALSE)
      
      
      
      observeEvent(input$selected_colx_corr,{
        index <- match(input$selected_colx_corr, df$INDICATOR)
        QI_xlab_corr(input$selected_colx_corr)
        QI_col <- df$COLUMN[index]
        QI_name_x_corr(QI_col)
        QI_filt <- numVars %>% filter(QI == QI_name_x_corr(), site_name=="Samaritan") %>% drop_na(Value)
        updateSliderInput(session, "slider_minmax_x_corr", value = c(min(QI_filt$Value),max(QI_filt$Value)),
                          min = min(QI_filt$Value), max = max(QI_filt$Value), step = 1)
      })
      observeEvent(input$selected_coly_corr,{
        index <- match(input$selected_coly_corr, df$INDICATOR)
        QI_ylab_corr(input$selected_coly_corr)
        QI_col <- df$COLUMN[index]
        QI_name_y_corr(QI_col)
        QI_filt <- numVars %>% filter(QI == QI_name_y_corr(), site_name=="Samaritan") %>% drop_na(Value)
        updateSliderInput(session, "slider_minmax_y_corr", value = c(min(QI_filt$Value),max(QI_filt$Value)),
                          min = min(QI_filt$Value), max = max(QI_filt$Value), step = 1)
      })
      observeEvent(input$selected_trendline_corr, {
        QI_trend_corr(input$selected_trendline_corr)
      }, ignoreNULL=FALSE)
      observeEvent(input$selected_genders_corr, {
        sg_corr <- input$selected_genders_corr
        sg_corr[sg_corr=="Male"] <- 1
        sg_corr[sg_corr=="Female"] <- 0
        QI_gender_corr(sg_corr)
      }, ignoreNULL=FALSE)
      observeEvent(input$selected_imagingdone_corr, {
        id_corr <- input$selected_imagingdone_corr
        id_corr[id_corr=="Done"] <- 1
        id_corr[id_corr=="Not done"] <- 0
        QI_imaging_corr(id_corr)
      }, ignoreNULL=FALSE)
      observeEvent(input$selected_prenotification_corr, {
        p_corr <- input$selected_prenotification_corr
        p_corr[p_corr=="Prenotified"] <- 1
        p_corr[p_corr=="Not prenotified"] <- 0
        QI_prenotification_corr(p_corr)
      }, ignoreNULL=FALSE)
      observeEvent(input$selected_mrs_corr, {
        QI_mrs_corr(input$selected_mrs_corr)
      }, ignoreNULL=FALSE)
      observeEvent(input$slider_minmax_x_corr, {
        QI_filterminmax_x_corr(input$slider_minmax_x_corr[1]:input$slider_minmax_x_corr[2])
      })
      observeEvent(input$slider_minmax_y_corr, {
        QI_filterminmax_y_corr(input$slider_minmax_y_corr[1]:input$slider_minmax_y_corr[2])
      })
      observeEvent(input$selected_quarts_corr, {
        QI_filterquarts_corr(input$selected_quarts_corr)
      })
      observeEvent(input$selected_split_corr, {
        # Physiotherapy initiated", "Test for dysphagia screen"),
        QI_splitlab_corr(input$selected_split_corr)
        if (input$selected_split_corr=="Gender") {
          QI_split_corr("gender")
        } else if (input$selected_split_corr=="mRS on discharge") {
          QI_split_corr("discharge_mrs")
        } else if (input$selected_split_corr=="3-month mRS") {
          QI_split_corr("three_m_mrs")
        } else if (input$selected_split_corr=="Arrival pre-notified") {
          QI_split_corr("prenotification")
        } else if (input$selected_split_corr=="Imaging done") {
          QI_split_corr("imaging_done")
        } else if (input$selected_split_corr=="Physiotherapy initiated") {
          QI_split_corr("occup_physiotherapy_received")
        } else if (input$selected_split_corr=="Test for dysphagia screen") {
          QI_split_corr("dysphagia_screening_done")
        } else {
          QI_split_corr("None")
        }
      })
      
      
      
      observeEvent(input$selected_col_comp,{
        index <- match(input$selected_col_comp, df$INDICATOR)
        QI_ylab_comp(input$selected_col_comp)
        QI_col <- df$COLUMN[index]
        QI_name_comp(QI_col)
        QI_filt <- numVars %>% filter(QI == QI_name_comp(), site_name=="Samaritan") %>% drop_na(Value)
        updateSliderInput(session, "slider_minmax_comp", value = c(min(QI_filt$Value),max(QI_filt$Value)),
                          min = min(QI_filt$Value), max = max(QI_filt$Value), step = 1)
      })
      observeEvent(input$slider_minmax_comp, {
        QI_filterminmax_comp(input$slider_minmax_comp[1]:input$slider_minmax_comp[2])
      })
      observeEvent(input$selected_quarts_comp, {
        QI_filterquarts_comp(input$selected_quarts_comp)
      })
      observeEvent(input$selected_split_comp, {
        # Physiotherapy initiated", "Test for dysphagia screen"),
        if (input$selected_split_comp=="Gender") {
          QI_split_comp("gender")
          QI_xlab_comp(c("0" = "Female", "1" = "Male"))
        } else if (input$selected_split_comp=="mRS on discharge") {
          QI_split_comp("discharge_mrs")
          QI_xlab_comp(c(0:6))
        } else if (input$selected_split_comp=="3-month mRS") {
          QI_split_comp("three_m_mrs")
          QI_xlab_comp(c(0:6))
        } else if (input$selected_split_comp=="Arrival pre-notified") {
          QI_split_comp("prenotification")
          QI_xlab_comp(c("0" = "Not prenotified", "1" = "Prenotified"))
        } else if (input$selected_split_comp=="Imaging done") {
          QI_split_comp("imaging_done")
          QI_xlab_comp(c("0" = "Imaging not done", "1" = "imaging done"))
        } else if (input$selected_split_comp=="Physiotherapy initiated") {
          QI_split_comp("occup_physiotherapy_received")
          QI_xlab_comp(c("0" = "Physio. not started", "1" = "Physio. started"))
        } else if (input$selected_split_comp=="Test for dysphagia screen") {
          QI_split_comp("dysphagia_screening_done")
          QI_xlab_comp(c("0" = "Dysphagia not screened", "1" = "Dysphagia screening done"))
        } else {
          QI_split_comp("None")
        }
      })
      
      output$plot <- renderPlotly({
        if(!is.null(QI_name())) {
        
          QI_data <- numVars %>% filter(QI == QI_name(), site_name=="Samaritan", gender %in% QI_gender(), imaging_done %in% QI_imaging(), prenotification%in%QI_prenotification(), discharge_mrs%in%QI_mrs(),YQ%in%QI_filterquarts(), Value%in%QI_filterminmax()) %>% 
            drop_na(Value) %>% group_by(YQ, site_name, site_country) %>% 
            mutate(median = median(Value), sd = sd(Value), min=min(Value),
                      max=max(Value), mean=mean(Value), .groups = "drop") %>% ungroup()
          
          if (nrow(QI_data)>0){
            plot <- ggplot(QI_data, aes(x = YQ, y = .data[[QI_agg()]])) +
              geom_line(aes(group = 1,linetype = site_name), color="#D16A00", linetype="solid") +
              geom_point(color="#D16A00") +
              scale_color_discrete(labels=c("Your hospital"))+
              theme_bw() + xlab("Year and quarter") + ylab(paste(QI_ylab(),QI_agg(), sep=" "))+ scale_linetype_discrete(name = "Hospital")#+ theme(legend.position = "none")
            if (QI_displayaspercentage()) {
              plot <- plot + geom_text(aes(label=scales::percent(round(.data[[QI_agg()]], digits = 4))), size=4, nudge_y = 2, color="black")
            } else {
              plot <- plot + geom_text(aes(label=round(.data[[QI_agg()]], digits = 1)), size=4, nudge_y = 2, color="black")
              
            }
            
            if(QI_trend()){
              plot <- plot + geom_smooth(aes(group=-1),method="lm",se=FALSE, color="#D16A00")
            }
            
            if(QI_error()){
              plot <- plot + geom_errorbar(aes(ymin=min, ymax=(max-sd), color="#D16A00",alpha = 0.5))
            }
            
            if(!is.null(compared_hospitals()) && !is_empty(compared_hospitals())){
              compare_data <- numVars %>% filter(QI == QI_name(), site_name%in%compared_hospitals(), gender %in% QI_gender(), imaging_done %in% QI_imaging(), prenotification%in%QI_prenotification(), discharge_mrs%in%QI_mrs(),YQ%in%QI_filterquarts(), Value%in%QI_filterminmax()) %>% 
              drop_na(Value) %>%  group_by(site_name,YQ) %>% 
                mutate(median = median(Value), sd = sd(Value), min=min(Value),
                       max=max(Value), mean=mean(Value), .groups = "drop") %>% ungroup()
              
              
              
              plot <- plot + 
                geom_line(data=compare_data, aes(y = .data[[QI_agg()]], group=site_name,linetype = site_name), color="grey",alpha = 0.5) +
                geom_point(data=compare_data, aes(y = .data[[QI_agg()]]), color="grey",alpha = 0.5)
            }
            
            if(compare_national()==TRUE){
              
              compare_nat_data <- numVars %>% filter(QI == QI_name(), site_name!="Samaritan", gender %in% QI_gender(), imaging_done %in% QI_imaging(), prenotification%in%QI_prenotification(), discharge_mrs%in%QI_mrs(),YQ%in%QI_filterquarts(), Value%in%QI_filterminmax()) %>% 
              drop_na(Value) %>% group_by(YQ) %>% 
                mutate(median = median(Value), sd = sd(Value), min=min(Value),
                       max=max(Value), mean=mean(Value), .groups = "drop") %>% ungroup()
              
              
              plot <- plot + 
                geom_line(data=compare_nat_data, aes(group = 1,y = .data[[QI_agg()]]), color="#56B4E9",alpha = 0.5, linetype="solid") +
                geom_point(data=compare_nat_data, aes(y = .data[[QI_agg()]]), color="#56B4E9",alpha = 0.5)
            }
            
            
            ggplotly(plot)}

        }
      })
      
      output$distPlot <- renderPlotly({
        
        
        QI_data_dist <- numVars %>% filter(QI == QI_name_dist(), site_name=="Samaritan", gender %in% QI_gender_dist(), imaging_done %in% QI_imaging_dist(), prenotification%in%QI_prenotification_dist(), discharge_mrs%in%QI_mrs_dist(),YQ%in%QI_filterquarts_dist(), Value%in%QI_filterminmax_dist()) %>% drop_na(Value) %>% 
          group_by(site_name, site_country) 
        
        if (nrow(QI_data_dist)>1){
        distPlot <- ggplot(QI_data_dist, aes(x = Value)) +
          geom_density(aes(group = 1), color="#D16A00", linetype="solid") +
          scale_color_discrete(labels=c("Your hospital"))+
          theme_bw() + xlab(QI_xlab_dist())
        
        if(QI_mean_dist()){
          distPlot <- distPlot + geom_vline(xintercept = mean(QI_data_dist$Value), size=1.5, color="red",linetype=3)
        }
        
        if(QI_median_dist()){
          distPlot <- distPlot + geom_vline(xintercept = median(QI_data_dist$Value), size=1.5, color="red")
        }
        
        if(compare_national_dist()==TRUE){
        
          compare_nat_data_dist <- numVars %>% filter(QI == QI_name_dist(), site_name!="Samaritan", gender %in% QI_gender_dist(), imaging_done %in% QI_imaging_dist(), prenotification%in%QI_prenotification_dist(), discharge_mrs%in%QI_mrs_dist(),YQ%in%QI_filterquarts_dist(), Value%in%QI_filterminmax_dist()) %>% 
            drop_na(Value) %>% group_by(site_country)
          
          
          distPlot <- distPlot + 
            geom_density(data=compare_nat_data_dist, aes(group = 1), color="#56B4E9",alpha = 0.5, linetype="solid")
        }
        
        if(!is.null(compared_hospitals_dist()) && !is_empty(compared_hospitals_dist()) && compared_hospitals_dist()!="None"){
          compare_data_dist <- numVars %>% filter(QI == QI_name(), site_name==compared_hospitals_dist(), gender %in% QI_gender_dist(), imaging_done %in% QI_imaging_dist(), prenotification%in%QI_prenotification_dist(), discharge_mrs%in%QI_mrs_dist(),YQ%in%QI_filterquarts_dist(), Value%in%QI_filterminmax_dist()) %>% 
            drop_na(Value) %>%  group_by(site_name)
          
          
          
          distPlot <- distPlot + 
            geom_density(data=compare_data_dist, aes(group=1), color="grey",alpha = 0.5, linetype="solid")
        }
        
        ggplotly(distPlot)}
      })
      
    output$corrPlot <- renderPlotly({
      QI_data_x_corr <- numVars %>% filter(QI == QI_name_x_corr(), site_name=="Samaritan", gender %in% QI_gender_corr(), imaging_done %in% QI_imaging_corr(), prenotification%in%QI_prenotification_corr(), discharge_mrs%in%QI_mrs_corr(),YQ%in%QI_filterquarts_corr(), Value%in%QI_filterminmax_x_corr()) %>% drop_na(Value) %>% 
        group_by(site_name, site_country) 
      
      QI_data_y_corr <- numVars %>% filter(QI == QI_name_y_corr(), site_name=="Samaritan", gender %in% QI_gender_corr(), imaging_done %in% QI_imaging_corr(), prenotification%in%QI_prenotification_corr(), discharge_mrs%in%QI_mrs_corr(),YQ%in%QI_filterquarts_corr(), Value%in%QI_filterminmax_y_corr()) %>% drop_na(Value) %>% 
        group_by(site_name, site_country) 
      
      QI_data_corr <- merge(QI_data_x_corr, QI_data_y_corr, by = c("YQ", "site_name", "site_id", "subject_id","gender", "discharge_mrs", "prenotification","three_m_mrs", "imaging_done","occup_physiotherapy_received", "dysphagia_screening_done"))

      
      if(!is.null(QI_split_corr()) && !is_empty(QI_split_corr()) && QI_split_corr()!="None"){
        corrplot <- ggplot(data = QI_data_corr, aes(x = Value.x, y = Value.y, color=as.factor(.data[[QI_split_corr()]]), group=as.factor(.data[[QI_split_corr()]]))) + theme_bw() + 
          geom_point() + xlab(QI_xlab_corr()) + ylab(QI_ylab_corr())+ 
          scale_color_discrete(name = QI_splitlab_corr(), labels = c("Female", "Male"))
      } else {
        corrplot <- ggplot(data = QI_data_corr, aes(x = Value.x, y = Value.y)) + theme_bw() + 
          geom_point(color="#D16A00") + xlab(QI_xlab_corr()) + ylab(QI_ylab_corr()) 
        }

      if(QI_trend_corr()){
        corrplot <- corrplot + geom_smooth(method="lm", se=F)
      }
      
      ggplotly(corrplot)
    })
    
    output$compPlot <- renderPlotly({
      QI_data_comp <- numVars %>% filter(QI == QI_name_comp(), site_name=="Samaritan",YQ%in%QI_filterquarts_comp(), Value%in%QI_filterminmax_comp()) %>% drop_na(Value) %>% 
        group_by(site_name, site_country) 
      
      compPlot <- ggplot(data = QI_data_comp, aes(x = as.factor(.data[[QI_split_comp()]]), y = Value, color=as.factor(.data[[QI_split_comp()]]))) +
        geom_boxplot(notch = TRUE) + 
        theme_bw() + theme(legend.position = "none", axis.title.x = element_blank())+ 
        scale_x_discrete(labels=QI_xlab_comp()) + ylab(QI_ylab_comp())
    })
      
    }
  )
}
