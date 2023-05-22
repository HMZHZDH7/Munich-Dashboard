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
               choices = c("", "Paradise", "Angelvale", "Rose", "General", "Mercy", "Hope"),
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
               choices = c("Gender", "mRS", "Prenotification", "Imaging done", "Physiotherapy initiated"),
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
              choices = c("Gender", "mRS", "Prenotification", "Imaging done", "Physiotherapy initiated"),
              selected = "Gender"),
            ),
     
     

            column(3,
                   h3("Filter by values"),     
                   sliderInput(ns("slider_minmax_com"), label = h6("Filter values of y-axis variable (slider shows range of values in y-axis variable)"),
                               min = 0, max = 100, value = c(0, 100)),
                   
                   checkboxGroupInput(
                     inputId = ns("selected_quarts_com"),
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
        QI_col <- df$COLUMN[index]
        QI_name(QI_col)
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
        if(!compare_national())
        {compare_national(TRUE)}
        else
        {compare_national(FALSE)}
      })
      observeEvent(input$slider_minmax, {
        QI_filterminmax(input$slider_minmax[1]:input$slider_minmax[2])
      })
      observeEvent(input$selected_quarts, {
        QI_filterquarts(input$selected_quarts)
      })
      
      
      
      observeEvent(input$selected_col_dist,{
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
        if(compare_national_dist())
        {compare_national_dist(FALSE)}
        else
        {compare_national_dist(TRUE)}
      })
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
        QI_col <- df$COLUMN[index]
        QI_name_x_corr(QI_col)
      })
      observeEvent(input$selected_coly_corr,{
        index <- match(input$selected_coly_corr, df$INDICATOR)
        QI_col <- df$COLUMN[index]
        QI_name_y_corr(QI_col)
      })
      
      
      output$plot <- renderPlotly({
        if(!is.null(QI_name())) {
        
          QI_data <- numVars %>% filter(QI == QI_name(), site_name=="Samaritan", gender %in% QI_gender(), imaging_done %in% QI_imaging(), prenotification%in%QI_prenotification(), discharge_mrs%in%QI_mrs(),YQ%in%QI_filterquarts(), Value%in%QI_filterminmax()) %>% 
            drop_na(Value) %>% group_by(YQ, site_name, site_country) %>% 
            mutate(median = median(Value), sd = sd(Value), min=min(Value),
                      max=max(Value), mean=mean(Value), .groups = "drop") %>% ungroup()
          
          
          plot <- ggplot(QI_data, aes(x = YQ, y = .data[[QI_agg()]])) +
            geom_line(aes(group = 1,linetype = site_name), color="#D16A00", linetype="solid") +
            geom_point(color="#D16A00") +
            geom_text(aes(label=.data[[QI_agg()]]), size=3, nudge_y = 2, color="black") +
            scale_color_discrete(labels=c("Your hospital"))+
            theme_bw() 
          
          if(QI_trend()){
            plot <- plot + geom_smooth(method="lm")
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
          
          
          ggplotly(plot)
        }
      })
      
      output$distPlot <- renderPlotly({
        
        
        QI_data_dist <- numVars %>% filter(QI == QI_name_dist(), site_name=="Samaritan", gender %in% QI_gender_dist(), imaging_done %in% QI_imaging_dist(), prenotification%in%QI_prenotification_dist(), discharge_mrs%in%QI_mrs_dist(),YQ%in%QI_filterquarts_dist(), Value%in%QI_filterminmax_dist()) %>% drop_na(Value) %>% 
          group_by(site_name, site_country) 
        
        
        distPlot <- ggplot(QI_data_dist, aes(x = Value)) +
          geom_density(aes(group = 1), color="#D16A00", linetype="solid") +
          scale_color_discrete(labels=c("Your hospital"))+
          theme_bw()
        
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
        
        if(!is.null(compared_hospitals_dist()) && !is_empty(compared_hospitals_dist()) && compared_hospitals_dist()!=""){
          compare_data_dist <- numVars %>% filter(QI == QI_name(), site_name==compared_hospitals_dist(), gender %in% QI_gender_dist(), imaging_done %in% QI_imaging_dist(), prenotification%in%QI_prenotification_dist(), discharge_mrs%in%QI_mrs_dist(),YQ%in%QI_filterquarts_dist(), Value%in%QI_filterminmax_dist()) %>% 
            drop_na(Value) %>%  group_by(site_name)
          
          
          
          distPlot <- distPlot + 
            geom_density(data=compare_data_dist, aes(group=1), color="grey",alpha = 0.5, linetype="solid")
        }
        
        ggplotly(distPlot)
      })
      
    output$corrPlot <- renderPlotly({
      QI_data_x_corr <- numVars %>% filter(QI == QI_name_x_corr(), site_name=="Samaritan") %>% drop_na(Value) %>% 
        group_by(site_name, site_country) 
      
      QI_data_y_corr <- numVars %>% filter(QI == QI_name_y_corr(), site_name=="Samaritan") %>% drop_na(Value) %>% 
        group_by(site_name, site_country) 
      
      QI_data_corr <- merge(QI_data_x_corr, QI_data_y_corr, by = c("YQ", "site_name", "site_id", "subject_id"))

      corrplot <- ggplot(data = QI_data_corr, aes(x = Value.x, y = Value.y)) +
        geom_point(color="#D16A00") + 
        theme_bw()
    })
    
    output$compPlot <- renderPlotly({
      QI_data_comp <- numVars %>% filter(QI == QI_name(), site_name=="Samaritan") %>% drop_na(Value) %>% 
        group_by(site_name, site_country) 
      
      corrplot <- ggplot(data = QI_data_comp, aes(x = as.factor(gender), y = Value, color=as.factor(gender))) +
        geom_boxplot(notch = TRUE) + 
        theme_bw() + theme(legend.position = "none", axis.title.x = element_blank())+ 
        scale_x_discrete(labels=c("0" = "Female", "1" = "Male"))
    })
      
    }
  )
}
