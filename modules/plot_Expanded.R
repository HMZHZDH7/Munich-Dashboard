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
               choices = c(1:5),
               selected = c(1:5))  
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
               choices = hospitals,
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
               choices = c(1:5),
               selected = c(1:5))  
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
               inputId = ns("selected_colx_agg_corr"),
               label = h6("Select aggregation type for x-axis"),
               choices = c("median", "mean", "standard deviation", "minimum", "maximum"),
               selected = "median"),
             
             selectInput(
               inputId = ns("selected_coly_corr"),
               label = h6("Select y-axis variable"),
               choices = database,
               selected = "Modified ranking scale discharge"),
             
             selectInput(
               inputId = ns("selected_coly_agg_corr"),
               label = h6("Select aggregation type for y-axis"),
               choices = c("median", "mean", "standard deviation", "minimum", "maximum"),
               selected = "median"),
             
             h6("Show trend line"),
             checkboxInput(inputId = ns("selected_tredline_corr"), "Show trend line", value = FALSE),
            
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
               choices = c(1:5),
               selected = c(1:5))  
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
      })
      
      observeEvent(input$selected_comparisons,{
        compared_hospitals(input$selected_comparisons)
      }, ignoreNULL=FALSE)
      
      observeEvent(input$selected_natcomparisons,{
        if(!compare_national())
        {compare_national(TRUE)}
        else
        {compare_national(FALSE)}
        
      })
      
      observeEvent(input$selected_col_dist,{
        
        index <- match(input$selected_col_dist, df$INDICATOR)
        QI_col <- df$COLUMN[index]
        
        
        QI_name_dist(QI_col)
      })
      
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
      #validate(need(df(), "Waiting for data..."), errorClass = character(0))
      #QI_data <- df
      #browser()
      #       # Here we see the interactive plot. It selects from the database the chosen columns via input$variableName and generates a plot for it.
      output$plot <- renderPlotly({
        if(!is.null(QI_name())) {
          
          QI_filt <- numVars %>% filter(QI == QI_name(), site_name=="Samaritan") %>% drop_na(Value)

          updateSliderInput(session, "slider_minmax", value = c(min(QI_filt$Value), max(QI_filt$Value)),
                            min = min(QI_filt$Value), max = max(QI_filt$Value), step = 5)

          QI_data <- numVars %>% filter(QI == QI_name(), site_name=="Samaritan") %>% drop_na(Value) %>% 
            group_by(YQ, site_name, site_country) %>% 
            mutate(median = median(Value), sd = sd(Value), min=min(Value),
                      max=max(Value),.groups = "drop") %>% ungroup()
          
          
          #if(!is.null(compared_hospitals())) {
          #  compare_data <- numVars %>% filter(QI == QI_name(), site_name==compared_hospitals()) %>% na.omit() %>% 
          #    group_by(site_name,YQ) %>% 
          #    summarise(median = median(Value), sd = sd(Value), min=min(Value), max=max(Value),.groups = "drop")
          #  view(compare_data)
          #}
          #else{
          #  compared_hospitals(NULL)
          #}
          
          
          
          
          
          
          #plot <- ggplot(database, aes(x = .data[[input$selected_col]], y = .data[[input$selected_col2]])) +
          #geom_point()
          plot <- ggplot(QI_data, aes(x = YQ, y = median)) +
            geom_line(aes(group = 1,linetype = site_name), color="#D16A00", linetype="solid") +
            geom_point(color="#D16A00") +
            geom_text(aes(label=median), size=3, nudge_y = 2, color="black") +
            scale_color_discrete(labels=c("Your hospital"))+
            #geom_errorbar(aes(ymin=median-sd, ymax=median+sd)) + 
            theme_bw()
          
          if(input$selected_trend){
            plot <- plot + geom_smooth(method="lm")
          }
          
          if(!is.null(compared_hospitals()) && !is_empty(compared_hospitals())){
            compare_data <- numVars %>% filter(QI == QI_name(), site_name%in%compared_hospitals()) %>% drop_na(Value) %>% 
              group_by(site_name,YQ) %>% 
              summarise(median = median(Value), sd = sd(Value), min=min(Value), max=max(Value))  %>% ungroup()
            
            
            
            plot <- plot + 
              geom_line(data=compare_data, aes(y = median, group=site_name,linetype = site_name), color="grey",alpha = 0.5) +
              geom_point(data=compare_data, aes(y = median), color="grey",alpha = 0.5)
          }
          
          if(compare_national()==TRUE){
            compare_nat_data <- numVars %>% filter(QI == QI_name(), site_name!="Samaritan") %>% drop_na(Value) %>% 
              group_by(YQ) %>% 
              summarise(median = median(Value), sd = sd(Value), min=min(Value), max=max(Value),.groups = "drop")
            
            
            plot <- plot + 
              geom_line(data=compare_nat_data, aes(group = 1,y = median), color="#56B4E9",alpha = 0.5, linetype="solid") +
              geom_point(data=compare_nat_data, aes(y = median), color="#56B4E9",alpha = 0.5)
          }
          
          
          ggplotly(plot)
        }
      })
      output$distPlot <- renderPlotly({
        
        
        QI_data_dist <- numVars %>% filter(QI == QI_name_dist(), site_name=="Samaritan") %>% drop_na(Value) %>% 
          group_by(site_name, site_country) 
        #  mutate(median = median(Value), sd = sd(Value), min=min(Value),
        #         max=max(Value),.groups = "drop") %>% ungroup()
        
        
        disPlot <- ggplot(QI_data_dist, aes(x = Value)) +
          geom_density(aes(group = 1), color="#D16A00", linetype="solid") +
          scale_color_discrete(labels=c("Your hospital"))+
          #geom_errorbar(aes(ymin=median-sd, ymax=median+sd)) + 
          theme_bw()
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
