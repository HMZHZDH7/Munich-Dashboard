plot_Expanded_UI <- function(id, database, hospitals) {
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
               selected = "median")
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
             selectInput(
               inputId = ns("selected_filtercol"),
               label = h6("Select variable for filter"),
               choices = c("", database),
               selected = NULL))
    ),
    
    
    
    
  )
  
  #Storing the datatable output under this variable
  #table_UI <- dataTableOutput(outputId = ns("table"))
  
  #Here we select to output the plot and table under a tabsetPanel format, where the user can choose to look at either the plot or the underlying datatable
  #tabsetPanel(
  #  type = "tabs",
  #  tabPanel("Plot", plot_UI),
  #  tabPanel("Table", table_UI)
  #)
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
      #validate(need(df(), "Waiting for data..."), errorClass = character(0))
      #QI_data <- df
      #browser()
      #       # Here we see the interactive plot. It selects from the database the chosen columns via input$variableName and generates a plot for it.
      output$plot <- renderPlotly({
        if(!is.null(QI_name())) {
          view(numVars)
          
          QI_data <- numVars %>% filter(QI == QI_name(), site_name=="Samaritan") %>% drop_na(Value) %>% 
            group_by(YQ, site_name, site_country) %>% 
            mutate(median = median(Value), sd = sd(Value), min=min(Value),
                      max=max(Value),.groups = "drop") %>% ungroup()
          view(df)
          view(QI_data)
          
          
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
    }
  )
}
