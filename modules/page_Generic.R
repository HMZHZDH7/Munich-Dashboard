#The structure for generic pages, since most normal tabs will look similar it's easier to just have a generic page module.
#If further down the line each tab needs to be individually customisable then each tab will need its own module for its tailored structure
#Note that this module takes as a parameter the tab name, this is to be able to filter from the QI list which QI's belong to each tab.
page_Generic_UI <- function(id, pageName) {
  ns <- NS(id)
  fluidPage(
      column(6,
        plot_Expanded_UI(ns("VisSection"), mtcars)
      )
    )
  
}

page_Generic <- function(id, pageName) {
  moduleServer(
    id,
    function(input, output, session) {
      plot_Expanded("VisSection", mtcars)
    }
  )
}