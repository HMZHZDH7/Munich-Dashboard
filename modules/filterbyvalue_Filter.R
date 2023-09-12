createFilterbyvalueUI <- function(id, suffix="") {
    ns <- NS(id)

    # Define the UI elements for the filter by values
    filterByValueUI <- list(
        column(
            6,
            h3(paste0("Filter by values ")), 
            sliderInput(
                if(suffix == "_corr"){
                     inputId = ns(paste0("slider_minmax_x", suffix)) 
                } else {
                   inputId = ns(paste0("slider_minmax", suffix))
                },
                label = h6("Filter values of x-axis variable (slider shows range of values in x-axis variable)"),
                min = 0, max = 100, value = c(0, 100)
            ),
            if (suffix == "_corr") {
                sliderInput(
                    inputId = ns(paste0("slider_minmax_y", suffix)), 
                    label = h6("Filter values of y-axis variable (slider shows range of values in y-axis variable)"),
                    min = 0, max = 100, value = c(0, 100)
                )
            },
            checkboxGroupInput(
                inputId = ns(paste0("selected_quarts", suffix)), 
                label = h6("Quarters shown in plot:"),
                choices = c(
                    "2018 Q1", "2018 Q2", "2018 Q3", "2018 Q4",
                    "2019 Q1", "2019 Q2", "2019 Q3", "2019 Q4",
                    "2020 Q1", "2020 Q2", "2020 Q3", "2020 Q4",
                    "2021 Q1", "2021 Q2", "2021 Q3", "2021 Q4",
                    "2022 Q1", "2022 Q2", "2022 Q3", "2022 Q4"
                ),
                selected = c(
                    "2018 Q1", "2018 Q2", "2018 Q3", "2018 Q4",
                    "2019 Q1", "2019 Q2", "2019 Q3", "2019 Q4",
                    "2020 Q1", "2020 Q2", "2020 Q3", "2020 Q4",
                    "2021 Q1", "2021 Q2", "2021 Q3", "2021 Q4",
                    "2022 Q1", "2022 Q2", "2022 Q3", "2022 Q4"
                )
            )
        )
    )

    # Return the UI elements as a list
    return(filterByValueUI)
}
