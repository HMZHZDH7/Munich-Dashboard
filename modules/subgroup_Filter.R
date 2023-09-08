createSubgroupFilterUI <- function(id, suffix="") {
    ns <- NS(id)

    # Define the UI elements for subgroups filter
    subgroupFilterUI <- list(
        column(
            6,
             h3(paste0("Filter by subgroups")),
            checkboxGroupInput(
                inputId = ns(paste0("selected_genders", suffix)),
                label = h6("Genders shown in plot:"),
                choices = c("Female", "Male"),
                selected = c("Female", "Male")
            ),
            checkboxGroupInput(
                inputId = ns(paste0("selected_imagingdone", suffix)),
                label = h6("Imaging of patients shown in plot"),
                choices = c("Done", "Not done"),
                selected = c("Done", "Not done")
            ),
            checkboxGroupInput(
                inputId = ns(paste0("selected_prenotification", suffix)),
                label = h6("Prenotification of patients in plot"),
                choices = c("Prenotified", "Not prenotified"),
                selected = c("Prenotified", "Not prenotified")
            ),
            checkboxGroupInput(
                inputId = ns(paste0("selected_mrs", suffix)),
                label = h6("mRS of patients in plot"),
                choices = c(0:6),
                selected = c(0:6)
            )
        )
    )

    # Return the UI elements as a list
    return(subgroupFilterUI)
}
