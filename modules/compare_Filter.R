createCompareUI <- function(id, suffix = "") {
    ns <- NS(id)

    # Define the UI elements for comparison
    compareUI <- list(
        column(
            6,
            h3(paste0("Compare")),
            h6("Compare with country"),
            checkboxInput(
                inputId = ns(paste0("selected_natcomparisons", suffix)),
                "Show national value", value = FALSE
            ),
            if (suffix == "_dist") {
                selectInput(
                    inputId = ns(paste0("selected_comparisons", suffix)),
                    label = h6("Compare with hospitals"),
                    choices = c(
                        "None", "Paradise", "Angelvale", "Rose", "General", "Mercy", "Hope"
                    ),
                    selected = NULL
                )
            } else {
                checkboxGroupInput(
                    inputId = ns("selected_comparisons"),
                    label = h6("Compare with hospitals"),
                    choices = hospitals,
                    selected = NULL
                )
            }
        )
    )

    # Return the UI elements as a list
    return(compareUI)
}
