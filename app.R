library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)

# Load emissions data
total_emissions <- readRDS("gwp_total_emissions_long.rds")

ui <- fluidPage(
  titlePanel("Emissions Data Mapper"),
  sidebarPanel(
    selectInput("selected_area", "Select Country:", choices = sort(unique(total_emissions$Area))),
    selectInput("selected_year", "Select Year:", choices = sort(unique(total_emissions$Year))),
    fileInput("user_file", "Upload Excel File:", accept = ".xlsx"),
    downloadButton("download_data", "Download Processed Data")
  )
)

server <- function(input, output, session) {
  user_data <- reactive({
    req(input$user_file)
    user_structure <- read_xlsx(input$user_file$datapath, sheet = "User Structure")
    equivalence <- read_xlsx(input$user_file$datapath, sheet = "Equivalence")
    
    # Add Undetermined category
    undetermined_list <- as.data.frame(sort(unique(total_emissions$`ISIC Detail`))[startsWith(sort(unique(total_emissions$`ISIC Detail`)), "9999")])
    colnames(undetermined_list) <- "User Structure (without total)"
    user_structure <- rbind(user_structure, undetermined_list)
    
    list(user_structure = user_structure, equivalence = equivalence)
  })
  
  final_product <- reactive({
    req(input$selected_area, input$selected_year, user_data())
    data <- user_data()
    
    total_emissions %>% 
      filter(Area == input$selected_area, Year == as.numeric(input$selected_year)) %>% 
      left_join(data$equivalence, by = "ISIC Detail") %>% 
      mutate(`User Equivalent` = if_else(is.na(`User Equivalent`), `ISIC Detail`, `User Equivalent`)) %>% 
      mutate(`User Equivalent` = factor(`User Equivalent`, levels = data$user_structure[[1]])) %>% 
      pivot_wider(
        id_cols = c(Area, ISO3, Year, `IPCC Code`, IPCC, Driver, `Emission Type`, Unit),
        names_from = `User Equivalent`,
        values_from = Value,
        values_fn = sum,
        names_expand = TRUE
      ) %>% 
      arrange(Unit) %>% 
      mutate(Total = rowSums(across(where(is.numeric) & -3), na.rm = TRUE)) %>% 
      relocate(starts_with("9999"), .after = Total)
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      req(final_product())
      paste0("mapped-emdb-", unique(final_product()$ISO3), "-", input$selected_year, ".xlsx")
    },
    content = function(file) {
      write.xlsx(final_product(), file)
    }
  )
}

shinyApp(ui, server)
