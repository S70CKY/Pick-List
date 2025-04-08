library(shiny)
library(readxl)
library(DT)
library(dplyr)
library(tidyr)
library(writexl)

# urine, plasma, serum
# Load data
data <- read_excel("/Users/s70cky/Desktop/Picket_List/PicketList_main.xlsx", sheet = 1)

ui <- fluidPage(
  titlePanel("SwiSCI Sample Query Tool"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sampleType", "Sample Type", choices = c("All", unique(data$`Sample Type`))), # plasma, serum, urin, rna
      selectInput("sampleGroup", "Sample Group", choices = c("All", unique(data$`Sample Group`))),
      selectInput("freezer", "Freezer", choices = c("All", unique(data$Freezer))),
      textInput("position", "Search Position (e.g., '1 / A')", ""),
      checkboxInput("volumePositive", "Volume > 0", value = TRUE),
      checkboxInput("vialsPositive", "Vials > 0", value = TRUE),
      checkboxInput("onlySwiSCI", "Only SwiSCI samples", value = FALSE),
      checkboxInput("requireT1T4", "Require both T1 & T4 samples per ID", value = FALSE),
      checkboxInput("onlyT4", "Only T4 samples", value = FALSE),
      checkboxInput("excludeQC", "Exclude QC samples", value = FALSE),
      checkboxInput("lowestPosition", "Keep only lowest PositionNumber (1–12) per ID & Group", value = FALSE),
      downloadButton("downloadData", "Download Filtered Data")
    ),
    mainPanel(
      DTOutput("table")
    )
  )
)

server <- function(input, output, session) {
  
  filteredData <- reactive({
    df <- data
    
    # Apply column removal
    df <- df %>%
      select(-Owner, -Expiration, -`Blood withdrawl`, -`Processing start`, -`Processing finished`, 
             -`Freezed Date (outside of biobank)`, -`Blood Type`, -`Lab specialist`, 
             -`Quality control`, -`Documentation 2`, -Level1, -Level2, -Level3, -Level4, -Level5)
    
    # Only SwiSCI?
    if (input$onlySwiSCI) {
      df <- df %>% filter(grepl("^SwiSCI", Name))
    }
    
    # Only T4 samples?
    if (input$onlyT4) {
      df <- df %>% filter(`Sample Group` == "t4")
    }
    
    # Require both T1 & T4?
    if (input$requireT1T4) {
      df <- df %>%
        mutate(PositionNumber = as.numeric(gsub("^(\\d+).*", "\\1", Position))) %>%
        group_by(SwiSCI_ID) %>%
        filter(any(`Sample Group` == "t1") & any(`Sample Group` == "t4")) %>%
        ungroup() %>%
        group_by(SwiSCI_ID, `Sample Group`) %>%
        slice_min(order_by = PositionNumber, n = 1, with_ties = FALSE) %>%
        ungroup()
    }
    
    # Exclude QC
    if (input$excludeQC) {
      df <- df %>% filter(!grepl(" QC$", Name))
    }
    
    # Volume > 0
    if (input$volumePositive) {
      df <- df %>% filter(Volume > 0)
    }
    
    # Vials > 0
    if (input$vialsPositive) {
      df <- df %>% filter(Vials > 0)
    }
    
    # Position filter (1–12) and keep only lowest PositionNumber per ID + Sample Group
    if (input$lowestPosition) {
      df <- df %>%
        mutate(PositionNumber = as.numeric(gsub("^(\\d+).*", "\\1", Position))) %>%
        filter(PositionNumber %in% 1:12) %>%
        group_by(SwiSCI_ID, `Sample Group`) %>%
        slice_min(order_by = PositionNumber, n = 1, with_ties = FALSE) %>%
        ungroup()
    }
    
    # UI Filters
    if (input$sampleType != "All") {
      df <- df[df$`Sample Type` == input$sampleType, ]
    }
    if (input$sampleGroup != "All") {
      df <- df[df$`Sample Group` == input$sampleGroup, ]
    }
    if (input$freezer != "All") {
      df <- df[df$Freezer == input$freezer, ]
    }
    if (input$position != "") {
      df <- df[grepl(input$position, df$Position, ignore.case = TRUE), ]
    }
    
    return(df)
  })
  
  output$table <- renderDT({
    datatable(filteredData(), options = list(pageLength = 15, scrollX = TRUE))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Filtered_SwiSCI_Data_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(filteredData(), file)
    }
  )
}

shinyApp(ui = ui, server = server)
