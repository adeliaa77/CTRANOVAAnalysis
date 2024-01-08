# Load libraries
library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(rstatix)
library(plotly)
library(htmltools)
library(tidyr)
library(shinydashboard)

# Sample data
ctr_data <- data.frame(
  day = 1:10,
  left = c(2.5, 2.7, 2.8, 2.6, 3.0, 2.4, 2.9, 2.5, 2.6, 2.7),
  center = c(3.8, 3.5, 4.0, 3.7, 3.9, 3.6, 4.1, 3.4, 3.8, 3.9),
  right = c(3.1, 2.9, 3.0, 3.2, 3.3, 2.8, 3.4, 3.1, 3.2, 3.5)
)

# UI
ui <- fluidPage(
  titlePanel("Click-Through Rates Analysis"),
  tabsetPanel(
    tabPanel("Input Data",
             fluidRow(
               column(
                 width = 6,
                 textInput("left_input", "Enter left sidebar data", ""),
                 textInput("center_input", "Enter center page data", ""),
                 textInput("right_input", "Enter right sidebar data", ""),
                 actionButton("submit_btn", "Add Data")
               ),
               column(
                 width = 6,
                 DTOutput("data_table"),
                 actionButton("edit_data_btn", "Edit Selected Data"),
                 actionButton("delete_data_btn", "Delete Selected Data")
               )
             )
    ),
    tabPanel("ANOVA Analysis",
             verbatimTextOutput("output_anova"),
             box(
               title = "LEFT SIDEBAR",
               solidHeader = TRUE,
               status = "primary",
               textOutput("box_left")
             ),
             box(
               title = "CENTER PAGE",
               solidHeader = TRUE,
               status = "warning",
               textOutput("box_center")
             ),
             box(
               title = "RIGHT SIDEBAR",
               solidHeader = TRUE,
               status = "success",
               textOutput("box_right")
             )
    ),
    tabPanel("Visualization",
             sidebarLayout(
               sidebarPanel(
                 selectInput("plotType", "Choose Plot Type",
                             choices = c("Barplot", "Boxplot", "Scatterplot"),
                             selected = "Barplot")
               ),
               mainPanel(
                 plotlyOutput("plot")
               )
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Initialize default data
  rv <- reactiveValues(data = ctr_data, selected_rows = NULL, editing_row = NULL)
  
  # Display data table
  output$data_table <- renderDT({
    datatable(rv$data, editable = TRUE, selection = "multiple")
  })
  
  # Add new data to the table
  observeEvent(input$submit_btn, {
    new_row <- data.frame(
      day = nrow(rv$data) + 1,
      left = as.numeric(input$left_input),
      center = as.numeric(input$center_input),
      right = as.numeric(input$right_input)
    )
    rv$data <- rbind(rv$data, new_row)
  })
  
  # Set selected rows
  observeEvent(input$data_table_rows_selected, {
    rv$selected_rows <- input$data_table_rows_selected
  })
  
  # Delete selected data
  observeEvent(input$delete_data_btn, {
    if (!is.null(rv$selected_rows)) {
      rv$data <- rv$data[-rv$selected_rows, ]
      rv$selected_rows <- NULL
    }
  })
  
  # Edit selected data
  observeEvent(input$edit_data_btn, {
    if (!is.null(rv$selected_rows) && length(rv$selected_rows) == 1) {
      rv$editing_row <- rv$selected_rows
      updateTextInput(session, "left_input", value = as.character(rv$data$left[rv$editing_row]))
      updateTextInput(session, "center_input", value = as.character(rv$data$center[rv$editing_row]))
      updateTextInput(session, "right_input", value = as.character(rv$data$right[rv$editing_row]))
    }
  })
  
  # Save changes after editing
  observeEvent(input$submit_btn, {
    if (!is.null(rv$editing_row)) {
      rv$data$left[rv$editing_row] <- as.numeric(input$left_input)
      rv$data$center[rv$editing_row] <- as.numeric(input$center_input)
      rv$data$right[rv$editing_row] <- as.numeric(input$right_input)
      rv$editing_row <- NULL
    }
  })
  
  # Statistical analysis
  output$output_anova <- renderPrint({
    if (is.null(rv$data)) return(NULL)  # Avoid analysis if data is empty
    result_anova <- aov(cbind(left, center, right) ~ day, data = rv$data)
    print(summary(result_anova))
  })
  
  # Display ANOVA interpretation
  output$box_left <- renderText({
    if (is.null(rv$data)) return(NULL)
    result_anova <- aov(left ~ day, data = rv$data)
    p_value <- format(summary(result_anova)[[1]]$`Pr(>F)`[1], digits = 3)
    if (as.numeric(p_value) > 0.05) {
      paste("By using a confidence level of 95%, obtained a p-value(", p_value, ") > alpha (0.05), it can be concluded that there is no significant difference for the left sidebar group compared to other groups.")
    } else {
      paste("By using a confidence level of 95%, obtained a p-value(", p_value, ") < alpha (0.05), it can be concluded that there is a significant difference for the left sidebar group compared to other groups.")
    }
  })
  
  output$box_center <- renderText({
    if (is.null(rv$data)) return(NULL)
    result_anova <- aov(center ~ day, data = rv$data)
    p_value <- format(summary(result_anova)[[1]]$`Pr(>F)`[1], digits = 3)
    if (as.numeric(p_value) > 0.05) {
      paste("By using a confidence level of 95%, obtained a p-value(", p_value, ") > alpha (0.05), it can be concluded that there is no significant difference for the center page group compared to other groups.")
    } else {
      paste("By using a confidence level of 95%, obtained a p-value(", p_value, ") < alpha (0.05), it can be concluded that there is a significant difference for the center page group compared to other groups.")
    }
  })
  
  output$box_right <- renderText({
    if (is.null(rv$data)) return(NULL)
    result_anova <- aov(right ~ day, data = rv$data)
    p_value <- format(summary(result_anova)[[1]]$`Pr(>F)`[1], digits = 3)
    if (as.numeric(p_value) > 0.05) {
      paste("By using a confidence level of 95%, obtained a p-value (", p_value, ") > alpha (0.05), it can be concluded that there is no significant difference for the right sidebar group compared to other groups.")
    } else {
      paste("By using a confidence level of 95%, obtained a p-value(", p_value, ") < alpha (0.05), it can be concluded that there is a significant difference for the right sidebar group compared to other groups.")
    }
  })
  
  # Data Visualization
  output$plot <- renderPlotly({
    if (is.null(rv$data) || is.null(input$plotType) || input$plotType == "") return(NULL)
    
    if (input$plotType == "Barplot") {
      # Barplot for each variable
      data_plot <- rv$data %>%
        pivot_longer(cols = c(left, center, right), names_to = "Group", values_to = "Value")
      
      colors <- c("mediumpurple1", "lightblue", "pink")
      
      p <- plot_ly(data_plot, x = ~Group, y = ~Value, type = "bar", color = ~Group, colors = colors) %>%
        layout(title = "Sum of Click Based on Location",
               xaxis = list(title = "Location"),
               yaxis = list(title = "Sum"),
               showlegend = FALSE)
      
      return(p)
    } else if (input$plotType == "Boxplot") {
      # Boxplot for each variable
      box_left <- plot_ly(rv$data, y = ~left, type = "box", name = "Left")
      box_center <- plot_ly(rv$data, y = ~center, type = "box", name = "Center")
      box_right <- plot_ly(rv$data, y = ~right, type = "box", name = "Right")
      
      box_combined <- subplot(box_left, box_center, box_right, nrows = 3, shareX = TRUE)
      
      return(box_combined)
    } else if (input$plotType == "Scatterplot") {
      # Scatterplot for each variable comparing to day
      entertaining_data <- rv$data %>%
        pivot_longer(cols = c(left, center, right), names_to = "Group", values_to = "Value")
      
      entertaining_chart <- plot_ly(entertaining_data, x = ~day, y = ~Value, type = "scatter", mode = "lines+markers", color = ~Group) %>%
        layout(title = "Scatterplot",
               xaxis = list(title = "Day"),
               yaxis = list(title = "Value"))
      
      return(entertaining_chart)
    }
  })
}

# Run the application
shinyApp(ui, server)
