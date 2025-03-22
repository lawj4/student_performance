library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# Load data
data <- read.csv("data.csv")

# UI
ui <- fluidPage(
    titlePanel("Student Performance Dashboard"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("xvar", "Select X-axis Variable:", 
                        choices = c("Hours_Studied", "Attendance",
                                    "Sleep_Hours", "Tutoring_Sessions", "Family_Income")),
            selectInput("colorvar", "Color By:", 
                        choices = c("Gender", "School_Type", "Parental_Education_Level")),
            sliderInput("attendance", "Filter by Attendance (%)", 
                        min = min(data$Attendance, na.rm = TRUE), 
                        max = max(data$Attendance, na.rm = TRUE), 
                        value = c(min(data$Attendance, na.rm = TRUE), max(data$Attendance, na.rm = TRUE))),
            checkboxInput("showTable", "Show Data Table", value = TRUE),
            tags$h2("Box Plots Filter"),

            selectInput("resource_variable", "Select Variable for Access to Resources:",
                choices = c("Access_to_Resources", "Parental_Involvement", "Motivation_Level","Family_Income","Peer_Influence")),
            uiOutput("summaryBox")
        ),
        
        mainPanel(
            plotOutput("scatterPlot"),
            DTOutput("dataTable")
        )
    )
)

# Server
server <- function(input, output) {
    # Filtered data based on attendance range
    filtered_data <- reactive({
        data %>%
            filter(Attendance >= input$attendance[1] & Attendance <= input$attendance[2])
    })
    
    # Compute summary statistics
    summary_stats <- reactive({
        filtered <- filtered_data()
        list(
            std_dev = sd(filtered$Exam_Score, na.rm = TRUE),
            median = median(filtered$Exam_Score, na.rm = TRUE),
            Q1 = quantile(filtered$Exam_Score, 0.25, na.rm = TRUE),
            Q3 = quantile(filtered$Exam_Score, 0.75, na.rm = TRUE)
        )
    })
    
    # Render summary statistics
    output$summaryBox <- renderUI({
        stats <- summary_stats()
        
        # Create the plot for boxplots separated by Access_to_Resources
        tagList(
            plotOutput("boxPlot")  # Boxplot output rendered in summary box
        )
    })
    
    # Boxplot for the data separated by Access_to_Resources (or the selected resource variable)
    output$boxPlot <- renderPlot({
        filtered <- filtered_data()  # Get filtered data
        
        # Use the selected variable for Access_to_Resources or the resource variable
        resource_variable <- input$resource_variable  # This is dynamically chosen by user
        
        # Calculate median and quartiles (Q1, Q3) for each resource_variable category
        quartiles <- filtered %>%
            group_by(.data[[resource_variable]]) %>%
            summarise(
                Q1 = quantile(Exam_Score, 0.25, na.rm = TRUE),
                Median = median(Exam_Score, na.rm = TRUE),
                Q3 = quantile(Exam_Score, 0.75, na.rm = TRUE)
            )
        
        # Calculate median Exam_Score for each resource_variable category for sorting
        median_values <- quartiles %>%
            arrange(Median)  # Sort by median value

        # Reorder the factor for the selected resource variable based on the sorted median values
        filtered[[resource_variable]] <- factor(filtered[[resource_variable]], 
                                                 levels = median_values[[resource_variable]])
        
        # Create a boxplot with the selected resource variable ordered by median Exam_Score
        p <- ggplot(filtered, aes(x = .data[[resource_variable]], y = Exam_Score, fill = .data[[resource_variable]])) +
            geom_boxplot(outlier.shape = NA,  # Remove outlier dots
                         color = "black", 
                         whisker.width = 0.5) +  # Control whisker width
            stat_boxplot(geom = "errorbar", width = 0.5) +  # Solid lines at whiskers
            labs(title = paste("Box and Whisker Plot of Exam Scores (Sorted by Median)"),
                 x = resource_variable, y = "Exam Score") +
            theme_minimal()
        
        # Add labels for Q1, Median, and Q3
        p + geom_text(data = quartiles, 
                      aes(x = .data[[resource_variable]], y = Q1, label = paste("Q1:", round(Q1, 2))),
                      position = position_nudge(y = -5), size = 3) +
            geom_text(data = quartiles, 
                      aes(x = .data[[resource_variable]], y = Median, label = paste("Median:", round(Median, 2))),
                      position = position_nudge(y = 5), size = 3) +
            geom_text(data = quartiles, 
                      aes(x = .data[[resource_variable]], y = Q3, label = paste("Q3:", round(Q3, 2))),
                      position = position_nudge(y = 5), size = 3)
    })

    # Scatter Plot (filtered by attendance)
    output$scatterPlot <- renderPlot({
        ggplot(filtered_data(), aes(x = .data[[input$xvar]], y = Exam_Score, color = .data[[input$colorvar]])) +
            geom_point(size = 3, alpha = 0.7) +
            geom_smooth(method = "lm", se = FALSE, color = "black") +
            labs(title = "Exam Score vs Selected Factor", x = input$xvar, y = "Exam Score") +
            theme_minimal()
    })
    
    # Data Table (filtered by attendance)
    output$dataTable <- renderDT({
        if (input$showTable) {
            datatable(filtered_data())
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

