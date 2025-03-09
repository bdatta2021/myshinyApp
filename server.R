



# Load required libraries
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(randomForest)
library(MASS)

# Ensure dataset exists before loading
if (!file.exists("updated_data_with_group.csv")) {
  stop("Error: Dataset 'updated_data_with_group.csv' not found in the working directory!")
}

# Load dataset
df <- read.csv("updated_data_with_group.csv", stringsAsFactors = FALSE)

server <- function(input, output, session) { 
  # Store the dataset reactively
  stat_data <- reactiveVal(NULL)  # Initialize with df
  
  # Ensure file upload updates dataset
  observeEvent(input$file, {
    req(input$file)
    new_data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    stat_data(new_data)
  })
  
  
  
  # Render the uploaded data table in the Data tab
  output$data_table <- renderDT({
    req(stat_data())  # Ensure data is available
    datatable(stat_data(), options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  
  
  
  
  
  # Dynamically update variable choices when data changes
  observe({
    req(stat_data()) 
    updateSelectInput(session, "selected_variable", choices = colnames(stat_data()))
    updateSelectInput(session, "x_variable", choices = colnames(stat_data()))
    updateSelectInput(session, "y_variable", choices = colnames(stat_data()))
    updateSelectInput(session, "test_variable", choices = colnames(stat_data()))
    updateSelectInput(session, "independent_variable", choices = colnames(stat_data()))
  })
  
  # Render Data Table using stat_data()
 # output$stat_data_table <- renderDT({
  #  req(stat_data())  
   # datatable(stat_data(), options = list(pageLength = 10, autoWidth = TRUE))
  #})
  
  # Compute and Show Summary Statistics
  output$summary_output <- renderPrint({
    req(input$selected_variable)  
    df_selected <- stat_data()  
    selected_vars <- input$selected_variable  
    continuous_vars <- selected_vars[sapply(df_selected[selected_vars], is.numeric)]
    
    if (length(continuous_vars) == 0) {
      return("No continuous variables selected for summary statistics.")
    }
    
    summary(df_selected[continuous_vars])  
  })
  
  
  
  
  # Graphical Presentation: Generate Plot
  output$graph_output <- renderPlot({
    req(stat_data(), input$x_variable)
    
    df_selected <- stat_data()
    
    if (!(input$x_variable %in% colnames(df_selected))) {
      return(NULL)
    }
    
    
    
    
    
    # Define different plot types
    plot <- switch(input$plot_type,
                   "Scatter Plot" = {
                     req(input$y_variable)
                     ggplot(df_selected, aes_string(x = input$x_variable, y = input$y_variable)) +
                       geom_point(color = "blue") +
                       labs(title = paste("Scatter Plot of", input$x_variable, "vs", input$y_variable),
                            x = input$x_variable, 
                            y = input$y_variable) +
                       theme_minimal()
                   },
                   
                   # "Boxplot" = {
                   #   req(input$y_variable)
                   #   ggplot(df_selected, aes_string(x = input$x_variable, y = input$y_variable)) +
                   #     geom_boxplot() +
                   #     labs(title = paste("Boxplot of", input$x_variable, "vs", input$y_variable),
                   #          x = input$x_variable, 
                   #          y = input$y_variable) +
                   #     theme_minimal()
                   # },
                   "Bar Chart" = ggplot(df_selected, aes_string(x = input$x_variable)) +
                     geom_bar() +
                     labs(title = paste("Bar Chart of", input$x_variable), x = input$x_variable, y = "Count") +
                     theme_minimal(),
                   "Density Plot" = ggplot(df_selected, aes_string(x = input$x_variable)) +
                     geom_density(fill = "green", alpha = 0.5) +
                     labs(title = paste("Density Plot of", input$x_variable), x = input$x_variable, y = "Density") +
                     theme_minimal(),
                   # "Violin Plot" = {
                   #   req(input$y_variable)
                   #   ggplot(df_selected, aes_string(x = input$x_variable, y = input$y_variable)) +
                   #     geom_violin() +
                   #     labs(title = paste("Violin Plot of", input$x_variable, "vs", input$y_variable),
                   #          x = input$x_variable, 
                   #          y = input$y_variable) +
                   #     theme_minimal()
                   # },
                   # 
                   # "Line Plot" = {
                   #   req(input$y_variable)
                   #   ggplot(df_selected, aes_string(x = input$x_variable, y = input$y_variable)) +
                   #     geom_line() +
                   #     labs(title = paste("Line Plot of", input$x_variable, "vs", input$y_variable),
                   #          x = input$x_variable, 
                   #          y = input$y_variable) +
                   #     theme_minimal()
              #     },
                   
    )
    
    plot
  })
    
    
    
  
  # Perform Statistical Tests when Button is Clicked
  output$test_output <- renderPrint({
    req(stat_data(), input$test_variable, input$independent_variable, input$test_type)  
    df_selected <- stat_data()
    
    print(paste("Selected Test:", input$test_type))  # Debugging line
    
    # Extract selected variables
    dependent_var <- input$test_variable
    independent_var <- input$independent_variable
    
    if (!(dependent_var %in% colnames(df_selected)) || !(independent_var %in% colnames(df_selected))) {
      return("Error: Selected variables are not in the dataset.")
    }
    
    table_data <- table(df_selected[[dependent_var]], df_selected[[independent_var]])
    
    if (input$test_type == "Fisher's Exact Test") {
      dep_levels <- unique(df_selected[[dependent_var]][!is.na(df_selected[[dependent_var]])])
      ind_levels <- unique(df_selected[[independent_var]][!is.na(df_selected[[independent_var]])])
      
      if (length(dep_levels) > 2) {
        df_selected[[dependent_var]] <- ifelse(df_selected[[dependent_var]] %in% dep_levels[1], "Group1", "Group2")
      }
      if (length(ind_levels) > 2) {
        df_selected[[independent_var]] <- ifelse(df_selected[[independent_var]] %in% ind_levels[1], "Group1", "Group2")
      }
      
      cat("\nRunning Fisher's Exact Test...\n")
      return(fisher.test(table_data))
    }
    
    test_result <- switch(input$test_type,
                          "Chi-Square Test" = { chisq.test(table_data) },
                          
                          { cat("No valid test selected.\n") }
    )
    
    print(test_result)
  })
}
