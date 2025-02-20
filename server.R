

# Load required libraries

library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(randomForest)
library(MASS)

# Load dataset using relative path
df <- read.csv("updated_data_with_group.csv", stringsAsFactors = FALSE)

server <- function(input, output, session) { 
  # Store the dataset reactively
  stat_data <- reactiveVal(df)  # Initialize with df
  
  # File uploading for "Data" tab
  output$data_table <- renderTable({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Observe file input for dataset upload
  observeEvent(input$stat_file, {
    req(input$stat_file)
    df_uploaded <- read.csv(input$stat_file$datapath)
    
    # Check if Group column exists
    if (!"Group" %in% colnames(df_uploaded)) {
      showNotification("Error: The dataset must contain a 'Group' column.", type = "error")
      return()
    }
    
    #Update reactive dataset
    stat_data(df_uploaded)
    showNotification("Data uploaded successfully!", type = "message")
    
    #  Update dropdown choices dynamically
    updateSelectInput(session, "selected_variable", choices = colnames(df_uploaded))
    updateSelectInput(session, "test_variable", choices = colnames(df_uploaded))
    updateSelectInput(session, "independent_variable", choices = colnames(df_uploaded))
  })
  
  # Render Data Table using stat_data()
  output$stat_data_table <- renderDT({
    req(stat_data())
    datatable(stat_data(), options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  # Compute and Show Summary Statistics when Button is Clicked
  output$summary_output <- renderPrint({
    req(stat_data(), input$selected_variable)  
    df_selected <- stat_data()  
    selected_vars <- input$selected_variable  
    continuous_vars <- selected_vars[sapply(df_selected[selected_vars], is.numeric)]
    
    if (length(continuous_vars) == 0) {
      return("No continuous variables selected for summary statistics.")
    }
    
    summary(df_selected[continuous_vars])  
  })
  
  # Perform Statistical Tests when Button is Clicked
  output$test_output <- renderPrint({
    req(stat_data(), input$test_variable, input$independent_variable, input$test_type)  
    df_selected <- stat_data()
    
    # Print test type for debugging
    print(paste("Selected Test:", input$test_type))
    
    # Extract selected variables
    dependent_var <- input$test_variable
    independent_var <- input$independent_variable
    
    # Ensure variables exist in the dataset
    if (!(dependent_var %in% colnames(df_selected)) || !(independent_var %in% colnames(df_selected))) {
      return("Error: Selected variables are not in the dataset.")
    }
    
    # Create Contingency Table
    table_data <- table(df_selected[[dependent_var]], df_selected[[independent_var]])
    
    # If Fisher's Exact Test is selected, ensure a 2x2 contingency table
    if (input$test_type == "Fisher's Exact Test") {
      # Reduce categories to 2 levels
      dep_levels <- unique(df_selected[[dependent_var]][!is.na(df_selected[[dependent_var]])])
      ind_levels <- unique(df_selected[[independent_var]][!is.na(df_selected[[independent_var]])])
      
      if (length(dep_levels) > 2) {
        df_selected[[dependent_var]] <- ifelse(df_selected[[dependent_var]] %in% dep_levels[1], "Group1", "Group2")
      }
      if (length(ind_levels) > 2) {
        df_selected[[independent_var]] <- ifelse(df_selected[[independent_var]] %in% ind_levels[1], "Group1", "Group2")
      }
      
      # Run Fisher’s Exact Test
      cat("\nRunning Fisher's Exact Test...\n")
      return(fisher.test(table_data))
    }
    
    # Run other statistical tests
    test_result <- switch(input$test_type,
                          "Chi-Square Test" = { chisq.test(table_data) },
                          "Logistic Regression" = { glm(as.formula(paste(dependent_var, "~", independent_var)), data = df_selected, family = binomial) },
                          "T-Test" = { t.test(df_selected[[dependent_var]] ~ df_selected[[independent_var]], data = df_selected) },
                          "ANOVA" = { aov(as.formula(paste(dependent_var, "~", independent_var)), data = df_selected) },
                          "Random Forest" = { randomForest(as.formula(paste(dependent_var, "~", independent_var)), data = df_selected, importance = TRUE) },
                          { cat("No valid test selected.\n") }
    )
    
    print(test_result)
  })
  
  output$test_table <- renderDT({
    datatable(stat_data())  # Using stat_data() instead of df for dynamic updates
  })
  
  
}

