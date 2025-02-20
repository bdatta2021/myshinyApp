

library(DT)



# Define the UI
ui <- navbarPage(
  title = "wiSDOM",
  
  # Data Tab
  tabPanel(
    "Data",
    h3("Upload the Data"),
    fileInput("file", "Please Upload your File"),
    tableOutput("data_table")
  ),
  
  # Statistical Analysis Tab
  tabPanel(
    "Statistical Analysis",
    sidebarLayout(
      sidebarPanel(
        fileInput("stat_file", "Upload the Data", accept = ".csv"),
        hr(),
        
        h4("Select Variables for Analysis"),
        selectInput("selected_variable", "Select Variable:", choices = NULL, multiple = TRUE),  
        
        hr(),
        h4("Summary Statistics"),  
        actionButton("run_summary", "Show Summary")  
      ),
      
      mainPanel(
        h4("Uploaded Data"),
        DTOutput("stat_data_table"),
        hr(),
        
        h4("Summary Statistics Output"),  
        verbatimTextOutput("summary_output"),  
        
        hr(),
        uiOutput("result_output"),
        plotOutput("result_plot"),
        DTOutput("result_table")
      )
    )
  ),
  
  # Tests Tab 
  tabPanel(
    "Tests",
    sidebarLayout(
      sidebarPanel(
        h4("Select Test Type"),
        selectInput("test_type", "Choose a Test:", 
                    choices = c("Chi-Square Test", "Fisher's Exact Test", "Logistic Regression", "T-Test", "ANOVA", "Random Forest")),
        hr(),
        
        h4("Select Variables for Testing"),
        selectInput("test_variable", "Dependent Variable:", choices = NULL),  
        selectInput("independent_variable", "Independent Variable:", choices = NULL),  
        
        actionButton("run_test", "Run Test")  
      ),
      
      mainPanel(
        h4("Test Results"),
        verbatimTextOutput("test_output"),
        plotOutput("test_plot"),
        DTOutput("test_table")
      )
    )
  )
)
