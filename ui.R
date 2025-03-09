


library(shiny)
library(DT)

# Define the UI
ui <- navbarPage(
  title = "wiSDOM",
  
  tabPanel(
    "Data",
    h3("Upload the Data"),
    fileInput("file", "Please Upload your File"),
    DTOutput("data_table")
  ),
  
  tabPanel(
    "Statistical Analysis",
    sidebarLayout(
      sidebarPanel(
        h4("Select Variables for Analysis"),
        selectInput("selected_variable", "Select Variable:", choices = NULL, multiple = TRUE),  
        actionButton("run_summary", "Show Summary")  
      ),
      mainPanel(
      #  h4("Uploaded Data"),
       # DTOutput("stat_data_table"),
        h4("Summary Statistics Output"),  
        verbatimTextOutput("summary_output")  
      )
    )
  ),
  
  tabPanel(
    "Graphical Presentation",
    sidebarLayout(
      sidebarPanel(
        h4("Select Variables for Graph"),
        selectInput("plot_type", "Select Plot Type:", choices = c("Scatter Plot", "Bar Chart","Density Plot")),
        selectInput("x_variable", "Select X Variable:", choices = NULL),
        conditionalPanel(
          condition = "input.plot_type == 'Scatter Plot' ||
                      # input.plot_type == 'Boxplot' ||
                      # input.plot_type == 'Line Plot'",
          selectInput("y_variable", "Select Y Variable:", choices = NULL)
        ),
        actionButton("plot_graph", "Generate Graph")
      ),
      mainPanel(
        h4("Generated Plot"),
        plotOutput("graph_output")
      )
    )
  )
  ,
  
  tabPanel(
    "Tests",
    sidebarLayout(
      sidebarPanel(
        h4("Select Test Type"),
        selectInput("test_type", "Choose a Test:", 
                    choices = c("Chi-Square Test", "Fisher's Exact Test")),
        h4("Select Variables for Testing"),
        selectInput("test_variable", "Dependent Variable:", choices = NULL),  
        selectInput("independent_variable", "Independent Variable:", choices = NULL),  
        actionButton("run_test", "Run Test")  
      ),
      mainPanel(
        h4("Test Results"),
        verbatimTextOutput("test_output"),
        DTOutput("test_table")
      )
    )
  )
)

