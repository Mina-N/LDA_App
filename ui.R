

# Define UI ----
ui <- fluidPage(
    titlePanel("Topic Modeling NINR"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Create topic models with
                     grant application data."),
            
            # Select grant .xlsx files
            fileInput("file", h3("Grant application file input"), multiple = TRUE, accept = c(".xls", ".xlsx")),
            
            helpText("Select whether intermediate output should be written to Excel files."),
            
            radioButtons("intermediate", h3("I want intermediate output written to a file."),
                         choices = list("Yes" = 1, "No" = 2)),
            
            
            #helpText("Enter the number of topic groups to be created. To use a 
            #         number optimized by LDA, select 0."),
            
            # Choose to either force a constant number of topic groups or let LDA determine number of topic groups
            #numericInput("num", 
            #             h3("Numeric input"), 
            #             value = 0),
            
        
            #helpText("Select the threshold that determines which topics appear in the ggupset plot."),
            
            # Select the threshold that determines which topics appear in the ggupset plot
            #sliderInput("range", 
            #            label = "Topic Threshold:",
            #            min = 0, max = 100, value = 0),
        ),
        
        mainPanel(
            #textOutput("topic_num_var"),
            textOutput("threshold_var"),
            textOutput("threshold_lambda"),
            textOutput("file_var"),
            textOutput("render"),
            visOutput("plot_var")
        )
    )
)

