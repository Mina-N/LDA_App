# Source helper functions
source("helpers.R")

# Define server logic ----
server <- function(input, output, session) {
    rv <- reactiveValues(
        ask_for_stopwords = TRUE,
        dt_matrix = TRUE,
        stopwords = NULL,
        data = NULL,
        lda_param = NULL,
        dtm = NULL,
        input_data = NULL,
        processing = "Processing has started.",
        processing_done = FALSE,
        lda_mod = FALSE
    )
    
    # TODO: allow user to select number of topics for LDA - currently 13
    #output$topic_num_var <- renderText({ 
    #    paste("The number of topics is ", input$num)
    #});
    
    #output$threshold_var <- renderText({ 
    #    paste("The threshold is ", input$range)
    #})
    
    output$threshold_lambda <- renderText({
        paste("Lambda is ", input$lambda_param)
    })
    
    output$file_var <- renderText({
        req(input$file)
        df <- list()
        if (is.null(rv$stopwords)) {
            tryCatch(
                {
                    for(i in 1:length(input$file[,1])){
                        df[[i]] <- read_excel(input$file[[i, 'datapath']])
                    }
                    
                    rv$input_data = df
                    rv$ask_for_stopwords <- isolate(!rv$ask_for_stopwords)
                },
                error = function(e) {
                    stop(safeError(e))
                }
            )
            
            if (length(input$file[,1]) == 1) {
                paste("The file is ", input$file[1])
            }
            else {
                paste("The files are ", input$file[1])
            }
        }
        processing()
    })
    
    processing <- reactive({
        rv$processing
    })
    
    output$plot_var <- renderVis({
        req(rv$lda_mod)
        readLines("lda_model/lda.json")
    })
    
    observeEvent(rv$ask_for_stopwords, ignoreInit = TRUE, {
        # Combine data
        data <- combine_data(rv$input_data)
        
        # Modify data
        data <- modify_data(data)
        
        rv$data <- data
        
        showModal(modalDialog(
            title = "Stopwords",
            radioButtons("stopwords", h3("I want to load custom stopwords."),
                         choices = list("Yes" = 1, "No" = 2)),
            footer = actionButton("continue_stopwords", "Continue", class = "btn-primary")
        ))
    })
    
    observeEvent(input$continue_stopwords, {
        removeModal()
        stopwords <- NULL
        if (input$stopwords == 1) {
            showModal(modalDialog(
                title = "Stopwords File",
                fileInput("stopword_file", h3("Stopwords file input"), multiple = FALSE, accept = c(".xls", ".xlsx")),
                footer = actionButton("continue_stopfile", "Continue", class = "btn-primary")
            ))
        }
    })
    
    observeEvent(input$continue_stopfile, {
        stopwords <- read_excel(input$stopword_file$datapath, col_names = FALSE)
        rv$stopwords <- stopwords
        removeModal()
        showModal(modalDialog(
            title = "LDA Parameters",
            radioButtons("lda_param", h3("I want to load LDA parameters"),
                         choices = list("Yes" = 1, "No, I want to train on a new corpus" = 2)),
            footer = actionButton("continue_lda", "Continue", class = "btn-primary")
        ))
    })
    
    observeEvent(input$continue_lda, {
        removeModal()
        dtm <- dt_matrix_vocab(rv$data, rv$stopwords)
        rv$dtm <- dtm
        
        if (input$lda_param == 1) {
            showModal(modalDialog(
                title = "LDA Parameters File",
                fileInput("lda_file", h3("LDA parameters input"), multiple = FALSE, accept = c(".csv")),
                footer = actionButton("continue_ldafile", "Continue", class = "btn-primary")
            ))
        }
        else {
            lda_param <- read_csv(optimize_param(dtm))
            rv$lda_param <- lda_param
        }
    })
    
    observeEvent(input$continue_ldafile, {
        removeModal()
        lda_param <- read_csv(input$lda_file$datapath)
        rv$lda_param <- lda_param
        showModal(modalDialog(
            title = "LDA Algorithm Lambda",
            helpText("Select whether frequency of terms (lambda < 1) or probability of chance
                     to observe a word in a given topic (lambda = 1) is more important when creating topic keywords."),
            sliderInput("lambda_param", 
                        label = "Lambda:",
                        min = 0, max = 1, value = 0),
            footer = actionButton("continue_lambda", "Continue", class = "btn-primary")
        ))
    })
    
    observeEvent(input$continue_lambda, {
        removeModal()
        rv$processing <- "Processing is complete. Image files and topic groups have been saved to the app directory."
        run_lda(rv$lda_param, rv$dtm, rv$data, input$lambda_param)
        rv$lda_mod <- isolate(!rv$lda_mod)
    })
}





