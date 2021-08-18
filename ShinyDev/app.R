
#library(tabplot)
library(DT)
library(psych)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(dplyr)
#library(devtools)
#install_github("mtennekes/tabplot")
library(tabplot)

ui <- fluidPage(
    mainPanel(
        titlePanel("Shiny App for Data Exploration"),
        
        
        tabsetPanel(
            tabPanel("Data input",
                     p("Before uploading your data, check that it is clean, and make sure missing values are denoted by NA"),
                     tags$hr(),
                     p("Select the options that match your CSV file, then upload your file"),
                     
                     radioButtons(inputId = 'header',
                                  label = 'Header',
                                  choices = c('Columns have header' = 'Yes',
                                              'Columns do not have headers'='No'),
                                  selected = 'Yes'),
                     
                     radioButtons('sep', 'Separator', 
                                  c(Comma = ',', 
                                    Semicolon = ';', 
                                    Tab = '\t'), 
                                  ','),
                     
                     radioButtons('quote', 'Quote', 
                                  c(None='', 
                                    'Double Quote'='"',
                                    'Single Quote'="'"),
                                  '"'),
                     
                     tags$hr(),
                     
                     
                     fileInput('file1', 'Choose a CSV file to upload:',
                               accept = c(
                                   'text/csv',
                                   'text/comma-separated-values',
                                   'text/tab-separated-values',
                                   'text/plain',
                                   '.csv',
                                   '.tsv'
                               )),
                     p("After uploading your CSV file, click on the 'Inspect the data' tab")
            ), #end tab
            
            tabPanel("Inspect the data",
                     
                     uiOutput("Sort_by"), 
                     tags$hr(),
                     uiOutput("Select_var"),
                     tags$hr(),
                     # verbatimTextOutput("selected_var"),
                     # tags$hr(),
                     p("Here is the raw data from the CSV file"),
                     DT::dataTableOutput('contents'),
                     tags$hr(),
                     p("This shows the data summary"),
                     verbatimTextOutput("Sum")
                     
            ), #endtab
            
            tabPanel("Tableplot",
                     
                     p('For the data exploration, we will be using the tableplot from the tabplot package'),
                     p('The tableplot is an exploratory tool that is useful to explore to relationships between the variables in a dataset'),
                     p('It can be used to highlight any anomaly/equivocal patterns in a dataset'),
                     p('It is also useful to visualize the occurrence or pattern of missing in a dataset'),
                     tags$hr(),
                     uiOutput("Sort_by_column"), #sort by column is coming from renderUI in server.r
                     tags$hr(),
                     uiOutput("Select_column"), #select the column to display is from renderUI in server.r
                     tags$hr(),
                     uiOutput("No_Bins"),
                     tags$hr(),
                     plotOutput('tableplot')
            ) 
        )
    )
)# end  tab

server <- function(input, output){
    
    # read in the CSV
    the_data_func <- reactive({
        inFile <- input$file1
        if(is.null(inFile)) return(NULL)
        the_data <- read.csv(inFile$datapath, header = (input$header == "Yes"),
                             sep = input$sep, quote = input$quote, stringsAsFactors=FALSE)
        return(the_data)
    })
    
    #col names
    var <- reactive({
        names(the_data_func())
    })
    
    # select variable to sort on
    output$Sort_by <- renderUI({
        selectInput("Sort_on", label="Sort on:", 
                    choices=var())
        
    })
    
    # Check boxes to choose columns
    output$Select_var <- renderUI({
        # select multiple columns to visualise using tabplot
        varSelectInput("Select_col", "Select the variable to display", 
                       data = the_data_func(),
                       multiple = TRUE)
    })
    
    
    # display a table of CSV content
    output$contents <- DT::renderDataTable({
        dat = the_data_func()
        subset_table <- dat  %>%
            dplyr::select(!!!input$Select_col)
        datatable(subset_table, options = list(
            order=list(match(input$Sort_on, names(the_data_func())), 'asc')))
    })
    
    # Summary
    output$Sum <- renderPrint({
        str(the_data_func())
    })
    
    # select variable to sort on
    output$Sort_by_column <- renderUI({
        selectInput("Sort_var", label="Sort table plot on:", 
                    choices=var())
        
    })
    
    
    output$Select_column <- renderUI({
        # select multiple columns to visualise using tabplot
        varSelectInput("Choose_colname", "Select the variable to display", 
                       data = the_data_func(),
                       multiple = TRUE)
    })
    
    # choose the number of bins
    output$No_Bins <- renderUI({
        # slide the number of bins
        sliderInput("nBins", label="Number of bins:", value=100,
                    min = 2, max = 500, step = 1)
    })
    
    NBins <-reactive({
        max(2,as.numeric(input$nBins), na.rm=TRUE)
    })
    
    #tableplot
    output$tableplot <- renderPlot({
        dat = the_data_func()
        subset_table <- dat %>%
            dplyr::select(!!!input$Choose_colname) #!!! big-bang operator
        tabplot::tableplot(subset_table, sortCol = input$Sort_var,
                           nBins = NBins(), colorNA_num="orange", colorNA = "orange", 
                           pals=list("BrBG") , numMode="MB-ML", sample=F)
    })
}
# Run the application 
shinyApp(ui = ui, server = server)

