####################################################
#      Summary & OLS App                           #
####################################################

library("shiny")
#library("foreign")

shinyUI(pageWithSidebar(
  # Header:
  headerPanel("OLS App"),
  # Input in sidepanel:
  sidebarPanel(

    h5(p("Data Input")),
    fileInput("file", "Upload input data (csv file with header)"),
    fileInput("filep", "Upload prediction data (csv file with header)"),
    h5(p("Data Selection")),
    htmlOutput("yvarselect"),
    htmlOutput("xvarselect"),
    htmlOutput("fxvarselect"),
    br()
  ),
  # Main:
  mainPanel( 
    
    tabsetPanel(type = "tabs",
                #
                
                tabPanel("Overview",
                         h4(p("How to use this shiny application")),
                         p("This shiny application require one data input from the user. To do so, click on the Browse (in left side-bar panel) and upload the csv data input file.
                           Note that this application can read only csv file (comma delimited file), so if you don't have csv input data file, first convert your data in csv format 
                           and then proceed. Make sure you have top row as variable names.",align="justify"),
                         p("Once csv file is uploaded successfully, variables in the data file will reflect in left-side Data Selection Panel. Now you can select 
                            dependent variable (Y Variable) from drop-down menu. By default all other remaining variables will be selected as explanatory variables (X variables). 
                              If you want to drop any variable from explanatory variables, just uncheck that variable and it will be dropped from the model. 
                            If any of the variables selected in explanatory variables is a factor variable, you can define that variable as factor variable just
                            by selecting that variable in the last list of variables
                           ",align="justify"),
                         br(),
                         h4(p("Download Sample Input Files")),
                         # br(),
                         downloadButton('downloadData', 'Download model training input file (works only in browsers)'),
                         br(),
                         br(),
                         p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file."),
                         ),
                tabPanel("Summary Stats",h4("Data"), verbatimTextOutput("head"),verbatimTextOutput("tail"),
                         h4("Data Summary"),verbatimTextOutput("summary"),h4("Missing Data Rows"),verbatimTextOutput("missing")),
                tabPanel("Summary OLS", h4("Summary OLS Model"),verbatimTextOutput("olssummary"),
                         h4("Summary OLS standardized model"),
                         verbatimTextOutput("olssummarystd")),
                tabPanel("Data with predicted Y", h4("Download Input Data with Predicted Y"),
                         downloadButton('downloadData2', 'Download data (Works only in browser)'),
                         br(),br(),tableOutput("datatable")),
                tabPanel("Correlation",h4("Correlation Table - Input data"), verbatimTextOutput("correlation"),
                         h4("Correlation Visulization - Input Data"),plotOutput("corplot")),
                tabPanel("Corr-Hist",h4("Discriptive Analytics - Input Data"),plotOutput("heatmap1")),
                tabPanel("Residuals Plot",h4("Fitted Values vs Residuals - Input Data"),
                         plotOutput("resplot2"),h4("Fitted Values vs Y - Input Data"),
                         plotOutput("resplot3"),h4("Residuals plot - Input Data"),
                         plotOutput("resplot1")),
                tabPanel("Prediction New Data",br(),
                         h4("First 10 rows of predicted data (upload prediction data)"),
                         p('"Yhat" column is the predicted value.'),
                         verbatimTextOutput('prediction'),
                         h4("Download Predicted data"),
                         downloadButton('downloadData1', 'Download data (Works only in browser)')      ) 
                )
      ) 
    ) 
  )
