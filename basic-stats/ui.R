####################################################
#      Summary & BAsic Stats                        #
####################################################

library("shiny")
#library("foreign")

shinyUI(pageWithSidebar(
  # Header:
 # headerPanel("Regression App"),
  headerPanel(title=div(img(src="isb.png",align = "right"), h2("Basic Data Stats App", style="bold"))),
  
  # Input in sidepanel:
  sidebarPanel(

    h5(p("Data Input")),
    fileInput("file", "Upload input data (csv file with header)"),
    h5(p("Data Selection")),
    htmlOutput("xvarselect"),
    htmlOutput("fxvarselect"),
    #submitButton(text = "Apply Changes", icon("refresh")),br(),
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
                         p("Once csv file is uploaded successfully, all variables in the data file will reflect in left-side 'Data Selection' Panel. 
                            If you want to drop any variable from the analysis, just uncheck that variable and it will be dropped from the analysis."),
                          p("Next you can select factor (categorical) variables just by selecting that variable in the 'Select factor (categorical) variables' list.",align="justify"),
                         br(),
                       #  h4(p("Download Sample Input Files")),
                         # br(),
                        # downloadButton('downloadData', 'Download Sample Data (works only in browsers)'),
                        # br(),
                        # br(),
                        # p("*Please note that download will not work with RStudio interface. Download will work only in web-browsers."),
                         ),
                tabPanel("Data",h4("Data"), verbatimTextOutput("head"),verbatimTextOutput("tail"),
                         h4("Data Summary"),verbatimTextOutput("summary")),
                tabPanel("Missing Data", h4("Missing Data Rows"),verbatimTextOutput("missing")),
                tabPanel("Correlation",
                          h4("Correlation Visulization"),plotOutput("corplot"),
                          h4("Correlation Table"), verbatimTextOutput("correlation")),
                tabPanel("Box Plot", h4("Box Plots"),plotOutput("bplot")),
                tabPanel("Data Visulization",br(),
                         h4("randomly select 500 rows from input data - if less than 500 rows in dataset, select whole dataset"),plotOutput("heatmap1")),
                tabPanel("Outliers",htmlOutput("outselect"), 
                         #h4("selcet a variable at the top for Rosner's outliers"),
                         plotOutput("hist"),
                         verbatimTextOutput("outlier")),
                tabPanel("Data with Dummy Variables",
                         br(),
                         h4(p("download input data with dummy variable columns added")),
                         downloadButton('downloadDatanew', 'download data'),  )
                )
      ) 
    ) 
  )
