####################################################
#      Summary & GLM App                           #
####################################################

library("shiny")
#library("foreign")

shinyUI(pageWithSidebar(
  # Header:
  #headerPanel("Logistic Regression (Binary Logit) App"),
  headerPanel( title=div(img(src="isb.png",align = "right"), h2("Logistic Regression (Binary Logit) App", style="bold")), windowTitle	='Binary Logit'),
  # Input in sidepanel:
  sidebarPanel(

    h5(p("Data Input")),
    fileInput("file", "Upload input data (csv file with header)"),
    fileInput("filep", "Upload new data for prediction (csv file with header)"),
    h5(p("Data Selection")),
    htmlOutput("yvarselect"),
    htmlOutput("xvarselect"),
  #  submitButton(text = "Apply Changes", icon("refresh")),br(),
    htmlOutput("fxvarselect"),
    br()
  ),
  # Main:
  mainPanel( 
    
    tabsetPanel(type = "tabs",
                #
                
                tabPanel("Overview",
                         h4(p("How to use this shiny application")),
                         p("This shiny application require one data input from the user. To do so, click on the Browse (in left side-bar panel)
                            and upload the csv data input file.
                           Note that this application can read only csv file (comma delimited file), so if you don't have csv input data file, first convert your data in csv format 
                           and then proceed. Make sure you have top row as variable names.",
                           align="justify"),
                         p("Once csv file is uploaded successfully, variables in the data file will reflect in left-side Data Selection Panel. Now you can select 
                            dependent variable (Y Variable) from drop-down menu. By default all other remaining variables will be selected as explanatory variables (X variables). 
                              If you want to drop any variable from explanatory variables, just uncheck that variable and it will be dropped from the model analysis. 
                            If any of the explanatory variables is a factor variable, you can define that variable as factor variable by selecting that variable in the last list of variables.",
                           align="justify"),
                         p("Binary logit classification model trains better when observations are equal distributed between two classes (0 and 1 outcomes).",
                           align="justify"),
                         br(),
                    #     h4(p("Download Sample Input Files")),
                         # br(),
                     #    downloadButton('downloadData', 'Download Sample Data (works only in browsers)'),
                      #   br(),
                       #  br(),
                        # p("*Please note that download will not work with RStudio interface. Download will work only in web-browsers."),
                         ),
                tabPanel("Data Summary",h4("Data"), verbatimTextOutput("head"),verbatimTextOutput("tail"),
                         h4("Data Summary"),verbatimTextOutput("summary"),h4("Missing Data Rows"),verbatimTextOutput("missing")),
                tabPanel("Summary Logit", h4("Summary Logit Model"),verbatimTextOutput("olssummary"),
                              h4('Confusion Matrix'), verbatimTextOutput("validation")),
                         #h4("Summary OLS standardized model"), verbatimTextOutput("olssummarystd")),
                tabPanel("Prediction Input Data", h4('Input data with prediction'),
                         p('"Y.Prob" column is the predicted probability of Y=1.'),
                         downloadButton('downloadData2', 'Download data (works only in browser)'),
                         br(),br(),tableOutput("datatable") #verbatimTextOutput('inputprediction')
                         ),
               # tabPanel("Correlation",h4("Correlation Table - Input data"), verbatimTextOutput("correlation"),
               #          h4("Correlation Visulization - Input Data"),plotOutput("corplot")),
                tabPanel("ROC", 
                         sliderInput('cutoff','Cutoff Probability (default=0.5)',0,1,0.5),
                         h4("Confusion Matrix Summary"),verbatimTextOutput("confusionmatrix"),
                         h4("ROC Curve"),plotOutput("roc")),
                #tabPanel("Residuals Plot",
                      #   h4("Fitted Values vs Residuals - Input Data"), plotOutput("resplot2"),
                      #   h4("Fitted Values vs Y - Input Data"), plotOutput("resplot3")),
                tabPanel("Prediction New Data",br(),
                         h4("First 10 rows of predictions for new data (upload prediction data)"),
                         p('"Y.Prob" column is the predicted probability of Y=1.'),
                         verbatimTextOutput('prediction'),
                         h4("Download new data with predictions"),
                         downloadButton('downloadData1', 'Download predictions (works only in browser)')      ) 
                )
      ) 
    ) 
  )
