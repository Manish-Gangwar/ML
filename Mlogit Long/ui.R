####################################################
#                ISB mlogit                        #
####################################################

library("shiny")
#library("foreign")
#library(shinydashboard)
#library(leaflet)



shinyUI(pageWithSidebar(
  # Header:
  
  headerPanel(title=div(img(src="isb.png",  height = 50, width = 100,align = "right"),
                                      h3("Multinomial Logit App", style="bold")
  )),
  
  # Input in sidepanel:
  sidebarPanel(
    h5(p("Data Input")),
    fileInput("file", "Upload input data (csv file with header)"),
    #fileInput("filep", "Upload prediction data (csv file with header)"),
    h5(p("Data Selection")),
    htmlOutput("Choicevarselect"),
    htmlOutput("Individualvarselect"),
    htmlOutput("Alternativesvarselect"),
    htmlOutput("Alternativefeaturesvarselect"),
    htmlOutput("Individualfeaturesvarselect"),
    
    br()
  ),
  # Main:
  mainPanel( 
    
    tabsetPanel(type = "tabs",
                #
                
                tabPanel("Overview",
                         h4(p("Data Description")),
                         p("The data, collected as part of a 1987 intercity mode choice study, are a sub-sample of 210 non-business trips between Sydney, Canberra and Melbourne in which the traveler chooses a mode from four alternatives (plane, car, bus and train). The sample, 840 observations, is choice based with over-sampling of the less popular modes (plane, train and bus) and under-sampling of the more popular mode, car. The level of service data was derived from highway and transport networks in Sydney, Melbourne, non-metropolitan N.S.W. and Victoria, including the Australian Capital Territory.",align="justify"),
                           h4(p("Download Sample Input Files")),
                           downloadButton('downloadData', 'Download model training input file (works only in browsers)'),
                           br(),br(),
                         p("Number of observations: 840 Observations on 4 Modes of transporataion, for 210 Individuals.",align="justify"),
                         h5(p("Number of variables: 8",align="justify")),
                           h5('individual = 1 to 210'),
                           h5('choice     = 0 - no 1 - yes'),
                           h5('mode     =  available alternatives'),
                           h5('wait       = terminal waiting time for plane, train and bus (minutes); 0 for car.'),
                           h5('vcost      = in vehicle cost for all stages (dollars).'),
                           h5('gcost      = generalized cost measure:invc+(invt*value of travel time savings)(dollars).'),
                           h5('icome      = household income'),
                           h5('size       = traveling group size in mode chosen (number).'),
                #          h5('lite_beer       =    Whether low calorie beer'),
                #          h5('regular_beer    =    Whether regular beer'),
                #          h5('ad_spend    =    ad spend in $000'),
                #          h5('price_per_oz    =    Price in $ per oz'),
                #          h5('beer_distbn    =    Weighted distribution'),
                #          h5('promo    =    Weighted Promotional activities'),
                #          h5('beer_sales_vol    =    volume sold in that week'),
                #          h5('month    =    month number 1 to 12'),
                #          ))
                          br(),
                          p("Please note that download will not work with RStudio interface. 
                            Download will work only in web-browsers. So open this app in a web-browser and then download the example file. 
                            For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
                          img(src = "example1.png")), #, height = 280, width = 400
                # 
                # tabPanel("Summary Stats", verbatimTextOutput("summary")),
                # tabPanel("Correlation", verbatimTextOutput("correlation"),plotOutput("heatmap")),
                 tabPanel("Summary Stats",h4("Data"), verbatimTextOutput("head"),verbatimTextOutput("tail"),
                         h4("Data Summary"),verbatimTextOutput("summary"),h4("Missing Data Rows"),verbatimTextOutput("missing")),
                tabPanel("Model Output", h4("Model Summary"),verbatimTextOutput("olssummary")),
              #  tabPanel("Correlation",h4("Correlation Table"), verbatimTextOutput("correlation"),h4("Correlation"),plotOutput("corplot")),
                 tabPanel("Prediction Probablities", 
                          h4(p("Download Output Probabilities")),
                          downloadButton('downloadData1', 'Download model training input file (works only in browsers)'),
                          br(),br(),
                          h4("Probablities"),verbatimTextOutput("probablities")),
                 tabPanel("Confusion Matrix", h4("Confusion Matrix Summary"),verbatimTextOutput("confusionmatrix"))
              #   tabPanel("ROC Curve", h4("ROC Curve Summary"),plotOutput("ROC"))
                 
                #          h4("Summary OLS standardized model"),
                #          verbatimTextOutput("olssummarystd")),
                # tabPanel("Residuals Plot",h4("Fitted Values vs Residuals"),
                #          plotOutput("resplot2"),h4("Fitted Values vs Y"),
                #          plotOutput("resplot3"),h4("Residuals plot"),
                #          plotOutput("resplot1")),
                # tabPanel("Data with predicted Y",tableOutput("datatable")),
                # tabPanel("Prediction",br(),
                #          h4("First 10 rows of predicted data"),
                #          p('"Yhat" column is the predicted value.'),
                #          verbatimTextOutput('prediction'),
                #          h4("Download Predicted data"),
                #          downloadButton('downloadData1', 'Download Predicted data (Works only in browser)')
                # )                
                
    )#type tabs
  ) #main panel
) #page with side bar
)#ui