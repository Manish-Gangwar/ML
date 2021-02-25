# 06-navlist.R

library(shiny)
library(shinythemes)
library(dplyr)
library(fastDummies)
library(Hmisc)
library(shinyWidgets)
library(shinyhelper)
library(VIM)
library(summarytools)
library(descriptr)
library(DataExplorer)
library(shinyBS)
library(stringr)

useSweetAlert()

ui <- fluidPage(theme = shinytheme("cerulean"),
                tagList(
                  tags$head(tags$script(type="text/javascript", src = "code.js")),
                  navbarPage(title="Basic Data Prepration and Analysis",
                             tabPanel(title = "Upload Data",icon = icon('upload', lib = 'glyphicon'),
                                      #titlePanel("Upload data for imputation"),
                                      
                                      
                                      #---code for example datasets will come
                                      
                                      fluidRow(
                                        hr(),
                                        
                                        column(3,
                                               h3("Load Example Datasets"),
                                               uiOutput("data_helper"),
                                               selectInput("ex_data",
                                                           label = "",
                                                           choices=c("sleep","diabetes","mtcars"),
                                                           multiple = FALSE,
                                                           selectize = TRUE,
                                                           selected=sleep),
                                               
                                               
                                               
                                               )
                                        
                                        #actionButton("load_dataset", "Load",icon = )

                                      ),
                                      
                                      
                                      
                                      fluidRow
                                      (
                                        
                                        hr(),
                                        h3("Upload Data"),
                                        column(5,
                                               fileInput('file',
                                                         label = 'Choose File',
                                                         placeholder = 'No File Selected')
                                        ),
                                        column(3,
                                               radioButtons("sep", 
                                                            label = "Seperator",
                                                            choices = list("Comma" = ',', "Semicolon" = ';', "Tab" = '\t',"Space"=' '), 
                                                            selected = ",")
                                        ),
                                        # column(2,radioButtons("quote", label = "Quote",
                                        #                       choices = list("None" = 1, "Double Quote" = 2, "Single Quote" = 3), 
                                        #                       selected = 1)
                                        #        ),
                                        column(3,radioButtons("na_str", label = "Select NA Values in Dataset",
                                                              choices = list("." = ".", "NA" = "NA","Other"="other"),
                                                              selected = "NA"),
                                               uiOutput("text")
                                        ),
                                        
                                        column(1,checkboxInput(inputId = "header",
                                                               label = "Header",value = TRUE
                                        )
                                        )
                                      ),#end of fluid row-1
                                      hr(),
                                      fluidRow(
                                        h3("Review Data"),
                                        dataTableOutput('head')
                                      )
                                      
                             ),# end of tabpanel-1
                             
                             source('ui/ui_dummy_en_EDA.R', local = TRUE)[[1]],
                             
                             tabPanel(title="Non-Metric Detection & Conversion",
                                      sidebarLayout(
                                        sidebarPanel(
                                          h4("Select columns to convert"),
                                          uiOutput('vars2conv_fact'),
                                          hr(),
                                          actionButton("conv", "Convert"),
                                        ),
                                        mainPanel(
                                          h4("Uploaded data structure"),
                                          dataTableOutput('df_str'),
                                          h4("Data structure after conversion"),
                                          dataTableOutput("df_conv_str")
                                        )
                                          
                                        )
                                      ),
                             
                             
                             tabPanel(title = "Missing Value Imputation",
                                      #sidebarLayout(
                                        sidebarPanel(
                                          conditionalPanel(condition="input.tabselected==1",
                                                          # actionButton('bt','button Tab 1')
                                          ),
                                          
                                          conditionalPanel(condition="input.tabselected==2",
                                                           h4("Continuous Variable"),
                                                           uiOutput('intvars2imp'),
                                                           selectInput("selIntImpMethod",
                                                                       label = "Imputation Method",
                                                                       choices=c(kNN ="knn",
                                                                                  mean="mean",
                                                                                 complete_case = "complete_case",
                                                                                 median = "median"),
                                                                       multiple = FALSE,
                                                                       selectize = TRUE,
                                                                       selected=NULL),
                                                           uiOutput("knn_parms"),
                                                           hr(),
                                                           h4("Categorical Variable"),
                                                           uiOutput('catvars2imp'),
                                                           selectInput("selCatImpMethod",
                                                                       label = "Imputation Method",
                                                                       choices=c(mode="mode"),
                                                                       multiple = FALSE,
                                                                       selectize = TRUE,
                                                                       selected=NULL),
                                                           checkboxInput("orig_col","Keep Original Columns",value = TRUE),
                                                           actionButton("imp", "Impute"),
                                                           downloadButton("imp_download")
                                                           
                                          )
                                          
                                      
                                        
                                      ),
                                      mainPanel(
                                        tabsetPanel(type = "tabs",
                                                    tabPanel("Missing Value Stats",value = 1,
                                                      h4("Missing Value Distribution"),
                                                      plotOutput("missing_plot")
                                                    ),
                                                    tabPanel("Imputation",value = 2,
                                                      h4("Stats after imputation"),
                                                      dataTableOutput("int_replacement"),
                                                      hr(),
                                                      h4("sample dataset after imputation"),
                                                      dataTableOutput("int_imp"),
                                                      #h4("Missing Values Imputed for categorical variables"),
                                                      #dataTableOutput("cat_imp")
                                                    ),id = "tabselected"
                                                    
                                        )
                                        
                                        
                                        
                                      )
                                      
                                     # )
                             ),
                             
                             tabPanel(title="Data Transformation",icon = icon('transfer', lib = 'glyphicon'),
                                      sidebarLayout(
                                        sidebarPanel(
                                          h4("select columns for Standardization"),
                                          uiOutput('std_vars'),
                                          hr(),
                                          
                                          radioButtons("method",
                                                             label = "select transformation method",
                                                             choices = c("Normalization  (minmax)"='minmax',
                                                                         "Standardization (mean=0,sd=1)" = 'standard',
                                                                         "Robust Scaling" ="robust",
                                                                         "None" = "none"
                                                                         )
                                                              ),
                                          #%>%
                                          # helper(type = "inline",
                                          #        title = "Inline Help",
                                          #        content = c("This helpfile is defined entirely in the UI!",
                                          #                    "This is on a new line.",
                                          #                    "This is some <b>HTML</b>."),
                                          #        size = "s"),
                                          checkboxInput("orig_col1","Keep Original Columns",value = TRUE),
                                          actionButton("transform","Transform"),
                                          downloadButton("trans_download")
                                        ),
                                        mainPanel(
                                          h4("Transformed Data"),
                                          dataTableOutput("trans_df")
                                        )
                                      )),
                             
                             
                             tabPanel(title = "Dummy Encoding",
                                      sidebarLayout(
                                        sidebarPanel(
                                            h4("Select columns to encode"),
                                            uiOutput('vars2conv'),
                                          hr(),
                                          checkboxInput("rem_org",
                                                        label = "Remove original columns",
                                                        value = TRUE
                                          ),
                                          hr(),
                                          helpText("Removes the first dummy and keep only n-1 dummies."),
                                          checkboxInput("rem_first_dum","Remove first dummy",
                                                        value = FALSE),
                                          hr(),
                                          helpText("Create new column for missing value or ignore"),
                                          checkboxInput("ign_NA","Ignore NA",
                                                        value = TRUE),
                                          hr(),
                                          actionButton("conv2", "Convert",icon = ),
                                          downloadButton("dummy_download")
                                        ),
                                        
                                        mainPanel(
                                          #h3("Uploaded Data Structure"),
                                          
                                          hr(),
                                          h3("Review converted dummy data"),
                                          
                                          dataTableOutput('dummy_table')
                                        )
                                      )
                             )
                             
                             
                                                  #source('ui/ui_dummy_en_EDA.R', local = TRUE)[[1]]
                                                  # source('ui/ui_freq_qual.R', local = TRUE)[[1]],
                                                  # source('ui/ui_freq_quant.R', local = TRUE)[[1]],
                                                  # source('ui/ui_cross.R', local = TRUE)[[1]],
                                                  # source('ui/ui_mult1.R', local = TRUE)[[1]],
                                                  # source('ui/ui_mult2.R', local = TRUE)[[1]],
                                                  # source('ui/ui_group_summary.R', local = TRUE)[[1]]
                                                   
                                      
                             )
                             
                  )# end of navbarpage
                  
                  
                )# end of taglist
  
#  )# end of fluidpage

  
  
  
  
  
