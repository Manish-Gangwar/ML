tabPanel('EDA', value = 'tab_eda', icon = icon('stats', lib = 'glyphicon'),
         
         navlistPanel(id = 'navlist_eda',
                      well = FALSE,
                      widths = c(2, 10),
                      
                      tabPanel('Screen', value = 'tab_screen',
                               
                               fluidPage(
                                 fluidRow(
                                   column(12, align = 'left',
                                          h4('Data Screening'),
                                          p('Screen data for missing values, verify column names and data types.')
                                   )
                                 ),
                                 hr(),
                                 fluidRow(
                                   column(12,align="left",verbatimTextOutput("dim"))
                                 ),
                                 fluidRow(
                                   br(),
                                   br(),
                                   column(12, align = 'center',
                                          verbatimTextOutput('screen_summary')
                                   )
                                 )
                               )
                      ),
                      
                      tabPanel('Summary', value = 'tab_summary',
                               
                               fluidPage(
                                 
                                 fluidRow(
                                   column(12, align = 'left',
                                          h4('Summary Statistics'),
                                          p('Generate descriptive statistics for continuous data.')
                                   )
                                 ),
                                 hr(),
                                 fluidRow(
                                   column(2, align = 'right',
                                          br(),
                                          br(),
                                          h5('Variable:')
                                          
                                   ),
                                   column(8, align = 'left',
                                          br(),
                                          uiOutput('num_var_for_summary')
                                          
                                   ),
                                   column(2, align = 'left',
                                          br(),
                                          br(),
                                          actionButton(inputId = 'submit_summary', label = 'Submit', width = '120px', icon = icon('check')),
                                          bsTooltip("submit_summary", "Click here to view summary statistics.",
                                                    "bottom", options = list(container = "body"))
                                   )
                                 ),
                                 
                                 fluidRow(
                                   br(),
                                   br(),
                                   column(12, align = 'center',
                                          verbatimTextOutput('num_summary')
                                   )
                                 )
                               )
                      ),
                      
                      tabPanel("Frequency-Qualitative",value = "freq_qual",
                               fluidPage(
                                 
                                 fluidRow(
                                   column(6, align = 'left',
                                          h4('Frequency Table (Qualitative Data)'),
                                          p('Generates frequency table for factor data and returns the frequency, cumulative frequency, 
                                                                          frequency percent, cumulative frequency percent and a bar plot.')
                                   ),
                                 ),
                                 hr(),
                                 
                                 fluidRow(
                                   column(2, align = 'right',
                                          br(),
                                          br(),
                                          h5('Variable:')
                                          
                                   ),
                                   
                                   column(6, align = 'left',
                                          
                                          br(),
                                          uiOutput('cat_var_for_freq')
                                          
                                   ),
                                   
                                   column(3, align = 'left',
                                          br(),
                                          br(),
                                          actionButton(inputId = 'submit_fqual', label = 'Submit', width = '120px', icon = icon('check')),
                                          bsTooltip("submit_fqual", "Click here to view frequency table.",
                                                    "bottom", options = list(container = "body"))
                                          
                                   )
                                   
                                 ),
                                 
                                 fluidRow(
                                   
                                   column(12, align = 'center',
                                          
                                          br(),
                                          br(),
                                          uiOutput('freq1_title'),
                                          # h3('Frequency Table'),
                                          br(),
                                          verbatimTextOutput('freq_qual'),
                                          br(),
                                          plotOutput('qual_vert', height = "500px", width = "75%"),
                                          br(),
                                          plotOutput('qual_horiz')
                                          
                                          
                                   )
                                   
                                 )
                               )
                      ),
                      tabPanel('Frequency-Quantitative', value = 'tab_fquant',
                               
                               fluidPage(
                                 
                                 fluidRow(
                                   column(6, align = 'left',
                                          h4('Frequency Table (Quantitative Data)'),
                                          p('Generates the frequency distribution of continuous data by splitting
                                                                          the data into equidistant intervals created based on the number of bins specified.')
                                   ),
                                   
                                 ),
                                 hr(),
                                 
                                 fluidRow(
                                   
                                   column(1, align = 'right', br(), h5('Variable:')),
                                   
                                   column(3, align = 'left',
                                          
                                          uiOutput('cont_var_for_freq')
                                          
                                   ),
                                   
                                   #column(1, align = 'right', br(), h5('Filter:')),
                                   
                                   # column(3, align = 'left',
                                   # sliderInput(inputId = 'filter_quant',  width = '250px',
                                   #             label = '',
                                   #             min = 0, max = 100,
                                   #             step = 1, value = c(20, 80)
                                   # ),
                                   # bsTooltip("filter_quant", "Filter data.",
                                   #           "bottom", options = list(container = "body"))
                                   # ),
                                   
                                   column(1, align = 'right', br(), h5('Bins:')),
                                   
                                   column(3, align = 'left',
                                          
                                          numericInput('bins', label = '', width = '200px',
                                                       min = 1, value = 5),
                                          bsTooltip("bins", "Specify the number of bins.",
                                                    "bottom", options = list(container = "body"))
                                          
                                          
                                   ),
                                   column(4, align = 'left',
                                          
                                          br(),
                                          actionButton(inputId = 'submit_fquant', label = 'Submit', width = '180px', icon = icon('check')),
                                          bsTooltip("submit_fquant", "Click here to view frequency table.",
                                                    "bottom", options = list(container = "body"))
                                          
                                          
                                   )
                                   
                                 ),
                                 
                                 # fluidRow(
                                 #   
                                 #   column(12, align = 'center',
                                 #          
                                 #          br(),
                                 #          br(),
                                 #          
                                 #          actionButton(inputId = 'submit_fquant', label = 'Submit', width = '180px', icon = icon('check')),
                                 #          bsTooltip("submit_fquant", "Click here to view frequency table.",
                                 #                    "bottom", options = list(container = "body"))
                                 #          
                                 #   )
                                 # ),
                                 
                                 fluidRow(
                                   
                                   br(),
                                   
                                   column(12, align = 'center',
                                          uiOutput('freq2_title'),
                                          verbatimTextOutput('freq_quant')
                                          
                                   )
                                   
                                 ),
                                 
                                 fluidRow(
                                   
                                   column(12, align = 'center',
                                          
                                          # h3('Histogram'),
                                          plotOutput('hist_freq_quant', height = "500px", width = "75%")
                                   )
                                 )
                                 
                               )
                               
                      ),
                      
                      tabPanel("Correlation",value="corr_plot",
                               
                               fluidPage(
                                 fluidRow(
                                   column(6, align = 'left',
                                          h4('Correlation Plot'),
                                          p('Generates the correlation plot between all numeric variables.'),
                                          
                                   ),
                                   
                                   
                                 ),
                                 hr(),
                                 
                                 fluidRow(
                                   plotOutput("corr_plot"),
                                   helpText("Note: Rows with missing values are not considered")
                                 )
                                 
                               )
                              
         )
         )
) # end of eda tab panel