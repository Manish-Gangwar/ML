####################################################
#           Image to TeXt                          #
####################################################
try(require("devtools")||install.packages("devtools"))
library("devtools")

try(require("shiny")||install.packages("shiny"))
try(require("tesseract")||install.packages("tesseract"))
try(require("magick")||install.packages("magick"))
try(require("magrittr")||install.packages("magrittr"))

library("shiny")
library("tesseract")
library("magick")
library("magrittr")
#library("foreign")

shinyUI(pageWithSidebar(
  # Header:
  headerPanel("Image to Text App"),
  # Input in sidepanel:
  sidebarPanel(
    
    h5(p("Data Input")),
    fileInput("file", "Upload Image file of Scanned Document)")
  ),
  # Main:
  mainPanel( 
    
    tabsetPanel(type = "tabs",
                #
                tabPanel("Overview",br(),h4("How to use this app"),br(),
                         p('To use this app, simply upload the image file in the left side bar panel. Once image file is uploaded, text from the image will show in "Image to Text" tab. One Sample image file is shown below.'),
                         
                         img(src = 'Scanned_Image_of_Text.jpg')),
                tabPanel("Image to Text",br(),br(),
                         downloadButton('downloadData', 'Download Converted Text File'), br(),br(),
                         p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
                         img(src = "example1.png"),
                         verbatimTextOutput("text"))
                    )
  ) 
) 
)

