#################################################
#      Summary & Basic Stats                      #
#################################################
if(!require("shiny")) {install.packages("shiny")}
if(!require("pastecs")){install.packages("pastecs")}
if(!require("RColorBrewer")){install.packages("RColorBrewer")}
if(!require("Hmisc")){install.packages("Hmisc")}
if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("reshape2")){install.packages("reshape2")}
if (!require("corrplot")) {install.packages("corrplot")}
if (!require("PerformanceAnalytics")) {install.packages("PerformanceAnalytics")}
if (!require("EnvStats")) {install.packages("EnvStats")}
if (!require("fastDummies")) {install.packages("fastDummies")}

library(shiny)
library(pastecs)
library(RColorBrewer)
library(Hmisc)
library(ggplot2)
library(reshape2)
library(corrplot)
library(PerformanceAnalytics)
library(EnvStats)
library(fastDummies)

# library(gplot)

shinyServer(function(input, output,session) {
  
Dataset <- reactive({
  if (is.null(input$file)) { return(NULL) }
  else{
    Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
    return(Dataset)
  }
})

output$xvarselect <- renderUI({
  if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
  if (is.null(input$file)) {return(NULL)}
  else {
  checkboxGroupInput("xAttr", "Select variables",
                     colnames(Dataset()), colnames(Dataset()))
  }
})

Dataset.temp = reactive({
  mydata = Dataset()[,c(input$xAttr)]
})

nu.Dataset = reactive({
  data = Dataset.temp()
  Class = NULL
  for (i in 1:ncol(data)){
    c1 = class(data[,i])
    Class = c(Class, c1)
  }
  nu = which(Class %in% c("numeric","integer"))
  nu.data = data[,nu] 
  return(nu.data)
})

output$fxvarselect <- renderUI({
  if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
  if (is.null(input$file)) {return(NULL)}
  else {
  checkboxGroupInput("fxAttr", "Select factor (categorical) variables",
                    #colnames(Dataset.temp()) )
                    colnames(Dataset.temp()),setdiff(colnames(Dataset.temp()),c(colnames(nu.Dataset()))) )
  }
})

mydata = reactive({
  mydata = Dataset()[,c(input$xAttr)]

  if (length(input$fxAttr) >= 1){
  for (j in 1:length(input$fxAttr)){
      mydata[,input$fxAttr[j]] = factor(mydata[,input$fxAttr[j]])
  }
  }
  return(mydata)
  
})


out = reactive({
data = mydata()
Missing=data[!complete.cases(data),]
Dimensions = dim(data)
Head = head(data)
Tail = tail(data)
Class = NULL
for (i in 1:ncol(data)){
  c1 = class(data[,i])
  Class = c(Class, c1)
}

nu = which(Class %in% c("numeric","integer"))
fa = which(Class %in% c("factor","character"))
nu.data = data[,nu] 
fa.data = data[,fa] 
Summary = list(Numeric.data = round(stat.desc(nu.data)[c(4,5,6,8,9,12,13),] ,4), factor.data = describe(fa.data))
# Summary = list(Numeric.data = round(stat.desc(nu.data)[c(4,5,6,8,9,12,13),] ,4), factor.data = describe(fa.data))

a = seq(from = 0, to=200,by = 4)
j = length(which(a < ncol(nu.data)))
out = list(Dimensions = Dimensions,Summary =Summary ,Tail=Tail,fa.data,nu.data,a,j, Head=Head,MissingDataRows=Missing)
return(out)
})

output$head = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    out()[8]
  }
})

output$tail = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    out()[3]
  }
})

output$missing = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    out()[9]
  }
})

output$summary = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    out()[1:2]
      }
})

# Select variables:
output$outselect <- renderUI({
  if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
  if (is.null(input$file)) {return(NULL)}
  else {
  selectInput("rAttr", "Select variable for Rosner's outlier test",
              colnames(out()[[5]]), colnames(out()[[5]])[1])
  }
})

output$hist = renderPlot({
  if (is.null(input$file)) {return(NULL)}
  else {
    hist(Dataset()[,input$rAttr])
  }
})

output$outlier = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    rosnerTest(Dataset()[,input$rAttr])
  }
})


output$heatmap = renderPlot({ 
    qplot(x=Var1, y=Var2, data=melt(cor(out()[[5]], use = "pairwise.complete.obs")), fill=value, geom="tile") +
    scale_fill_gradient2(limits=c(-1, 1))
})

plotsample =  reactive({
  sizedata= nrow(mydata())
  smp=(
    if (sizedata>500) {500} 
    else {
      sizedata
          }
      )
  sample( 1:sizedata, smp )
})

plot_data = reactive({
  my_data = out()[[5]]
  my_data[plotsample(),]
})


output$heatmap1 = renderPlot({ 
  if (is.null(input$file)) {return(NULL)}
  else {
  chart.Correlation(plot_data(),hitogram=TRUE)
  }
})

output$bplot = renderPlot({ 
  if (is.null(input$file)) {return(NULL)}
  else {
    boxplot(out()[[5]])
  }
})

output$correlation = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
  round(cor(out()[[5]], use = "pairwise.complete.obs"),2)
  }
  })

output$corplot = renderPlot({
  if (is.null(input$file)) {return(NULL)}
  else {
  my_data = out()[[5]]
  cor.mat <- round(cor(my_data),2)
  corrplot(cor.mat, 
           type = "upper",    # upper triangular form
           order = "hclust",  # ordered by hclust groups
           tl.col = "black",  # text label color
           tl.srt = 45) 
  }
  
})


output$downloadDatanew <- downloadHandler(
  filename = function() { "califhouse.csv" },
  content = function(file) {
        write.csv(dummy_cols(mydata(), remove_first_dummy=TRUE), file, row.names=F, col.names=F)
  }
)

output$downloadData <- downloadHandler(
  filename = function() { "califhouse.csv" },
  content = function(file) {
    write.csv(read.csv("data/califhouse.csv"), file, row.names=F, col.names=F)
  }
)


})

