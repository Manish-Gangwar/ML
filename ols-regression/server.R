#################################################
#      Summary & OLS App                      #
#################################################
if(!require("shiny")) {install.packages("shiny")}
if(!require("pastecs")){install.packages("pastecs")}
if(!require("RColorBrewer")){install.packages("RColorBrewer")}
if(!require("Hmisc")){install.packages("Hmisc")}
if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("reshape2")){install.packages("reshape2")}
if (!require("corrplot")) {install.packages("corrplot")}
if (!require("PerformanceAnalytics")) {install.packages("PerformanceAnalytics")}

library(shiny)
library(pastecs)
library(RColorBrewer)
library(Hmisc)
library(ggplot2)
library(reshape2)
library(corrplot)
library(PerformanceAnalytics)

# library(gplot)

shinyServer(function(input, output,session) {
  
Dataset <- reactive({
  if (is.null(input$file)) { return(NULL) }
  else{
    Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
    return(Dataset)
  }
})


pred.readdata <- reactive({
  if (is.null(input$filep)) { return(NULL) }
  else{
    readdata <- as.data.frame(read.csv(input$filep$datapath ,header=TRUE, sep = ","))
    return(readdata)
  }
})

# Select variables:
output$yvarselect <- renderUI({
  if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
  
  selectInput("yAttr", "Select Y variable",
                     colnames(Dataset()), colnames(Dataset())[1])
  
})

output$xvarselect <- renderUI({
  if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
  
  checkboxGroupInput("xAttr", "Select X variables",
                     setdiff(colnames(Dataset()),input$yAttr), setdiff(colnames(Dataset()),input$yAttr))
  
})

Dataset.temp = reactive({
  mydata = Dataset()[,c(input$yAttr,input$xAttr)]
})

output$fxvarselect <- renderUI({
  if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
  
  checkboxGroupInput("fxAttr", "Factor variable in X",
                     setdiff(colnames(Dataset.temp()),input$yAttr),"" )
  
})

mydata = reactive({
  mydata = Dataset()[,c(input$yAttr,input$xAttr)]

  if (length(input$fxAttr) >= 1){
  for (j in 1:length(input$fxAttr)){
      mydata[,input$fxAttr[j]] = factor(mydata[,input$fxAttr[j]])
  }
  }
  return(mydata)
  
})


Dataset.Predict <- reactive({
  fxc = setdiff(input$fxAttr, input$yAttr)
  mydata = pred.readdata()[,c(input$xAttr)]
  
  if (length(fxc) >= 1){
    for (j in 1:length(fxc)){
      mydata[,fxc[j]] = as.factor(mydata[,fxc[j]])
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



# output$scatterplots <- renderUI({
#   if (is.null(input$file)) {return(NULL)}
#   else {
#     
#     plot_output_list <- lapply(1:out()[[7]], function(i) {
#       plotname <- paste("plot", i, sep="")
#       plotOutput(plotname, height = 700, width = 700)
#     })
#     # Convert the list to a tagList - this is necessary for the list of items
#     # to display properly.
#     do.call(tagList, plot_output_list)
#   }
# })
# 
# # Call renderPlot for each one. Plots are only actually generated when they
# # are visible on the web page.
# max_plots = 50
# 
# for (i in 1:max_plots) {
#   # Need local so that each item gets its own number. Without it, the value
#   # of i in the renderPlot() will be the same across all instances, because
#   # of when the expression is evaluated.
#   local({
#     
#     my_i <- i 
#     plotname <- paste("plot", my_i, sep="")
#     
#     output[[plotname]] <- renderPlot({
#       out1 = out()
#       a = out1[[6]]
#       j = my_i
#       if (ncol(out1[[5]]) == a[j] + 1){
#         a1 = a[j]+1
#         a2 = a[j]-1
#         dai = out1[[5]][,a1:a2]
#         plot(dai)
#         }
#       
#       else if ( ncol(out1[[5]]) < a[j + 1]){
#         a1 = a[j]+1
#         a2 = ncol(out1[[5]])
#         dai = out1[[5]][,a1:a2]
#         plot(dai)
#       }
#       
#       else if(ncol(out1[[5]]) > a[j + 1]){
#         a1 = a[j]+1
#         a2 = a[j + 1]
#         dai = out1[[5]][,a1:a2]
#         plot(dai)
#       }
#       
#       mtext(paste("Scater plot " ,my_i), side = 3, line = 2, cex=2)
#         })
#   })
# }

output$heatmap = renderPlot({ 
    qplot(x=Var1, y=Var2, data=melt(cor(out()[[5]], use = "pairwise.complete.obs")), fill=value, geom="tile") +
    scale_fill_gradient2(limits=c(-1, 1))
})

output$heatmap1 = renderPlot({ 
  my_data = out()[[5]]
  chart.Correlation(my_data,hitogram=TRUE)
})

output$correlation = renderPrint({
  cor(out()[[5]], use = "pairwise.complete.obs")
  })

output$corplot = renderPlot({
  my_data = out()[[5]]
  cor.mat <- round(cor(my_data),2)
  corrplot(cor.mat, 
           type = "upper",    # upper triangular form
           order = "hclust",  # ordered by hclust groups
           tl.col = "black",  # text label color
           tl.srt = 45)  
  
})

ols = reactive({
    rhs = paste(input$xAttr, collapse = "+")
    ols = lm(paste(input$yAttr,"~", rhs , sep=""), data = mydata())
  return(ols)
})

ols2 = reactive({
  
  drop = which(input$yAttr == colnames(out()[[5]]))
               
  x0 = out()[[5]][,-drop]
  x01 = scale(x0, center = T, scale = T)
  
  y = out()[[5]][,drop]
  
  dstd = data.frame(y,x01)
  colnames(dstd) = c(input$yAttr,colnames(x01))
  
  if (ncol(data.frame(out()[[4]])) == 1) {
    fdata = data.frame(out()[[4]])
    colnames(fdata) = input$fxAttr
    dstd = data.frame(dstd,fdata)
  }
  
  else if (ncol(data.frame(out()[[4]])) > 1) {
    fdata = data.frame(out()[[4]])
    dstd = data.frame(dstd,fdata)
  }
  
  rhs = paste(input$xAttr, collapse = "+")
  ols = lm(paste(input$yAttr,"~", rhs , sep=""), data = dstd)
  return(ols)

  })

output$resplot1 = renderPlot({
  plot(ols()$residuals)
})

output$resplot2 = renderPlot({
  plot(ols()$residuals,ols()$fitted.values)
})

output$resplot3 = renderPlot({
  plot(mydata()[,input$yAttr],ols()$fitted.values)#
})


output$olssummary = renderPrint({
  summary(ols())
  })

output$olssummarystd = renderPrint({
  summary(ols2())
})

output$datatable = renderTable({
  Y.hat = ols()$fitted.values
  data.frame(Y.hat,mydata())
})


prediction = reactive({
  val = predict(ols(),Dataset.Predict())
  out = data.frame(Yhat = val, pred.readdata())
})

output$prediction =  renderPrint({
  if (is.null(input$filep)) {return(NULL)}
  head(prediction(),10)
})

#------------------------------------------------#
output$downloadData1 <- downloadHandler(
  filename = function() { "Predicted Data.csv" },
  content = function(file) {
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    write.csv(prediction(), file, row.names=F, col.names=F)
  }
)
output$downloadData <- downloadHandler(
  filename = function() { "beer data.csv" },
  content = function(file) {
    write.csv(read.csv("data/beer data.csv"), file, row.names=F, col.names=F)
  }
)

output$downloadData2 <- downloadHandler(
  filename = function() { "beer data - prediction sample.csv" },
  content = function(file) {
    write.csv(read.csv("data/beer data - prediction sample.csv"), file, row.names=F, col.names=F)
  }
)

})

