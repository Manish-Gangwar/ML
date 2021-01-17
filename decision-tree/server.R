###########################################################
#  Classification and Regression Tree App (server)        #
###########################################################
suppressPackageStartupMessages({
  try(require("shiny")||install.packages("shiny"))
  try(require("pastecs")||install.packages("pastecs"))
  try(require("rpart")||install.packages("rpart"))
  try(require("rpart.plot")||install.packages("rpart.plot"))
  try(require("dplyr")||install.packages("dplyr"))
  try(require("Hmisc")||install.packages("Hmisc"))
  try(require("hydroGOF")||install.packages("hydroGOF"))
  try(require("party")||install.packages("party"))
  try(require("partykit")||install.packages("partykit"))
})


library(shiny)
library(rpart)
library(rpart.plot)
library(pastecs)
library(dplyr)
library(Hmisc)
library(hydroGOF)
require(party)
require(partykit)


shinyServer(function(input, output,session) {
  
  #------------------------------------------------#
  
  readdata <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      readdata <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
      return(readdata)
    }
  })

  # Select variables:
  output$yvarselect <- renderUI({
    if (is.null(input$file)) {return(NULL)}
    
    selectInput("yAttr", "Select Y variable (Is it is a categorical variable? mark it as factor variable)",
                colnames(readdata()), colnames(readdata())[1])
    
  })
  
  output$xvarselect <- renderUI({
    if (identical(readdata(), '') || identical(readdata(),data.frame())) return(NULL)
    
    checkboxGroupInput("xAttr", "Select X variables",
                       setdiff(colnames(readdata()),input$yAttr), setdiff(colnames(readdata()),input$yAttr))
    
  })

  readdata.temp = reactive({
    mydata = readdata()[,c(input$yAttr,input$xAttr)]
  })

    
  output$fxvarselect <- renderUI({
    if (identical(readdata.temp(), '') || identical(readdata.temp(),data.frame())) return(NULL)
    
    checkboxGroupInput("fxAttr", "Select factor (categorical) variables",
                       colnames(readdata.temp()),input$yAttr )
    
  })
  
  
  Dataset = reactive({
    mydata = readdata()[,c(input$yAttr,input$xAttr)]
    
    if (length(input$fxAttr) >= 1){
      for (j in 1:length(input$fxAttr)){
        mydata[,input$fxAttr[j]] = as.factor(mydata[,input$fxAttr[j]])
      }
    }
    return(mydata)
    
  })
  
  
  # a = c('a','b','c')
  # b = ('c')
  # setdiff(a,b)
    #------------------------------------------------#
  
  out = reactive({
    data = Dataset()
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
    Summary = list(Numeric.data = round(stat.desc(nu.data)[c(4,5,6,8,9,12,13),] ,3), factor.data = describe(fa.data))
    
    a = seq(from = 0, to=200,by = 4)
    j = length(which(a < ncol(nu.data)))
    out = list(Dimensions = Dimensions,Summary =Summary ,Tail=Tail,fa.data,nu.data,a,j, Head=Head, MissingDataRows=Missing)
    return(out)
  })
  
  output$summarydata = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {
      out()[1:2]
    }
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
  
  
  testsample =  reactive({
  set.seed(5898)
  sample(1:nrow(Dataset()), round(nrow(Dataset())*((input$sample)/100)))
         })

  train_data = reactive({
      Dataset()[-testsample(),]
  })
  
  test_data = reactive({
    Dataset()[testsample(),]
  })
  
  pred.readdata <- reactive({
    if (is.null(input$filep)) { return(NULL) }
    else{
      readdata <- as.data.frame(read.csv(input$filep$datapath ,header=TRUE, sep = ","))
      return(readdata)
    }
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
  
  output$trainobs = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {
      dim( train_data())
    }
  })
  
  output$testobs = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {
      dim( test_data())
    }
  })
  
  output$predictobs = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {
      dim(Dataset.Predict())
    }
  })
  
  #------------------------------------------------#
  fit.rt = reactive({
  if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
  x = setdiff(colnames(Dataset()), input$Attr)
  y = input$yAttr
  # formula1 = 
  ## mean predictions
  
  if (class(train_data()[,c(input$yAttr)]) == "factor"){
  fit.rt <- rpart(as.formula(paste(y, paste( x, collapse = ' + '), sep=" ~ ")),
                  cp = input$cp,
                  method="class",   # use "class" for classification trees
                data=train_data())
  pr <- as.party(fit.rt)    # thus, we use same object 'rp' from the raprt package
  val1 = predict(pr, newdata = test_data(),type="response")
  val = predict(pr, newdata = train_data(),type="response")
  imp = round(fit.rt$variable.importance/sum(fit.rt$variable.importance),2)
  
  } else {
  fit.rt <- rpart(as.formula(paste(y, paste( x, collapse = ' + '), sep=" ~ ")),
                  cp = input$cp,
                  method="anova",   # use "class" for classification trees
                  data=train_data())
  pr <- as.party(fit.rt)    # thus, we use same object 'rp' from the raprt package
   val1 = predict(pr, newdata = test_data())
   val = predict(pr, newdata = train_data())
   imp = round(fit.rt$variable.importance/sum(fit.rt$variable.importance),2)
  }
  
  out = list(model = fit.rt, validation = val, imp = imp, validation1=val1)
    })
  
#-------------------------------------
  
  prediction = reactive({
    if (class(train_data()[,c(input$yAttr)]) == "factor"){
      
      fit.rt <- fit.rt()$model
      pr <- as.party(fit.rt)    # thus, we use same object 'rp' from the raprt package
      val3 = predict(pr, newdata = Dataset.Predict(),type="response")
      
    } 
    else {
      fit.rt <- fit.rt()$model
      pr <- as.party(fit.rt)    # thus, we use same object 'rp' from the raprt package
      val3 = predict(pr, newdata = Dataset.Predict())
      
    }
    
    out = data.frame(Yhat = val3, pred.readdata())
    return(out)    
    
  })
  
  predictionorg = reactive({
    if (class(train_data()[,c(input$yAttr)]) == "factor"){
      
      fit.rt <- fit.rt()$model
      pr <- as.party(fit.rt)    # thus, we use same object 'rp' from the raprt package
      val3 = predict(pr, newdata = Dataset(),type="response")
      
    } 
    else {
      fit.rt <- fit.rt()$model
      pr <- as.party(fit.rt)    # thus, we use same object 'rp' from the raprt package
      val3 = predict(pr, newdata = Dataset())
      
    }
    
    out = data.frame(Yhat = val3, readdata())
    return(out)    
    
  })
#---------------------------------------------------------------  
  

  output$validation1 = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    
    if (class(test_data()[,c(input$yAttr)]) == "factor"){
      y = test_data()[,input$yAttr]
      yhat = fit.rt()$validation1
    confusion_matrix = table(y,yhat)
    accuracy = (sum(diag(confusion_matrix))/sum(confusion_matrix))
    out = list(Confusion_matrix_of_Validation = confusion_matrix, Accuracy_of_Validation = accuracy)
    } else {
    dft = data.frame(scale(data.frame(y = test_data()[,input$yAttr], yhat = fit.rt()$validation1)))
    mse.y = mse(dft$y,dft$yhat)
    out = list(Mean_Square_Error_of_Standardized_Response_in_Test_Data = mse.y)
    } 
    out
       })
  
  output$validation = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    
    if (class(train_data()[,c(input$yAttr)]) == "factor"){
      y = train_data()[,input$yAttr]
      yhat = fit.rt()$validation
      confusion_matrix = table(y,yhat)
      accuracy = (sum(diag(confusion_matrix))/sum(confusion_matrix))
      out = list(Confusion_matrix_of_Validation = confusion_matrix, Accuracy_of_Validation = accuracy)
    } else {
      dft = data.frame(scale(data.frame(y = train_data()[,input$yAttr], yhat = fit.rt()$validation)))
      mse.y = mse(dft$y,dft$yhat)
      out = list(Mean_Square_Error_of_Standardized_Response_in_Training_Data = mse.y)
    } 
    out
  })
  

  #------------------------------------------------#
  output$results = renderPrint({
    
    if (is.null(input$file)) {return(NULL)}
     printcp(fit.rt()$model) # display the results
    # formula.mod()
  })
  
  
  #------------------------------------------------#
  output$summary = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    
    summary(fit.rt()$model) # detailed summary of splits  
  })
  
  
  #------------------------------------------------#
  output$imp = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    
    fit.rt()$imp
  })
  
  #------------------------------------------------#
  output$plot1 = renderPlot({
    
    if (is.null(input$file)) {return(NULL)}
    
    plotcp(fit.rt()$model) # visualize cross-validation results   
  })
  
  
  #------------------------------------------------#
  output$plot2 = renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    title1 = paste("Decision Nodes for", input$yAttr)
    
    fit.rt1 = fit.rt()$model
    fit.rt1$frame$yval = as.numeric(rownames(fit.rt()$model$frame))
    
    # create attractive postcript plot of tree 
    post(fit.rt1, 
         #file = "tree2.ps", 
         filename = "",   # will print to console
         use.n = FALSE,
         compress = TRUE,
         title = title1) 
    
  })
  
  output$plot3 = renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    title1 = paste("decision tree of", input$yAttr, "in test data")
    
  post((fit.rt()$model), 
       # file = "tree2.ps", 
       filename = "",   # will print to console
       use.n = TRUE,
       compress = TRUE,
       title = title1) 
  })
  
  
  #------------------------------------------------#
  nodes1 =  reactive({
    
  tree_nodes = as.data.frame(fit.rt()$model$where)
  colnames(tree_nodes) <- "node_number"
  # tree_nodes %>% head()
    
  a0 = as.numeric(rownames(fit.rt()$model$frame)); a0
  a1 = seq(1:nrow(fit.rt()$model$frame)); a1 
  a2 = as.vector(fit.rt()$model$where)
  node_num = a2
  for (i1 in 1:nrow(tree_nodes)){
    node_num[i1] = a0[a2[i1]]
  }
  
  tree_nodes1 <- fit.rt()$model$where %>% as.data.frame() %>% 
  cbind(node_num) %>% dplyr::select("node_num")
  tree_nodes1
  
  })

  output$nodesout1 = renderPrint({
    head(nodes1(),15)
  })
  
  output$nodesout <- renderDataTable({  	
    data.frame(nodes1(), train_data())
  }, options = list(lengthMenu = c(10, 20, 50), pageLength = 10))  # my edits here
  
  output$downloadData3 <- downloadHandler(
    filename = function() { "Nodes Info.csv" },
    content = function(file) {
      if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
      dft = data.frame(row_numer = row.names(nodes1()), nodes1(), train_data());   # data.frame(row_numer = row.names(nodes1()), nodes1())
      write.csv(dft, file, row.names=F, col.names=F)
    }
  )
  output$downloadData4 <- downloadHandler(
    filename = function() { "Nodes Info.csv" },
    content = function(file) {
      if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
      dft = data.frame(row_numer = row.names(nodes1()), nodes1(), train_data());   # data.frame(row_numer = row.names(nodes1()), nodes1())
      write.csv(dft, file, row.names=F, col.names=F)
    }
  )  

  output$prediction =  renderPrint({
    if (is.null(input$filep)) {return(NULL)}
    head(prediction(),10)
  })
  output$predictionorg =  renderPrint({
    if (is.null(input$filep)) {return(NULL)}
    head(predictionorg(),10)
  })  
  #------------------------------------------------#
  output$downloadData0 <- downloadHandler(
    filename = function() { "Predicted Data.csv" },
    content = function(file) {
      if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
      write.csv(predictionorg(), file, row.names=F, col.names=F)
    }
  )
  output$downloadData1 <- downloadHandler(
    filename = function() { "Predicted Data.csv" },
    content = function(file) {
      if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
      write.csv(prediction(), file, row.names=F, col.names=F)
    }
  )
  output$downloadData <- downloadHandler(
    filename = function() { "mTitanicAll.csv" },
    content = function(file) {
      write.csv(read.csv("data/mTitanicAll.csv"), file, row.names=F, col.names=F)
    }
  )

  
  })