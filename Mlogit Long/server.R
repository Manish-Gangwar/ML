#################################################
#                 ISB MLogit                    #
#################################################

#Required Packages
if(!require("shiny")) {install.packages("shiny")}
if(!require("pastecs")){install.packages("pastecs")}
if(!require("RColorBrewer")){install.packages("RColorBrewer")}
if(!require("Hmisc")){install.packages("Hmisc")}
if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("reshape2")){install.packages("reshape2")}

if (!require("corrplot")) {install.packages("corrplot")}
if (!require("caret")) {install.packages("caret")}
if (!require("Rfast")) {install.packages("Rfast")}
if (!require("dplyr")) {install.packages("dplyr")}
if (!require("multiROC")) {install.packages("multiROC")}

library(shiny)
library(pastecs)
library(Hmisc)
library(RColorBrewer)
library(Hmisc)
library(ggplot2)
library(mlogit)
library(reshape2)
library(dplyr)
library(caret)
library(Rfast)
library(nnet)
library(dummies)
library(multiROC)
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
  output$Choicevarselect <- renderUI({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    selectInput("ChoiceAttr", "Select choice/outcome variable",
                colnames(Dataset()), colnames(Dataset())[1])
    
  })
  output$Individualvarselect <- renderUI({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    selectInput("IndividualAttr", "Select observation ID variable",
                setdiff(colnames(Dataset()),input$ChoiceAttr), setdiff(colnames(Dataset()),input$ChoiceAttr)[1])
    
  })
  
  #convert into factor variables:
  output$Alternativesvarselect <- renderUI({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
   
    selectInput("AlternativesAttr", "Select avaialble alternatives column",
                       setdiff(colnames(Dataset()),c(input$IndividualAttr,input$ChoiceAttr)), setdiff(colnames(Dataset()),c(input$IndividualAttr,input$ChoiceAttr))[1])
    
  })
  
  
  output$Alternativefeaturesvarselect <- renderUI({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    checkboxGroupInput("AlternativefeaturesAttr", "Select alternatives specific variables",
                       setdiff(colnames(Dataset()),c(input$IndividualAttr,input$AlternativesAttr,input$ChoiceAttr)),"")
    
  })
  
  output$Individualfeaturesvarselect <- renderUI({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)

    checkboxGroupInput("IndividualfeaturesAttr", "Select observation specific variable",
                       setdiff(colnames(Dataset()),c(input$IndividualAttr,input$AlternativefeaturesAttr,input$AlternativesAttr,input$ChoiceAttr)),"" )

  })

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
    Summary = list(Numeric.data = round(stat.desc(nu.data)[c(4,5,6,8,9,12,13),] ,4), factor.data = describe(fa.data))
    # Summary = list(Numeric.data = round(stat.desc(nu.data)[c(4,5,6,8,9,12,13),] ,4), factor.data = describe(fa.data))
    
    a = seq(from = 0, to=200,by = 4)
    j = length(which(a < ncol(nu.data)))
    out = list(Dimensions = Dimensions,Summary =Summary ,Tail=Tail,fa.data,nu.data,a,j, Head=Head,MissingDataRows=Missing)
    return(out)
  })
  output$summary = renderPrint({
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
  
  output$summary = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {
      out()[1:2]
    }
  })

  
  # mydata2 = reactive({
  #   H=dfidx(Dataset(),choice = input$yAttr,varying = input$xAttr)
  #   return(H)
  # })
  
  rhs=reactive({
      if (length(input$IndividualfeaturesAttr)>=1){
ind.features=paste(input$IndividualfeaturesAttr,collapse = "+")
         alt.features=(paste(input$AlternativefeaturesAttr,collapse = "+"))
         rhs=paste(alt.features,"|",ind.features,sep = "")
        }else{rhs=paste(input$AlternativefeaturesAttr,collapse = "+")}
      
      return(rhs)
      })
  
  
  ols = reactive({
    #rhs=paste(input$xAttr,collapse = "+")
   # reg.form= paste(input$yAttr,"~",rhs,collapse = "")
   #  mlogit.reg=mlogit(reg.form,H)
    #mlogit.reg = mlogit(paste(input$yAttr,"~", rhs() , sep=""), data = mydata2())
    #mlogit.reg = mlogit(formula=as.formula(paste(input$yAttr,"~", rhs() ,sep="")),data=mydata2())
    formula.reg=as.formula(paste(input$ChoiceAttr,"~",rhs(),sep = ""))
    #DCE_data<- mlogit.data(data=Dataset(), choice = input$ChoiceAttr, shape = "long", alt.var = input$AlternativesAttr,id.var = input$IndividualAttr) 
    a <- mlogit(formula=formula.reg,Dataset())

    return(a)
  })
  
  ols.pred = reactive({
    b <- predict(ols(), newdata=pred.readdata())
    return(b)
  })

  
  output$olssummary = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {
  summary(ols())
    }
   #print(ols())
  })
  output$confusionmatrix = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {
    data.fit=(fitted(object = ols(), outcome=FALSE))
    trial=Rfast::rowMaxs(data.fit,value = FALSE)
    data.fit=as.data.frame(data.fit)
    data.fit$predict=colnames(data.fit)[trial]
    
    choice.col=(as.vector(Dataset()[,input$ChoiceAttr]))
    data.fit$actual=as.vector(Dataset()[which(choice.col==1),input$AlternativesAttr])
    data.try=as.data.frame(data.fit)
    
    caret::confusionMatrix(as.factor(data.try$predict),as.factor(data.try$actual))
    }
  })
  
  output$probablities = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {
  data.fit=(fitted(object = ols(), outcome=FALSE))
    trial=Rfast::rowMaxs(data.fit,value = FALSE)
    data.fit=as.data.frame(data.fit)
    data.fit$predict=colnames(data.fit)[trial]

  choice.col=(as.vector(Dataset()[,input$ChoiceAttr]))
  data.fit$actual=as.vector(Dataset()[which(choice.col==1),input$AlternativesAttr])
  data.fit$obs_ID=as.vector(Dataset()[which(choice.col==1),input$IndividualAttr])
  #data.try=as.data.frame(data.fit)
  print(data.fit)
    }
})
  output$ROC = renderPlot({
    if (is.null(input$file)) {return(NULL)}
    else {  
    data.fit=(fitted(object = ols(), outcome=FALSE))
    trial=Rfast::rowMaxs(data.fit,value = FALSE)
    data.fit=as.data.frame(data.fit)
    #data.fit$predict.choice=colnames(data.fit)[trial]
    
    choice.col=(as.vector(Dataset()[,input$ChoiceAttr]))
    #label_true=as.vector(Dataset()[which(choice.col==1),input$AlternativesAttr])
    
    
    colnames(data.fit) <- paste(colnames(data.fit), "_pred_MN")
    #data.try=as.data.frame(data.fit)
    
    #test_df=data[data$choice==1,]
    test_df=Dataset()[which(choice.col==1),input$AlternativesAttr]
    true_label <- dummies::dummy(test_df, sep = ".")
    
    colnames(true_label) <- gsub(".*?\\.", "", colnames(true_label))
     colnames(true_label) <- paste(colnames(true_label), "_true")
     final_df <- cbind(true_label, data.fit)
    roc_res <- multi_roc(final_df, force_diag=T)
    pr_res <- multi_pr(final_df, force_diag=T)

    plot_roc_df <- plot_roc_data(roc_res)
    plot_pr_df <- plot_pr_data(pr_res)


    ggplot(plot_roc_df, aes(x = 1-Specificity, y=Sensitivity)) +
      geom_path(aes(color = Group, linetype=Method), size=1.5) +
      geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),
                   colour='grey', linetype = 'dotdash') +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.justification=c(1, 0), legend.position=c(.95, .05),
            legend.title=element_blank(),
            legend.background = element_rect(fill=NULL, size=0.5,
                                           linetype="solid", colour ="black"))
  }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { "Travel.csv" },
    content = function(file) {
      write.csv(read.csv("data/Travel.csv"), file, row.names=F, col.names=F)
    }
  )
  
  output$downloadData1 <- downloadHandler(
    filename = function() { "Predicted Probabilities.csv" },
    content = function(file) {
      data.fit=(fitted(object = ols(), outcome=FALSE))
      trial=Rfast::rowMaxs(data.fit,value = FALSE)
      data.fit=as.data.frame(data.fit)
      data.fit$predict=colnames(data.fit)[trial]
      choice.col=(as.vector(Dataset()[,input$ChoiceAttr]))
      data.fit$actual=as.vector(Dataset()[which(choice.col==1),c(input$AlternativesAttr)])
      data.fit$obs_ID=as.vector(Dataset()[which(choice.col==1),c(input$IndividualAttr)])
      data.try=as.data.frame(c(data.fit))
      write.csv(data.try, file, row.names=F, col.names=F)
    }
  )  
  
  
  output$downloadData2 <- downloadHandler(
    filename = function() { "Prediction New Data.csv" },
    content = function(file) {
    write.csv(ols.pred(), file, row.names=F, col.names=F)
    }
  ) 
  
  # output$resplot2 = renderPlot({
  #    plot(ols()$residuals,ols()$fitted.values)
  #  })

  # output$resplot3 = renderPlot({
  #   plot(mydata()[,input$yAttr],ols()$fitted.values)#
  # })
  #
  })
