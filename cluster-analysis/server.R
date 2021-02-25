####################################################
#      Cluster Analysis                         #
####################################################
if(!require("shiny")) {install.packages("shiny")}
if(!require("cluster")){install.packages("cluster")}
if(!require("ggbiplot")){install.packages("ggbiplot")}
if(!require("mclust")){install.packages("mclust")}
if(!require("MASS")){install.packages("MASS")}
if(!require("Hmisc")){install.packages("Hmisc")}
if(!require("pastecs")){install.packages("pastecs")}

library('shiny')
library('cluster')
library('ggbiplot')
library('mclust')
library('MASS')
library('Hmisc')
library('pastecs')

shinyServer(function(input, output){
  
  
  Dataset <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
      rownames(Dataset) = Dataset[,1]
      Dataset11 = Dataset[,2:ncol(Dataset)]
      return(Dataset11)
    }
  })
  
  nu.Dataset = reactive({
    data = Dataset()
    Class = NULL
    for (i in 1:ncol(data)){
      c1 = class(data[,i])
      Class = c(Class, c1)
    }
    nu = which(Class %in% c("numeric","integer"))
    nu.data = data[,nu] 
    return(nu.data)
  })
  
  output$xvarselect <- renderUI({
    if (is.null(input$file)) {return(NULL)}
    else {
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    checkboxGroupInput("xAttr", "Select only numerical X variables",
                       colnames(Dataset()), colnames(nu.Dataset()) )
    }
  })
  
  output$fxvarselect <- renderUI({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    if (is.null(input$file)) {return(NULL)}
    else {
    checkboxGroupInput("fxAttr", "Factor variable in X",
                       setdiff(colnames(Dataset()),"") )
    }
  })

  mydata = reactive({
   mydata = Dataset()[,c(input$xAttr)]
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

    out = list(Dimensions = Dimensions, Summary =Summary ,Tail=Tail, Head=Head, MissingDataRows=Missing, num.data=nu.data, factr.data=fa.data)
    return(out)
  })
  
  Dataset2 = reactive({
    x0 = mydata()
    x01 = scale(x0, center = T, scale = T)
    dstd = data.frame(x01)
    #colnames(dstd) = c(colnames(x01))
    return(dstd)
  })
  
  output$head = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {
      out()[4]
    }
  })
  
  output$scldt = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {
      dt=Dataset2()
      list(Note="data is 'mean-centered' and 'scaled' for cluster analysis", dtt=head(dt))
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
      out()[5]
    }
  })
  
  output$summ = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {
      out()[1:2]
    }
  })
  
  
  
  t0 = reactive({
    set.seed(12345)
    if (input$select == "."){} else {
      if (input$select == "K-Means") ({
      
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      
      else {
        fit = kmeans(Dataset2(),input$Clust)
        Segment =  fit$cluster
        d = data.frame(r.name = row.names(mydata()),Segment,mydata())
        Cluster.Membership = as.character(fit$cluster)
        clustmeans = aggregate(mydata(),by = list(Cluster.Membership), FUN = mean)
        Summary = list(d, Cluster.Membership = Cluster.Membership, Cluster.Means =clustmeans, Count = table(Cluster.Membership) )
        return(Summary)
      }
    })
    
    else if (input$select == "Hierarchical") ({
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      else {
        distm <- dist(Dataset2(), method = "euclidean") # distance matrix
        fit <- hclust(distm, method="ward.D") 
        Segment =  cutree(fit, k=input$Clust)
        d = data.frame(r.name = row.names(mydata()),Segment,mydata())
        Cluster.Membership =  as.character(cutree(fit, k=input$Clust))
        clustmeans = aggregate(mydata(),by = list(Cluster.Membership), FUN = mean)
        Summary = list(d, Cluster.Membership = Cluster.Membership, Cluster.Means =clustmeans, Count = table(Cluster.Membership), ModelSumm = fit )
        return(Summary)
      }
    })
    }
  })
    
    output$table <- renderDataTable({
      if (is.null(input$file)) {return(NULL)}
      else {
      t0()[[1]]
      }
    }, options = list(lengthMenu = c(5, 30, 50,100), pageLength = 30))
    
    output$caption1 <- renderText({
      if (input$select == ".") return ("choose cluster algorithm and click ''Apply Changes'' ")
      #else if (input$select == "Model Based") return ("Model Based Segmentation -  Summary")
      else if (input$select == "K-Means") return ("K-Means Segmentation -  Summary")
      else if (input$select == "Hierarchical") return ("Hierarchical Segmentation -  Summary")
      else return (NULL)
    })
    
    output$summary <- renderPrint({
      if (input$select == "."){} else {
      
      if (input$select == "K-Means") ({
        
        if (is.null(input$file)) {
          # User has not uploaded a file yet
          return(data.frame())
        }
        else {
          Summary = list( t0()[2], t0()[3], t0()[4])
          Summary
        }
      })
      
      else  if (input$select == "Model Based") ({
        
        if (is.null(input$file)) {
          # User has not uploaded a file yet
          return(data.frame())
        }
        
        else {
          Summary = list(t0()[2], t0()[3], t0()[4], t0()[5] )
          Summary
        }
      })
      
      else if (input$select == "Hierarchical") ({
        if (is.null(input$file)) {
          # User has not uploaded a file yet
          return(data.frame())
        }
        else {
          d <- dist(Dataset2(), method = "euclidean") # distance matrix
          fit <- hclust(d, method="ward.D") 
          Cluster.Membership =  as.character(cutree(fit, k=input$Clust))
          clustmeans = aggregate(mydata(),by = list(Cluster.Membership), FUN = mean)
          Summary = list(Cluster.Membership = Cluster.Membership, Cluster.Means =clustmeans, Count = table(Cluster.Membership), ModelSumm = fit )
          Summary
        }
      })
      }
    })
       
    output$plotpca = renderPlot({ 
      
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      else {
        data.pca <- prcomp(Dataset2(),center = TRUE,scale. = TRUE)
        plot(data.pca, type = "l"); abline(h=1)    
      }
    })
    
    
    output$plot = renderPlot({  
      set.seed(12345)
      
      if (input$select == "K-Means") ({
        if (is.null(input$file)) {
          # User has not uploaded a file yet
          return(data.frame())
        }
        
        fit = kmeans(Dataset2(),input$Clust)
        
        classif1 = as.character(fit$cluster)
        data.pca <- prcomp(Dataset2(),
                           center = TRUE,
                           scale. = TRUE)
        
        # plot(data.pca, type = "l"); abline(h=1)    
        
        g <- ggbiplot(data.pca,
                      obs.scale = 1,
                      var.scale = 1,
                      groups = classif1,
                      ellipse = TRUE,
                      circle = TRUE)
        
        g <- g + scale_color_discrete(name = '')
        g <- g + theme(legend.direction = 'horizontal',
                       legend.position = 'top')
        print(g)
        
      })
      
      else if (input$select == "Hierarchical") ({
        if (is.null(input$file)) {
          # User has not uploaded a file yet
          return(data.frame())
        }
        
        d <- dist(Dataset2(), method = "euclidean") # distance matrix
        fit <- hclust(d, method="ward.D") 
        plot(fit) # display dendogram
        groups <- cutree(fit, k=input$Clust) # cut tree into 5 clusters
        # draw dendogram with red borders around the 5 clusters
        rect.hclust(fit, k=input$Clust, border="red") 
      })
    })
    
    output$downloadData4 <- downloadHandler(
      filename = function() { "segmentation.csv" },
      content = function(file) {
      write.csv(t0(), file, row.names=F)
      }
      
    )

})
  
