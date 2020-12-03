####################################################
#      Marketing Segmentation                      #
####################################################

if (!require("shiny")) {install.packages("shiny")}
if (!require("devtools")) {install.packages("devtools")}; library(devtools)
if (!require("plyr")) {install.packages("plyr")}; library(plyr)
if (!require("ggbiplot")) {install_github("ggbiplot","vqv")}
if (!require("ggplot2")) {install.packages("ggplot2")}
if (!require("dplyr")) {install.packages("dplyr")}
if (!require("scales")) {install.packages("scales")}
if (!require("grid")) {install.packages("grid")}; library("grid")
if (!require("cluster")) {install.packages("cluster")}
if (!require("mclust")) {install.packages("mclust")}
if (!require("MASS")) {install.packages("MASS")}
if (!require("scales")) {install.packages("scales")}
if (!require("gridExtra")) {install.packages("gridExtra")}

library("shiny")
library("cluster")
library("ggbiplot")
library("mclust")
library("MASS")
library("ggplot2")
library("scales")
library("gridExtra")
library("dplyr")
library('kableExtra')
library('data.table')





shinyServer(function(input, output){
  
  
  Dataset <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
      rownames(Dataset) = Dataset[,1]
      Dataset1 = Dataset[,2:ncol(Dataset)]
      #Dataset = t(Dataset)
      Dataset1 = as.data.frame(scale(Dataset1, center = T, scale = T))
      return(Dataset1)
    }
  })
  
  Dataset2 <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
      rownames(Dataset) = Dataset[,1]
      Dataset1 = Dataset[,2:ncol(Dataset)]
      return(Dataset1)
    }
  })

  output$downloadData1 <- downloadHandler(
    filename = function() { "ConneCtorPDASegmentation.csv" },
    content = function(file) {
      write.csv(read.csv("data/ConneCtorPDASegmentation.csv"), file, row.names=F)
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() { "ConneCtorPDADiscriminant.csv" },
    content = function(file) {
      write.csv(read.csv("data/ConneCtorPDADiscriminant.csv"), file, row.names=F)
    }
  )
  
  output$downloadData3 <- downloadHandler(
    filename = function() { "ConneCtorPDAClassification.csv" },
    content = function(file) {
      write.csv(read.csv("data/ConneCtorPDAClassification.csv"), file, row.names=F, col.names=F)
    }
  )
  
  t0 = reactive({
    set.seed(12345)
    if (input$select == "K-Means") ({
      
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      
      else {
        fit = kmeans(Dataset(),input$Clust)
        Segment.Membership =  fit$cluster
        d = data.frame(r.name = row.names(Dataset2()),Segment.Membership,Dataset2())
        return(d)
      }
    })
    
    else if (input$select == "Hierarchical") ({
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      else {
        distm <- dist(Dataset(), method = "euclidean") # distance matrix
        fit <- hclust(distm, method="ward") 
        Segment.Membership =  cutree(fit, k=input$Clust)
        d = data.frame(r.name = row.names(Dataset2()),Segment.Membership,Dataset2())
        return(d)
      }
    })
  })
    
    output$table <- renderDataTable({
      t0()
    }, options = list(lengthMenu = c(5, 30, 50,100), pageLength = 30))
    
    output$caption1 <- renderText({
      if (input$select == "Model Based") return ("Model Based Segmentation -  Summary")
      else if (input$select == "K-Means") return ("K-Means Segmentation -  Summary")
      else if (input$select == "Hierarchical") return ("Hierarchical Segmentation -  Summary")
      else return (NULL)
    })
    
    output$summary <- renderText({
      
      set.seed(12345)
      
      if (input$select == "K-Means") ({
        
        if (is.null(input$file)) {
          # User has not uploaded a file yet
          return(data.frame())
        }
        else {
          # 
          # fit = kmeans(Dataset(),input$Clust)
          # Segment.Membership = as.character(fit$cluster)
          # clustmeans = aggregate(Dataset2(),by = list(Segment.Membership), FUN = mean)
          # Summary = list(Segment.Membership = Segment.Membership, SegMeans =clustmeans, Count = table(Segment.Membership) )
          # 
          # 
          Summary<-t0()[-1] %>% 
            #mutate(Group = as.factor(Segment.Membership)) %>%
            group_by(Segment.Membership) %>%
            summarize_all(.funs = list(mean)) %>%
            arrange(Segment.Membership) %>%
            round(2)%>%
            mutate_if(is.numeric, function(x) {
              cell_spec(x, bold = T, 
                        color = spec_color(x, end = 0.9),
                        font_size = spec_font_size(x,begin = 14,end = 20))
            }) %>%
            kable(escape = F, align = "c") %>%
            kable_styling(c("striped", "condensed"), full_width = F)%>%
          footnote(general = "Mean value of all variables within each cluster. ")
          return(Summary)
        }
      })  
      else if (input$select == "Hierarchical") ({
            if (is.null(input$file)) {
              # User has not uploaded a file yet
              return(data.frame())
            }
            else {
              # d <- dist(Dataset(), method = "euclidean") # distance matrix
              # fit <- hclust(d, method="ward")
              # Segment.Membership =  as.character(cutree(fit, k=input$Clust))
              # clustmeans = aggregate(Dataset2(),by = list(Segment.Membership), FUN = mean)
              # Summary = list(Segment.Membership = Segment.Membership, SegMeans =clustmeans, Count = table(Segment.Membership), ModelSumm = fit )
              # Summary
              Summary<-t0()[-1] %>% 
               # mutate(Group = as.factor(Segment.Membership)) %>%
                group_by(Segment.Membership) %>%
                summarize_all(.funs = list(mean)) %>%
                arrange(Segment.Membership) %>%
                round(2)%>%
                mutate_if(is.numeric, function(x) {
                  cell_spec(x, bold = T, 
                            color = spec_color(x, end = 0.9),
                            font_size = spec_font_size(x,begin = 14,end = 20))
                }) %>%
                kable(escape = F, align = "c") %>%
                kable_styling(c("striped", "condensed"), full_width = F)%>%
                footnote(general = "Mean value of all variables within each cluster. ")
              return(Summary)
            }
          })
    })
       
    output$plotpca = renderPlot({ 
      
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      else {
        data.pca <- prcomp(Dataset(),center = TRUE,scale. = TRUE)
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
        
        fit = kmeans(Dataset(),input$Clust)
        
        classif1 = as.character(fit$cluster)
        data.pca <- prcomp(Dataset(),
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
        
        d <- dist(Dataset(), method = "euclidean") # distance matrix
        fit <- hclust(d, method="ward") 
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
    
    output$downloadData5 <- downloadHandler(
      filename = function() { "targeting.csv" },
      content = function(file) {
        write.csv(t1(), file, row.names=F)
      }
    )
    
    
})
  
