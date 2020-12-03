#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$freqDist <- renderPlot({
    req(input$data)
    data <- read.csv(input$data$datapath)
    hist(data[,2], breaks=50, main="Histogram of recency", xlab="Days since last visit", ylab="Frequency in the dataset")
  })
  
  output$scoreHist <- renderPlot({
    req(input$data)
	  data <- read.csv(input$data$datapath)
    analysis_date <- lubridate::as_date('2007-01-01', tz = 'UTC')
    
    # test-driving some lubridate applications
    date1 = data$most_recent_visit
    a1=date1[1]; a2=date1[2]; a1;a2
    a2-a1  # time differences this easy to do!
    sort(date1[1:20])   # sort() works peacefully.
    
  
    ## use rfm::rfm_table_customer() func  
    rfm_result <- rfm_table_customer(data = data,
                                    names(data)[1], 
                                    names(data)[3],
                                    names(data)[2], 
                                    names(data)[4], 
                                    analysis_date,
                                    recency_bins = input$bins, 
                                    frequency_bins = input$bins, 
                                    monetary_bins = input$bins)    # 0.10 secs
  
    rfm_score = rfm_result$rfm$rfm_score; rfm_score[1:10]
    hist(rfm_score, breaks=20)  # pfft. Drop.
  })
  
  output$graph1 <- renderPlot({
    req(input$data)
	  data <- read.csv(input$data$datapath)
	  analysis_date <- lubridate::as_date('2007-01-01', tz = 'UTC')
	  ## use rfm::rfm_table_customer() func  
	  rfm_result <- rfm_table_customer(data = data,
	                                   names(data)[1], 
	                                   names(data)[3],
	                                   names(data)[2], 
	                                   names(data)[4], 
	                                   analysis_date,
	                                   recency_bins = input$bins, 
	                                   frequency_bins = input$bins, 
	                                   monetary_bins = input$bins)    # 0.10 secs
    # Heat map output
    rfm_heatmap(rfm_result)
  
  })
  
  output$graph2 <- renderPlot({
    req(input$data)
	  data <- read.csv(input$data$datapath)
	  analysis_date <- lubridate::as_date('2007-01-01', tz = 'UTC')
	  ## use rfm::rfm_table_customer() func  
	  rfm_result <- rfm_table_customer(data = data,
	                                   names(data)[1], 
	                                   names(data)[3],
	                                   names(data)[2], 
	                                   names(data)[4], 
	                                   analysis_date,
	                                   recency_bins = input$bins, 
	                                   frequency_bins = input$bins, 
	                                   monetary_bins = input$bins)    # 0.10 secs
    # bar chart output
    rfm_bar_chart(rfm_result)
  
  })
  
  output$graph3 <- renderPlot({
    req(input$data)
	  data <- read.csv(input$data$datapath)
	  analysis_date <- lubridate::as_date('2007-01-01', tz = 'UTC')
	  ## use rfm::rfm_table_customer() func  
	  rfm_result <- rfm_table_customer(data = data,
	                                   names(data)[1], 
	                                   names(data)[3],
	                                   names(data)[2], 
	                                   names(data)[4], 
	                                   analysis_date,
	                                   recency_bins = input$bins, 
	                                   frequency_bins = input$bins, 
	                                   monetary_bins = input$bins)    # 0.10 secs
    # histogram output
    rfm_histograms(rfm_result)
    
  })
  
  output$graph4 <- renderPlot({
    req(input$data)
	  data <- read.csv(input$data$datapath)
	  analysis_date <- lubridate::as_date('2007-01-01', tz = 'UTC')
	  ## use rfm::rfm_table_customer() func  
	  rfm_result <- rfm_table_customer(data = data,
	                                   names(data)[1], 
	                                   names(data)[3],
	                                   names(data)[2], 
	                                   names(data)[4], 
	                                   analysis_date,
	                                   recency_bins = input$bins, 
	                                   frequency_bins = input$bins, 
	                                   monetary_bins = input$bins)    # 0.10 secs
    rfm_order_dist(rfm_result)
  
  })
  
  output$graph5 <- renderPlot({
    req(input$data)
	  data <- read.csv(input$data$datapath)
	  analysis_date <- lubridate::as_date('2007-01-01', tz = 'UTC')
	  ## use rfm::rfm_table_customer() func  
	  rfm_result <- rfm_table_customer(data = data,
	                                   names(data)[1], 
	                                   names(data)[3],
	                                   names(data)[2], 
	                                   names(data)[4], 
	                                   analysis_date,
	                                   recency_bins = input$bins, 
	                                   frequency_bins = input$bins, 
	                                   monetary_bins = input$bins)    # 0.10 secs
    # recency vs freq scatterplot
    rfm_rf_plot(rfm_result)
  })
  
  
  ####################################################################################
  ####################################################################################
  ####################################################################################
  ####################################################################################
  ####################################################################################
  
  segmentGenerator <- function(){
    
    data <- read.csv(input$data$datapath)
    analysis_date <- lubridate::as_date('2007-01-01', tz = 'UTC')
    
    rfm_result <- rfm_table_customer(data = data,
                                     names(data)[1], 
                                     names(data)[3],
                                     names(data)[2], 
                                     names(data)[4], 
                                     analysis_date,
                                     recency_bins = input$bins, 
                                     frequency_bins = input$bins, 
                                     monetary_bins = input$bins)    # 0.10 secs
    rfm_order_dist(rfm_result)
    
    rfm_score = rfm_result$rfm$rfm_score; rfm_score[1:10]
    ## Build segment descriptors - tentative
    test_r = rfm_result$rfm$recency_score
    test_f = rfm_result$rfm$frequency_score
    test_m = rfm_result$rfm$monetary_score
    
    min1 = min(test_r); max1 = max(test_r); lowmed1 = floor(max1/2)
    min1; max1; lowmed1
   
    rfm_segments = case_when(
      
      rfm_score == paste0(max1, max1, max1) %>% as.numeric() ~ "best.customers",
      rfm_score == paste0(min1, min1, max1) %>% as.numeric() ~ "lost",
      rfm_score < paste0(min1, min1, lowmed1) %>% as.numeric() ~ "lost n cheap",
      
      test_f == max1 ~ "loyals",
      test_m == max1 ~ "big spenders"    
    )
    
    # replace NA with 'Others'
    a0 = (is.na(rfm_segments));   rfm_segments[a0] = "Others"
    rfm_segments[1:40]
    
    # Calc Segment size
    rfm_segments = rfm_segments %>% 
      data_frame() %>% rename(segment = ".") %>% bind_cols(data) 
    
    rfm_segments %>%
      count(segment) %>%
      arrange(desc(n)) %>%
      rename(Segment = segment, Count = n)
    
    segment_data <- data.frame(rfm_segments[,2],
                               rfm_score,rfm_segments[,1],
                               rfm_segments[,3],
                               rfm_segments[,5],
                               rfm_segments[,4])
    
    colnames(segment_data) = c("customer_id",
                               "rfm_score",
                               "segment",
                               "recency_days",
                               "revenue",
                               "frequency")
    return (segment_data)
  }
  
  segment <- reactive({
    val = segmentGenerator()
    val = unique(val[,2:3])
    colnames(val) = c("rfm_score","segment")
    return(val)
  })
  
  output$downloadSegmentData <- downloadHandler(
    filename = function() { "Segment_data.csv" },
    content = function(file) {
      write.csv(segment(), file, row.names = F)
    }
  )
  
  output$segment1 <-renderPlot({
    analysis_date <- lubridate::as_date('2007-01-01', tz = 'UTC')
    req(input$segmentData)
    
	  segments = read.csv(input$segmentData$datapath)
	  rfm_segments = merge(segment(), segmentGenerator(), by="rfm_score")  # segment == segment_data
	  rfm_segments$segment.x <- NULL
	  colnames(rfm_segments)[3] <- "segment"
	  
	  # median recency
	  data <-
	    rfm_segments %>% # data_frame() %>%
	    group_by(segment) %>%
	    dplyr::select(segment, recency_days) %>%
	    summarize(median(recency_days)) %>%
	    dplyr::rename(segment = segment, avg_recency = `median(recency_days)`) %>%
	    arrange(avg_recency)

	  n_fill <- nrow(data)

	  ggplot(data, aes(segment, avg_recency)) +
	    geom_bar(stat = "identity", fill = brewer.pal(n = n_fill, name = "Set1")) +
	    xlab("Segment") + ylab("Median Recency") +
	    ggtitle("Median Recency by Segment") +
	    coord_flip() +
	    theme(
	      plot.title = element_text(hjust = 0.5)
	    )

  })

  output$segment2 <-renderPlot({
    analysis_date <- lubridate::as_date('2007-01-01', tz = 'UTC')
    req(input$segmentData)
    
    segments = read.csv(input$segmentData$datapath)
    rfm_segments = merge(segment(), segmentGenerator(), by="rfm_score")  # segment == segment_data
    rfm_segments$segment.x <- NULL
    colnames(rfm_segments)[3] <- "segment"

	  # Median Monetary Value
	  data <-
	    rfm_segments %>%
	    group_by(segment) %>%
	    dplyr::select(segment, revenue) %>%
	    summarize(median(revenue)) %>%
	    rename(segment = segment, avg_monetary = `median(revenue)`) %>%
	    arrange(avg_monetary)

	  n_fill <- nrow(data)

	  ggplot(data, aes(segment, avg_monetary)) +
	    geom_bar(stat = "identity", fill = brewer.pal(n = n_fill, name = "Set1")) +
	    xlab("Segment") + ylab("Median Monetary Value") +
	    ggtitle("Median Monetary Value by Segment") +
	    coord_flip() +
	    theme(
	      plot.title = element_text(hjust = 0.5)
	    )

  })
  
  output$segment3 <-renderPlot({
    analysis_date <- lubridate::as_date('2007-01-01', tz = 'UTC')
    req(input$segmentData)
    
    segments = read.csv(input$segmentData$datapath)
    rfm_segments = merge(segment(), segmentGenerator(), by="rfm_score")  # segment == segment_data
    rfm_segments$segment.x <- NULL
    colnames(rfm_segments)[3] <- "segment"
    
    # Median Monetary Value
    data <-
      rfm_segments %>%
      group_by(segment) %>%
      dplyr::select(segment, frequency) %>%
      summarize(median(frequency)) %>%
      rename(segment = segment, avg_frequency = `median(frequency)`) %>%
      arrange(avg_frequency)
    
    n_fill <- nrow(data)
    
    ggplot(data, aes(segment, avg_frequency)) +
      geom_bar(stat = "identity", fill = brewer.pal(n = n_fill, name = "Set1")) +
      xlab("Segment") + ylab("Median Frequency Value") +
      ggtitle("Median Frequency Value by Segment") +
      coord_flip() +
      theme(
        plot.title = element_text(hjust = 0.5)
      )
    
  })
  
  output$downloadData1 <- downloadHandler(
    filename = function(){ "Example1.csv" },
    content = function(file){
      write.csv(read.csv("data/Example1.csv"),file,row.names = F)
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function(){"Segment1.csv"},
    content = function(file){
      write.csv(read.csv("data/Segment1.csv"),file,row.names = F)
    }
  )
  
})
