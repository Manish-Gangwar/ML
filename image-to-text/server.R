#################################################
#           Image to TeXt                       #
#################################################

shinyServer(function(input, output,session) {

  text = reactive({
    
    image_read(input$file$datapath) %>%
    
    image_resize("2000") %>%
    
    image_convert(colorspace = 'gray') %>%
    
    image_trim() %>%
    
    image_ocr()
  })
  
  output$text = renderText({
  text()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { "Image_to_text.txt" },
    content = function(file) {
      writeLines(unlist(strsplit(text(),"\n")), file)
    }
  )
  
  })

