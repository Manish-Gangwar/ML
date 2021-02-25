

server <- function(input, output,session) {
  
  observe_helpers(help_dir = "helper", withMathJax = TRUE)
  
  
  
  output$data_helper <- renderUI({
    
    helper(shiny_tag = "",
           icon = "question",
           colour = "green",
           type = "markdown",
           content = input$ex_data
    )
  })
  
  output$text <- renderUI({
    if(input$na_str=="other")
    {
      textInput("na_text", "Input", "please enter NA string")
    }
  })
  
  
  
  
  
  #------------------------------------------------------Tab-1 Data Upload-----------------------------------------------#
  
  data <- reactive({
    if (is.null(input$file)) { 
      
      df <- sample_data(input$ex_data)
      
      return(df)
      
      
      
    }
    else{
      df <- read.csv(input$file$datapath,
                     sep = input$sep,
                     na.strings = if(input$na_str=="other"){input$na_text}else{input$na_str},
                     header = input$header,stringsAsFactors = TRUE
      )
      return(df)
    }
  })
  
  
  output$head <- renderDataTable({head(data(),n = 10)})
  
  
  #----------------------------------------------------Tab-2 Non-Metric Detection & Conversion ----------------------------#
  data_fr_str <- reactive({data_frame_str(data())}) # get structure of uploaded dataset
  
  
  output$df_str <- renderDataTable({
    data_fr_str()
  })
  
  
  #if unique value of any column is less than 7 and it is numeric or integer then auto-select it  for factor conversion
  output$vars2conv_fact <- renderUI({
    #if (is.null(input$file)) { return(NULL) }
    #else{
    cond_df <- data_fr_str() %>% filter((class=="numeric"| class=="integer") & unique_value_count<7)
    cols <- cond_df$variable
    selectInput("selVar2conv",
                label = "Select columns for factor conversion ",
                choices=names(data()),
                multiple = TRUE,
                selectize = TRUE,
                selected=cols)
    # }
  })
  
  
  values <- reactiveValues(df_data = NULL) 
  
  observeEvent(input$conv, {
    
    data_copy <- data()
    Factors <- input$selVar2conv
    # print(Factors)
    if(is.null(Factors)){
      values$df_data <- data_copy
    }else{
      data_copy[Factors]<-lapply(data_copy[Factors],factor)
      #  print(str(data_copy))
      values$df_data <- data_copy
    }
    # print(data_copy[Factors])
    
    
  })
  
  
  
  data_fac <- reactive({if(is.null(values$df_data))
  {
    return(data())
  }else{
    return(values$df_data)
  }
    
  }) # convereted factorial dataset
  
  output$df_conv_str <- renderDataTable({head(data_frame_str(data_fac()),n=10)})
  
  
  
  #------------------------------------------------- Tab-3 Missing Value--------------------------------------------#
  
  # sub-tab-1
  output$missing_plot <- renderPlot({
    display_missing_percentage(data_fac())
  })
  
  
  # sub-tab-2
  miss_cols_list <- reactive({miss_cols(data_fac())})
  
  
  # list of integer column for imputation
  output$intvars2imp <- renderUI({
    # if (is.null(input$file)) { return(NULL) }
    # else{
    int_cols <- miss_cols_list()
    print(int_cols)
    cols <- int_cols[[1]]
    selectInput("IntVar2Imp",
                label = "Select numerical columns for imputation ",
                choices=cols,
                multiple = TRUE,
                selectize = TRUE,
                selected=cols)
    # }
  })
  
  output$knn_parms <- renderUI({
    if(input$selIntImpMethod!='knn'){
      return(NULL)}
    else{
      checkboxInput("ind","create replacement indicator",value = TRUE)
    }
  })
  
  
  # list of categorical columns for imputation  
  output$catvars2imp <- renderUI({
    #if (is.null(input$file)) { return(NULL) }
    # else{
    cat_cols <- miss_cols_list()
    cols <- cat_cols[[2]]
    selectInput("CatVar2Imp",
                label = "Select categorical columns for imputation",
                choices=cols,
                multiple = TRUE,
                selectize = TRUE,
                selected=cols)
    # }
  })
  
  
  # imputed_df <- reactive({
  #   imputer(df = data_fac(),
  #           num_method = input$selIntImpMethod,
  #           cat_method = input$selCatImpMethod,
  #           int_cols = input$IntVar2Imp,
  #           cat_cols = input$CatVar2Imp,
  #           original_flag = TRUE
  #           )
  # })
  
  values1 <- reactiveValues(
    # if(is.null(input$IntVar2Imp)){return(data_fac())}
    #    else{return(NULL)}
    df_imputed = NULL
  ) #imputed df after factor conversion
  
  
  values2 <- reactiveValues(replaced_by = NULL) 
  
  observeEvent(input$imp, {
    
    if(is.null(input$IntVar2Imp)){
      values1$df_imputed <- data_fac()
      sendSweetAlert(
        session = session,
        title = "Information",
        text = "Since no variable is Imputed, data from previous tab will be used in Transformation tab",
        type = "info"
      )
      
    }else{
      df <- data_fac()
      imputed_df <-  imputer(df = df,
                             num_method = input$selIntImpMethod,
                             cat_method = input$selCatImpMethod,
                             int_cols = input$IntVar2Imp,
                             cat_cols = input$CatVar2Imp,
                             original_flag = input$orig_col,
                             replacement_ind = ifelse(!is.null(input$ind),input$ind,FALSE)
      )
      values1$df_imputed <- imputed_df[[1]]
      values2$replaced_by <- imputed_df[[2]]
      sendSweetAlert(
        session = session,
        title = "Imputed Successfully !!",
        text = NULL,
        type = "success"
      )
    }
    
    
    
  })
  
  
  
  output$int_imp <- renderDataTable({head(values1$df_imputed,10)})
  
  output$int_replacement <- renderDataTable({values2$replaced_by})
  
  
  output$imp_download <- downloadHandler(
    filename = function() {
      paste("imputed", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values1$df_imputed, file, row.names = FALSE)
    }
  )
  
  #--------------------------------------------------- Tab-4 Data Transformation-------------------------------------------#
  
  values3 <- reactiveValues(trans_df = NULL) 
  
  
  df_imputed <- reactive({
    
    if(is.null(values1$df_imputed)){
      return(data())
    }else{
      return(values1$df_imputed)
    }
  })
  
  df_imputed_cols <- reactive({df_imp_str <- data_frame_str(df_imputed())
  int_cols <- df_imp_str %>% filter(class=="numeric"| class=="integer")
  return(int_cols$variable)
  
  })
  # cols_to_tr <- reactive({miss_cols(df_imputed())})
  
  output$std_vars <- renderUI({
    
    
    cols <- df_imputed_cols()
    # if (is.null(input$file)) { return(NULL) }
    # else{
    #int_cols <- c("a","b")
    print(cols)
    #int_cols <- cols_to_tr[[1]]
    #print(int_cols)
    #cols <- int_cols[[1]]
    selectInput("trans_cols",
                label = "Select numerical columns for transformation ",
                choices=cols,
                multiple = TRUE,
                selectize = TRUE,
                selected=cols)
    # }
  })
  
  
  observeEvent(input$transform, {
    
    if(is.null(input$trans_cols)){
      values3$trans_df <- df_imputed()
      sendSweetAlert(
        session = session,
        title = "Information",
        text = "Since no variable is selected, data from previous tab will be used in dummy tab",
        type = "info"
      )
      
    }else{
      df <- df_imputed()
      
      trans_df <- data_transform(df = df,
                                 method = input$method,
                                 cols = input$trans_cols,
                                 original_flag = input$orig_col1)
      
      
      values3$trans_df <- trans_df
      
      sendSweetAlert(
        session = session,
        title = "Transformed Successfully !!",
        text = NULL,
        type = "success"
      )
    }
    
    
    
  })
  
  # output$downloadData1 <- downloadHandler(
  #   filename = function() { paste(str_split(input$file$name,"\\.")[[1]][1],"_centralities.csv",collapse = "") },
  #   content = function(file) {
  #     write.csv(centralities(), file, row.names=F)
  #   }
  # )
  
  
  output$trans_download <- downloadHandler(
    filename = function() {
      # if (is.null(input$file)){
      #  paste(str_split(input$file$name,"\\.")[[1]][1],"_transformed.csv",collapse = "")
      # }else{
      paste("transformed", ".csv", sep = "")
      #}
      
    },
    content = function(file) {
      write.csv(values3$trans_df, file, row.names = FALSE)
    }
  )
  
  
  
  # 
  # 
  # 
  # trans_df <- reactive({data_transform(df = df_imputed(),
  #                                      method = input$method,
  #                                      cols = input$trans,
  #                                      original_flag = input$orig_col1)})
  
  output$trans_df <- renderDataTable({head(values3$trans_df,10)})
  
  #--------------------------------------------------- Tab-5 Dummy Variables-------------------------------------------#
  
  df_transformed <- reactive({
    
    if(is.null(values3$trans_df)){
      return(data())
    }else{
      return(values3$trans_df)
    }
  })
  
  
  
  
  
  
  
  output$vars2conv <- renderUI({
    #if (is.null(input$file)) { return(NULL) }
    # else{
    df <- df_transformed()
    cols <- names(df%>%select_if(is.factor))
    selectInput("selVar",
                label = "Select columns for conversion ",
                choices=cols,
                multiple = TRUE,
                selectize = TRUE,
                selected=cols)
    #    }
  })
  
  
  
  values_dummy <- reactiveValues(dummy_df = NULL) 
  
  observeEvent(input$conv2, {
    
    if(is.null(input$selVar)){return(data())}
    else{
      if (input$selVar == ""){
        values_dummy$dummy_df <- values3$trans_df
      }else{
        df <- values3$trans_df
        dummy_df <- dummy_columns(df,
                                  select_columns = input$selVar,
                                  remove_selected_columns = input$rem_org,
                                  remove_first_dummy = input$rem_first_dum,
                                  ignore_na = input$ign_NA)
        values_dummy$dummy_df <- dummy_df
      }
      
    }
    
    
  })
  
  
  
  data_fac_dummy <- reactive({values_dummy$dummy_df}) # convereted factorial dataset
  
  # dummy_data <- reactive({
  #                      if(length(input$selVar)==0){return(data())}
  #                      else{
  #                          dummy_df <- dummy_columns(data(),
  #                                                    select_columns = input$selVar,
  #                                                    remove_selected_columns = input$rem_org,
  #                                                    remove_first_dummy = input$rem_first_dum,
  #                                                    ignore_na = input$ign_NA)
  #                      return(dummy_df)
  #                          }
  #                        })
  
  
  output$dummy_table <- renderDataTable({
    head(data_fac_dummy(),
         n=10)})
  
  
  output$dummy_download <- downloadHandler(
    filename = function() {
      paste("dummy", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_fac_dummy(), file, row.names = FALSE)
    }
  )
  
  
  #-------------------------TAB-5 Summary Report---------------------------------------------
  
  # Tab-1 data screen
  output$dim <- renderPrint({paste0("Uploaded dataset has ",dim(data())[1]," observations and ",dim(data())[2]," variables")})
  output$screen_summary <- renderPrint({ds_screener(data())})
  
  # Tab-2 Summary Stats
  output$num_var_for_summary <- renderUI({
    # if (is.null(input$file)) { return(NULL) }
    # else{
    int_cols_1 <- names(dplyr::select_if(data(),is.numeric))
    print(int_cols_1)
    #print(int_cols)
    #cols <- int_cols[[1]]
    selectInput("num_var",
                label = "",
                choices=int_cols_1,
                multiple = FALSE,
                selectize = TRUE,
                selected=int_cols_1)
    # }
  })
  
  # selected data
  d_summary <- eventReactive(input$submit_summary, {
    # validate(need(input$var_summary != '', 'Please select a variable.'))
    req(input$num_var)
    ds_summary_stats(data(), !! sym(input$num_var))
  })
  
  
  output$num_summary <- renderPrint({
    d_summary()
  })
  
  
  
  #----Tab 3 Freq Qual
  output$cat_var_for_freq <- renderUI({
    # if (is.null(input$file)) { return(NULL) }
    # else{
    cat_cols_1 <- names(dplyr::select_if(data(),is.factor))
    # print(int_cols_1)
    #print(int_cols)
    #cols <- int_cols[[1]]
    selectInput("char_var",
                label = "",
                choices=cat_cols_1,
                multiple = FALSE,
                selectize = TRUE,
                selected=cat_cols_1)
    # }
  })
  
  
  
  f1_title <- eventReactive(input$submit_fqual, {
    h3('Frequency Table')
  })
  
  output$freq1_title <- renderUI({
    f1_title()
  })
  
  fqual_out <- eventReactive(input$submit_fqual, {
    if(as.character(input$char_var)==""){return(NULL)}
    else{
      ki <- ds_freq_table(data(), !! sym(as.character(input$char_var)))
      ki
    }
    
  })
  
  # output
  output$freq_qual <- renderPrint({
    fqual_out()
  })
  
  output$qual_vert <- renderPlot({
    if(input$submit_fqual==0 | as.character(input$char_var)=="" ){return(NULL)}
    else{
      plot(fqual_out())
    }
    
  })
  
  
  #------Tab 4 Frequency-Quantitative
  output$cont_var_for_freq <- renderUI({
    # if (is.null(input$file)) { return(NULL) }
    # else{
    cont_cols_1 <- names(dplyr::select_if(data(),is.numeric))
    # print(int_cols_1)
    #print(int_cols)
    #cols <- int_cols[[1]]
    selectInput("cont_var",
                label = "",
                choices=cont_cols_1,
                multiple = FALSE,
                selectize = TRUE,
                selected=cont_cols_1)
    # }
  })
  
  
  
  # selected data
  d_freq_quant <- eventReactive(input$submit_fquant, {
    data <- data()[, input$cont_var]
  })
  # 
  # # update filter slider
  # observe({
  #   updateSliderInput(session = session,
  #                     inputId = 'filter_quant',
  #                     min = min(d_freq_quant()),
  #                     max = max(d_freq_quant()),
  #                     step = 1,
  #                     value = c(min(d_freq_quant()), max(d_freq_quant()))
  #   )
  # })
  # 
  # 
  
  # # # filters
  # fil_quant_data <- reactive({
  #   
  #   min_data <- input$filter_quant[1]
  #   max_data <- input$filter_quant[2]
  #   
  #   # f_data <- d_summary()[d_summary()[, 1] >= min_data & d_summary()[, 1] <= max_data, 1]
  #   f_data <- d_freq_quant()[d_freq_quant() >= min_data & d_freq_quant() <= max_data]
  #   fdata <- as.data.frame(f_data)
  #   names(fdata) <- as.character(input$var_freq_quant)
  #   fdata
  # })
  # 
  
  
  f1_title <- eventReactive(input$submit_fquant, {
    h3('Frequency Table')
  })
  
  output$freq2_title <- renderUI({
    f1_title()
  })
  
  fquant_out <- eventReactive(input$submit_fquant, {
    ki <- ds_freq_table(data(), !! sym(as.character(input$cont_var)),input$bins)
    ki
  })
  
  # output
  output$freq_quant <- renderPrint({
    fquant_out()
  })
  
  output$hist_freq_quant <- renderPlot({
    if(input$submit_fquant==0){return(NULL)}
    else{
      plot(fquant_out())
    }
    
  })
  
  #----Tab 5 Corr Plot
  output$corr_plot <- renderPlot({
    complete_data <- data()[complete.cases(data()),]
    plot_correlation(complete_data, type = "continuous") # for continuous vars
    
  })
  
}

