#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

## AUMENTA  TAMANHO DO ARQUIVO QUE O SHINY RECEBE
options(shiny.maxRequestSize = 3000*1024^4)

shinyServer(function(input, output, session) {
  
  
  ### TROCA DE PAGINA NA ETAPA DE SUBIR OS DADOS ------------------------------------------------------------
  
  
  observeEvent(input$controller, {
    updateTabsetPanel(inputId = "switcher", selected = input$controller)
  })
  
  switch_page <- function(i) {
    updateTabsetPanel(inputId = "wizard", selected = paste0("page_", i))
  }
  
  observeEvent(input$page_12, switch_page(2)) # primeira para segunda pagina
  observeEvent(input$page_21, switch_page(1)) # volta pra primeira
  observeEvent(input$page_23, switch_page(3)) # vai para a terceira pagina
  observeEvent(input$page_32, switch_page(2)) # volta para a segunda pagina
  
  
  
  ######   REATIVIDADE ---------------------------------------------------------------------------
  
  
  
  ################ LEITURA DA BASE DE DADOS
  
  
  DF <- reactive({
    
    req(input$FILECSV)
    
    tryCatch(
      {
        df <- read.csv(input$FILECSV$datapath,
                       header = input$header,
                       sep = input$sep,
                       dec = input$dec,
                       quote = input$quote)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    
  })
  
  
  ############# LIMPEZA DA BASE DE DADOS
  # 
  # TRATAMENTO <- reactive({
  #   
  #   data_clean( dtframe = DF(),
  #               price = DF()$input$PRECO ,
  #               order_date = DF()$input$PEDIDO)
  # })   
  # 
  # 
  
  
  
  ############# DATA DE CORTE
  
  
  # DATE <- reactive({
  #   input$DATE
  # })
  # 
  
  
  ########### CALCULO RFM
  
  #   
  # RFM <- reactive({
  #   
  #     tb <- TRATATMENTO()
  #     
  #       rfm_table_order(
  #       data = tb,
  #       customer_id = input$ID,
  #       revenue = Total_Price,
  #       order_date = Order_date,
  #       analysis_date = input$DATE
  #     )
  #   })
  
  
  
  
  
  ############ SEGMENTOS
  
  
  SEGMENTO <- eventReactive(input$ACTION, {
    
    ## TRATAMENTO
    tb <- data_clean( dtframe = DF(),
                      price = DF()$input$PRECO ,
                      order_date = DF()$input$PEDIDO)
    ## CALCULO RFM
    rfm_results <- rfm_table_order(
      data = tb,
      customer_id = input$ID,
      revenue = Total_Price,
      order_date = Order_date,
      analysis_date = input$DATE)
    
    ## SEGMENTOS
    SEGMENT_RESULT(rfm_results)
  })
  
  
  
  ####### GRAFICO CLASSIFICACAO
  
  GRAF_CLIENT <- reactive({
    rfm_plot_total_segment()
  })
  
  ######## GRAFICO DO MAPA DE CALOR
  
  MAPA_CALOR <- reactive({
    rfm_heatmap()
    
  })
  
  
  
  ##################  ATUALIZA COLUNAS NA PAGINA 2 ########################################################################
  
  observeEvent(eventExpr = input$page_12,
               handlerExpr = {
                 updateSelectizeInput(session,
                                      "ID",
                                      choices = colnames(DF()),
                                      selected = NULL)
                 
                 
                 updateSelectizeInput(session,
                                      "PEDIDO",
                                      choices = colnames(DF()),
                                      selected = NULL)
                 
                 updateSelectizeInput(session,
                                      "PRECO",
                                      choices = colnames(DF()),
                                      selected = NULL)
               })
  
  
  
  ### BASE DE DADOS ORIGINAL ############################################################################
  
  output$TABLE <- renderDataTable({
    
    req(input$FILECSV)
    
    ##### tentativa de rodar
    tryCatch(
      {
        df <- read.csv(input$FILECSV$datapath,
                       header = input$header,
                       sep = input$sep,
                       dec = input$dec,
                       quote = input$quote)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    if(input$UPLOAD != 0) {
      return(datatable(head(df)))
    }
    else {
      return("Clique em upload")
    }
    
    # if(input$disp == "head") {
    #   return(head(df))
    # }
    # else {
    #   return(df)
    # }
    
    
  })
  
  
  
  ########  MAPA DE CALOR  ######################################################################
  
  
  output$PLOT_MAPACALOR <- renderPlot({
    
    req(input$ACTION)
    
    RFM() %>% 
      MAPA_CALOR()
    
    
  })
  
  
  ############ GRAFICOS SEGMENTOS  ##########################################################################
  
  output$PLOT_SEGMENTOS <- renderPlot({
    
    req(input$ACTION)
    
    
    seg_results <- SEGMENTO()
    
    GRAF_CLIENT(seg_results)
  })
  
  
  
  
  
  
  ########  TABELA RFM  ##################################################################
  
  
  output$SCORE <-  renderTable({
    # req(input$ACTION)
    # withProgress(message = 'Calculation in progress',
    #              detail = 'This may take a while...', value = 0, {
    #                for (i in 1:15) {
    #                  incProgress(1/15)
    #                  Sys.sleep(5)
    #                }
    #              })
    
    
    
    SEGMENTO()
  })
  
  
  
  
  
  
  
  
  
  
  
  
})  


