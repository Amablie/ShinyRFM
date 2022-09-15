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
  
  
  
  ### LEITURA DA BASE DE DADOS
  DF <- reactive({
    
    req(input$FILECSV)
    
    df <- read.csv(input$FILECSV$datapath,
                   header = input$header,
                   sep = input$sep,
                   dec = input$dec,
                   quote = input$quote)
    
    return(df)
    
  })
  
  
  ### LIMPEZA DA BASE DE DADOS
  LIMPEZA <- reactive({
    (req$page_32)
    
    data_clean(
      dtframe = DF(),
      price = input$PRECO ,
      order_date = input$PEDIDO
    )
  })
  
  
  
  
  ### DATA DE CORTE
  DATE <- reactive({
    input$DATE
  })
  
  
  
  ### CALCULO RFM
  RFM <- reactive({
    
    req(input$ACTION)
    df <- read.csv(input$FILECSV$datapath,
                   header = input$header,
                   sep = input$sep,
                   dec = input$dec,
                   quote = input$quote)
    
    
    tb <- LIMPEZA(df)
    
    rfm_table_order(
      data = tb,
      customer_id = input$ID,
      revenue = Total_Price,
      order_date = Order_Date,
      analysis_date = DATE
    )
  })
  
  
  
  ### CLASSIFICAÃÃO DOS SEGMENTOS
  SEGMENTO <- reactive({
    SEGMENT_RESULT()
    
  })
  
  
  
  # GRAFICO CLASSIFICACAO
  
  GRAF_CLIENT <- reactive({
    rfm_plot_total_segment()
  })
  
  
  MAPA_CALOR <- reactive({
    rfm_heatmap()}
  )
  
  ### BASE DE DADOS ORIGINAL --------------------------------------------------
  
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
      return(datatable(df))
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
  
  
  
  # ALTERA COLUNAS ----------------------------------------------------------
  
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
  
  
  
  
  output$SCORE <- renderTable({
    req(input$ACTION)
    df <- RFM()
    segment <- SEGMENTO( tb
                         
    )
    
    if(input$ACTION != 0) {
      return(head(segment))
    }
    else {
      return("Clique em Executar")
    }
  })
  
  
  ### Mostra base de dados original
  
  
  output$PLOT_MAPACALOR <- renderPlot({
    
    req(input$ACTION)
    
    
    resultado <- RFM()
    MAPA_CALOR(resultado)
  })
  
  
  output$PLOT_SEGMENTOS <- renderPlot({
    
    req(input$ACTION)
    
    
    seg_results <- SEGMENTO(
      RFM())
    
    GRAF_CLIENT(seg_results)
  })
  
  
})  






