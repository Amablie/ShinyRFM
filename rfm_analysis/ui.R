#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(shinyjs)
# library(shinythemes)
library(reactable)
library(waiter)

# Define UI for application that draws a histogram


dashboardPage( skin = "black",
               dashboardHeader(title = "RFM"),
               dashboardSidebar(
                 tabsetPanel(
                   id = "wizard",
                   type = "hidden",
                   tabPanel("page_1", 
                            "Welcome!",
                            actionButton("page_12", "PROXIMO"),
                            
                            ###  INPUT DA BASE DE DADOS -----------------------------------------------------------------
                            
                            # Horizontal line ----
                            tags$hr(),
                            
                            
                            fileInput("FILECSV", "Escolhar um Arquivo CSV",
                                      multiple = FALSE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")),
                            
                            checkboxInput("header", "Header", TRUE),
                            
                            # Input: Select separator ----
                            radioButtons("sep", "Separator",
                                         choices = c(Comma = ",",
                                                     Semicolon = ";",
                                                     Tab = "\t"),
                                         selected = ","),
                            
                            # Input: Select decimal ----
                            radioButtons("dec", "Decimal",
                                         choices = c(Comma = ",",
                                                     Period = "."),
                                         selected = ","),
                            
                            # Input: Select quotes ----
                            radioButtons("quote", "Quote",
                                         choices = c(None = "",
                                                     "Double Quote" = '"',
                                                     "Single Quote" = "'"),
                                         selected = '"'),
                            
                            
                            # radioButtons("disp", "Display",
                            #               choices = c(Head = "head",
                            #                         All = "all"),
                            #                selected = "head"),
                            actionButton("UPLOAD", label = "Upload"),
                            
                            
                   ),         
                   #### -----------------------------------------------------------------------------------------------
                   
                   ## SELECIONA AS COLUNAS CORRETAS PARA O TRATAMENTO ---------------------------------------------
                   
                   
                   tabPanel("page_2", 
                            "Only one page to go", 
                            fluidRow(actionButton("page_21", "VOLTAR"), actionButton("page_23", "PROXIMO")),
                            hr(),
                            
                            fluidPage(
                              # Copy the line below to make a select box 
                              selectizeInput("ID", label = h4("ID DO CLIENTE"),
                                             # multiple = TRUE,
                                             choices = NULL, 
                                             options = list(maxItems = 1)
                              ),
                              
                              selectizeInput("PEDIDO", label = h4("DATAS DO PEDIDO"), 
                                             # multiple = TRUE,
                                             choices = NULL, 
                                             options = list(maxItems = 1)
                              ),
                              
                              selectizeInput("PRECO", label = h4("RECEITA TOTAL DA COMPRA"),
                                             # multiple = TRUE,
                                             choices = NULL, 
                                             options = list(maxItems = 1)
                              ),
                            )
                   ),
                   
                   #### -----------------------------------------------------------------------------------------------
                   
                   ### DEFINIR DATA DE CORTE DA ANLISE ---------------------------------------------------------------
                   
                   tabPanel("page_3", 
                            "You're done!",
                            actionButton("page_32", "VOLTAR"),
                            
                            ### Data de corte da analise
                            fluidPage(
                              dateInput("DATE", label = h3("Data"), value = "2015-11-11")),
                            
                            # Copy the line below to make an action button
                            actionButton("ACTION", label = "Executar"),
                            
                   )
                   
                   #### ------------------------------------------------------------------------------------------------
                 )),
               
               
               
               dashboardBody( fluidPage(
                 fluidRow(
                 box( "MAPA DE CALOR",
                      width = 6,
                      plotOutput("PLOT_MAPACALOR")
                 ),
                 
                 box(" VOLUME DE CLIENTES POR CATEGORIA ",
                     width = 6,
                     plotOutput("PLOT_SEGMENTOS")
                 )
            ),
                 
                fluidRow( box( "Tabela RFM",
                      width = 12,
                      tableOutput("SCORE"))
                 ),
                 
                 
                 fluidRow(box("Tabela Original",
                              width = 12,
                              dataTableOutput("TABLE")))
               )
               )
               
)



