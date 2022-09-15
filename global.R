

#install.packages("anytime")
library(anytime)
library(rfm)
library(tidyverse)
library(DT)
library(reactable)

####################################################################################################################




online_retail <- read.csv("datasets/OnlineRetail.csv",
                          dec = ".", sep = ",", header = T)


data <- read.csv("datasets/scanner_data.csv",
                 dec = ".", sep = ",", header = T)

head(data)
head(online_retail)

str(data)
str(online_retail)
# 
# SELECTED <- function(base, ID , price, order_date){
#   select(base, 
#          ID , 
#          price, 
#          order_date)}

# anydate(c("11.23.2022 11:00"))

## TRATAMENTO ---------------------------------------------------------
data_clean <- function(dtframe, price, order_date ) {
  
  # '%d.%m.%Y %H:%M'
  # as.Date(order_date)
  
  x <- dtframe %>% 
    mutate(Order_date = as.Date(order_date, c("%m-%d-%y", "%m/%d/%y")),
           Total_Price = replace(price, price<=0, NA))
  ## LIMPA NA
  df_data <-drop_na(x)
  
  
  return(df_data)
}

### SELECIONA AS COLUNAS DA BASE

# lista_colunas<- function(x){
#   lista <-colnames(x)
#   return(lista)} 
## LISTA DE COLUNAS

# colunas_choices <- lista_colunas(online_retail)



# ---------------------------------------------------------------------

### TRATAMENTO

# base <- online_retail %>% 
#   mutate(Total_Price = Quantity*Price,
#          teste = anydate(InvoiceDate))  


tab_base <- data_clean(online_retail, 
                       online_retail$UnitPrice, 
                       online_retail$InvoiceDate)



str(tab_base)

head(tab_base)



database <- data_clean(data, 
                       data$Sales_Amount, 
                       data$Date )

str(database)

# -------------------------------------------------------------------------

### DATA DE CORTE
analysis.date <- 
  anydate("11-01-2015")

### CALCULO SCORE
rfm_result <- rfm_table_order(
  data = tab_base,
  customer_id = CustomerID,
  revenue =  UnitPrice,
  order_date = InvoiceDate,
  analysis_date = analysis.date
)


rfm_res <- rfm_table_order(
  data = database,
  customer_id = Customer_ID,
  revenue =  Sales_Amount,
  order_date = Date,
  analysis_date = Sys.Date()
)

### CALCULO SEGMENTO

SEGMENT_RESULT <- function(rfm){ 
  
  
  ### SEGMENTOS DE CLIENTES
  segment_names <- c("Campeoes", "Clientes fieis", "Lealdade potencial",
                     "Clientes Recentes", "Promissor", "Precisam de atencao", "Prestes a hibernar",
                     "Em risco", "Nao posso perde-los" , "Perdidos")
  
  recency_lower <- c(4, 2, 3, 4, 3, 2, 2, 1, 1, 1)
  recency_upper <- c(5, 5, 5, 5, 4, 3, 3, 2, 1, 2)
  frequency_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
  frequency_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
  monetary_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
  monetary_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
  
  
  
  ### TABELA FINAL
  final <- rfm_segment(rfm,
                       segment_names,
                       recency_lower,
                       recency_upper,
                       frequency_lower, 
                       frequency_upper, 
                       monetary_lower,
                       monetary_upper)
  
  return(final)}

segmentoss <- SEGMENT_RESULT(
  rfm_result
)



## GRÃFICO QUANTIDADE POR PERFIL
rfm_plot_total_segment <- function(segment){
  segment %>%
    group_by(segment) %>% 
    count(segment) %>%
    arrange(desc(n)) %>%
    rename(Segment = segment, Count = n) %>% 
    ggplot(aes(y = fct_reorder(Segment, Count),
               x = Count, fill= Segment)) +
    labs(x='Numero de clientes',y='Categoria',title='Numero de clientes por categoria')+
    theme_bw()+
    theme(legend.position = "none")+
    geom_col()
}


rfm_plot_total_segment(segmentoss)




###########################################################################################################################


# # #SELECT COLUMNS
# dados <- select(base, Customer.ID ,Total_Price, InvoiceDate)
# 
# ## TRATAMENTO ---------------------------------------------------------
# data_clean <- function(dtframe, total_price, order_date ) {
#   
#   
#   x <- dados %>% 
#     mutate(InvoiceDate = as.Date(order_date, '%d.%m.%Y %H:%M'),
#            Total_Price = replace(total_price, total_price<=0, NA))
#   ## LIMPA NA
#   df_data <-drop_na(x)
#   
#   
#   return(df_data)
# }
# 
# 
# 
# lista_colunas<- function(x){
#   lista <-colnames(x)
#   return(lista)} 
# ## LISTA DE COLUNAS
# 
# lista_colunas(online_retail)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # ---------------------------------------------------------------------
# 
# ### APLICAÃÃO DA FUNÃÃO DE TRATAMENTO
# 
# tab_base <- data_clean(base, base$Total_Price, base$InvoiceDate)
# 
# str(tab_base)
# 
# head(tab_base)
# 
# 
# # -------------------------------------------------------------------------
# 
# 
# ## RFM 
# 
# # data_rfm <- function(data, analysis.date){
# #   
# #   rfm <- data %>% group_by(Customer.ID) %>% 
# #     summarise(Recency = as.numeric(analysis.date - max(InvoiceDate)), 
# #               Frequency = n(), 
# #               Monetary = sum(Total_Price),
# #               First_shop = min(InvoiceDate))
# #   return(rfm)
# # }
# 
# 
# ## DATA DA ANALISE
# 
# 
# ## DATA DA ANALISE
# 
# 
# analysis.date <- 
#   lubridate::as_date("2013-01-31")
# 
# 
# 
# 
# 
# SEGMENT_RESULT <- function(rfm){ 
#   
#   
#   ### CALCULO
#   rfm_result <- rfm_table_order(
#     data = tb,
#     customer_id = ID,
#     revenue = REVENUE,
#     order_date = ORDERDATE,
#     analysis_date = DATEANALYSIS
#   )
#   
#   ### SEGMENTOS DE CLIENTES
#   segment_names <- c("Campeoes", "Clientes fieis", "Lealdade potencial",
#                      "Clientes Recentes", "Promissor", "Precisam de atencao", "Prestes a hibernar",
#                      "Em risco", "Nao posso perde-los" , "Perdidos")
#   
#   recency_lower <- c(4, 2, 3, 4, 3, 2, 2, 1, 1, 1)
#   recency_upper <- c(5, 5, 5, 5, 4, 3, 3, 2, 1, 2)
#   frequency_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
#   frequency_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
#   monetary_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
#   monetary_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
#   
#   
#   
#   ### TABELA FINAL
#   final <- rfm_segment(rfm,
#                        segment_names,
#                        recency_lower,
#                        recency_upper,
#                        frequency_lower, 
#                        frequency_upper, 
#                        monetary_lower,
#                        monetary_upper)
#   
#   return(final)}
# 
# 
# SEGMENT_RESULT(rfm_result)
# 
# 
# 
# ## TABELA COM SCORE
# rfm_result <- rfm_table_order(
#   data = tab_base,
#   customer_id = Customer.ID,
#   revenue = Total_Price,
#   order_date = InvoiceDate, 
#   analysis_date = analysis.date
# )
# 
# ## visÃ£o geral
# rfm_heatmap(rfm_result)
# rfm_bar_chart(rfm_result, bar_color = "purple")
# 
# ## grafico de dispersÃ£o
# rfm_fm_plot(rfm_result, point_color = "purple")
# rfm_rm_plot(rfm_result, point_color = "purple")
# rfm_rf_plot(rfm_result, point_color = "purple")
# 
# 
# 
# ## categorias
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ## Plots de segmentaÃ§Ã£o
# rfm_plot_median_monetary(segment)
# rfm_plot_median_frequency(segment)
# rfm_plot_median_recency(segment)
# 
# ## total de segmentos
# rfm_plot_total_segment <- function(segment){
#   segment %>%
#     group_by(segment) %>% 
#     count(segment) %>%
#     arrange(desc(n)) %>%
#     rename(Segment = segment, Count = n) %>% 
#     ggplot(aes(y = fct_reorder(Segment, Count),
#                x = Count, fill= Segment)) +
#     labs(x='Numero de clientes',y='Categoria',title='NÃºmero de clientes por categoria')+
#     theme_bw()+
#     theme(legend.position = "none")+
#     geom_col()
# }
# 
# rfm_plot_total_segment(segment)
# ## tabela
# 
# datatable(segment)
# 
# 
# 
