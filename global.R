

#install.packages("anytime")
library(anytime)
library(rfm)
library(tidyverse)
library(DT)
library(reactable)

####################################################################################################################
## CARREGANDO BASE DE DADOS
online_retail <- read.csv("Estatistica/2022_01/Dashboards/bases/OnlineRetail.csv",
                          dec = ".", sep = ",", header = T)

data <- read.csv("Estatistica/2022_01/Dashboards/bases/scanner_data.csv",
                 dec = ".", sep = ",", header = T)

cust_segment <- read.csv("Estatistica/2022_01/Dashboards/bases/customer_segmentation_10k.csv",
                         dec = ".", sep = ",", header = T)

head(data)
head(online_retail)

str(data)
str(online_retail)

# SELECTED <- function(base, ID , price, order_date){
#   select(base, 
#          ID , 
#          price, 
#          order_date)}

# anydate(c("11.23.2022 11:00"))

## TRATAMENTO ---------------------------------------------------------
data_clean <- function(dtframe, price, order_date ) {
  
  # '%d.%m.%Y %H:%M'
  # as.Date(order_date, format = "%Y-%m-%d")
  
  x <- dtframe %>% 
    mutate(Order_date = as.Date(order_date, format = "%Y-%M-%D"),
           Total_Price = replace(price, price<=0, NA))
  ## LIMPA NA
  df_data <-drop_na(x)
  
  
  return(df_data)
}

### SELECIONA AS COLUNAS DA BASE

lista_colunas<- function(x){
  lista <-colnames(x)
  return(lista)} 
## LISTA DE COLUNAS

colunas_choices <- lista_colunas(online_retail)



# ---------------------------------------------------------------------

### TRATAMENTO

# base <- online_retail %>% 
#   mutate(Total_Price = Quantity*Price,
#          teste = anydate(InvoiceDate))  

# str(base)

tab_base <- data_clean(online_retail, 
                       online_retail$UnitPrice, 
                       online_retail$InvoiceDate)


str(tab_base)

head(tab_base)



dataclear <- data_clean(data, 
                        data$Sales_Amount, 
                        data$Date )


str(dataclear)


cust_seg <- data_clean(cust_segment,
                       cust_segment$total_spent,
                       cust_segment$last_order)

str(cust_seg)



# -------------------------------------------------------------------------

### DATA DE CORTE
analysis.date <- Sys.Date()

### CALCULO SCORE
# rfm_result <- rfm_table_order(
#   data = dataclear,
#   customer_id = Customer_ID,
#   revenue = Sales_Amount,
#   order_date = Date,
#   analysis_date = analysis.date
# )

rfm_result <- rfm_table_order(
  data = cust_seg,
  customer_id = customer_id,
  revenue = Total_Price,
  order_date = Order_date,
  analysis_date = analysis.date
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

SEGMENT_RESULT(
  rfm_result
)

segmentoss

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

