

#install.packages("anytime")
library(anytime)
library(rfm)
library(tidyverse)
library(DT)
library(readxl)
library(reactable)

####################################################################################################################




online_retail <- read.csv("Estatistica/2022_01/Dashboards/bases/online_retail_listing.csv",
                          dec = ",", sep = ";", header = T)

data <- read.csv("Estatistica/2022_01/Dashboards/bases/scanner_data.csv",
                 dec = ".", sep = ",", header = T)

head(data)

str(data)

str(online_retail)

SELECTED <- function(base, ID , price, order_date){
  select(base, 
         ID , 
         price, 
         order_date)}

# anydate(c("11.23.2022 11:00"))

## TRATAMENTO ---------------------------------------------------------
data_clean <- function(dtframe, price, order_date ) {
  
  # '%d.%m.%Y %H:%M'
  as.Date(order_date)
  
  x <- dtframe %>% 
    mutate(Order_date = anydate(order_date),
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

base <- online_retail %>% 
  mutate(Total_Price = Quantity*Price,
         teste = anydate(InvoiceDate))  

str(base)

tab_base <- data_clean(base, base$Total_Price, base$InvoiceDate)


str(tab_base)

head(tab_base)



dataclear <- data_clean(data, data$Sales_Amount, data$Date )


anydate(data$Date)

# -------------------------------------------------------------------------

### DATA DE CORTE
analysis.date <- 
  lubridate::as_date("2014-01-31")

### CALCULO SCORE
rfm_result <- rfm_table_order(
  data = tab_base,
  customer_id = Customer.ID,
  revenue = Total_Price,
  order_date = InvoiceDate,
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

table

