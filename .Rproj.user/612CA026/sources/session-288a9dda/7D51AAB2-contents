library(xml2)
library(rvest)
library(magrittr)
library(dplyr)
library(readr)
library(purrr)
library(tidyquant)
library(stringr)

ytd_stock_price_gather <- function(ticker) {
  
  current_date <- Sys.Date() + 1
  current_date_year <- format(current_date, "%Y")
  start_year_date <- base::as.Date(paste0(current_date_year, "-01-01"))
  
  tq_res <- tq_get(x = ticker, get = "stock.prices",
                   from = as.character(start_year_date),
                   to = as.character(current_date)) %>% 
    select(symbol, date, close) %>% 
    arrange(desc(date))
  
  ticker_ytd <- tq_res %>% 
    filter(date %in% c(min(date), max(date))) %>% 
    mutate(ytd_pct_chg = (close / lead(close)) - 1) %>% 
    rename(close_price = close) %>% 
    filter(date == max(date))
  
  message(paste0("Done with ticker ", ticker))
  Sys.sleep(0.5)
  
  return(ticker_ytd)
  
}

sandp500_html <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")

sandp_html_table <- html_elements(sandp500_html, "table") %>% 
  keep(function(x) unname(html_attrs(x)["id"]) == "constituents")
  
sandp_df <- sandp_html_table %>% 
  html_table() %>% 
  nth(1) %>% 
  mutate(ticker = str_replace(Symbol, "\\.", "-")) %>% 
  rename(company_name = Security) %>% 
  select(ticker, company_name) 

sandp_ytd_returns_list <- map(sandp_df$ticker, ~ytd_stock_price_gather(.x))

sandp_ytd_returns_current <- sandp_ytd_returns_list %>% 
  list_rbind() %>% 
  left_join(sandp_df, by = c("symbol" = "ticker"))

sandp_ytd_returns <- read_csv("./data/sandp_ytd_returns.csv",
         col_names = T,
         col_types = "cDddc")

if (max(sandp_ytd_returns_current$date) > max(sandp_ytd_returns$date)) {
  
  sandp_ytd_returns_updated <- bind_rows(
    sandp_ytd_returns_current,
    sandp_ytd_returns
  )
  
  write_csv(sandp_ytd_returns_updated,
            "./data/sandp_ytd_returns.csv")
  
}
