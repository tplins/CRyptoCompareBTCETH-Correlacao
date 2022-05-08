library(tidyquant)
library(timetk)
library(dplyr)
library(tidyr)
library(ggplot2)

# Data Frame
crypto <- tq_get(c("BTC-USD", "LTC-USD"),
                 from="2011-12-15",
                 to="2022-05-07"
) %>%
  select(date,symbol,close) %>%
  mutate(symbol=sub("-USD","",symbol))


# Simple Graphic
crypto %>%
  plot_time_series(date,close,symbol,.smooth = FALSE)

# Log Graphic
crypto %>%
  mutate(close_log=log1p(close)) %>%
  plot_time_series(date,close_log,symbol,.smooth = FALSE)

# Graphic with Log
crypto %>% 
  group_by(symbol) %>%
  mutate(close_standardize = standardize_vec(log1p(close))) %>%
  ungroup() %>%
  plot_time_series(date,close_standardize,symbol, .smooth = FALSE)

#Measure BTC-ETH
crypto_wider <- crypto %>%
  pivot_wider(names_from = symbol,values_from = close) %>%
  drop_na() %>%
  mutate(across(BTC:LTC,.fns=log1p))

cor(crypto_wider$BTC,crypto_wider$LTC)

ggplot(crypto_wider)+
  aes(x=BTC,y=LTC)+
  geom_polygon()
