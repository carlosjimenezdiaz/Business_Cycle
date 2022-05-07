# Libraries that we need
library(tidyquant)
library(tidyverse)
library(tibbletime)
library(viridis)

# Variables
n_periods = 4  # Num of years to calculate the SMA

# Getting the Slope of the Yield Curve and the recession periods (FRED database)
Economic_Data <- tidyquant::tq_get(c("T10Y2YM", "USREC"), 
                                   get  = "economic.data", 
                                   from = "1953-01-01") %>%
  spread(symbol, price) %>%
  dplyr::mutate(MA_Slope = TTR::SMA(T10Y2YM, n = 12*n_periods)/100) %>%
  dplyr::select(-T10Y2YM) %>%
  purrr::set_names(c("date", "Crisis", "MA_Slope")) %>%
  dplyr::mutate(Crisis = ifelse(Crisis == 1, "Yes", "No"))

# local Dataframes
Crisis_DB <- data.frame(Col_Legend = c(2000, 2008, 2009),
                        Label      = c("Dotcom", "GFC", "GFC"))

# Getting the VIX from Yahoo Finance (transforming the Freq to Month)
VIX_DB <- tq_get("^VIX",
                 from = Sys.Date() - lubridate::years(100),
                 to   = Sys.Date(),
                 get  = "stock.prices",
                 complete.cases = TRUE) %>%
  tibbletime::as_tbl_time(date) %>%
  tibbletime::as_period("monthly", side = "start") %>%
  dplyr::select(date, adjusted) %>%
  dplyr::mutate(MA_VIX = SMA(adjusted, n = 12*n_periods)/100) %>%
  dplyr::select(-adjusted)


# Joining both DB
Global_DB <- Economic_Data %>% 
  left_join(VIX_DB, by = "date") %>% 
  drop_na() %>%
  as_tibble() %>%
  dplyr::mutate(Col_Legend = floor_date(date, years(n_periods)) %>% lubridate::year(),
                Col_Legend = case_when(lubridate::year(date) == lubridate::year(Sys.Date()) ~ lubridate::year(Sys.Date()),
                                       TRUE ~ Col_Legend))


# Plotting the results
n_years = 15 # Last X years to display in the chart

Mid_Point_Vix   = (max(Global_DB$MA_VIX) + min(Global_DB$MA_VIX))/2
Mid_Point_Slope = (max(Global_DB$MA_Slope) + min(Global_DB$MA_Slope))/2

Global_DB %>% 
  dplyr::mutate(MA_VIX_Lag    = dplyr::lag(MA_VIX),
                MA_Slope_Lag  = dplyr::lag(MA_Slope),
                tail          = last(date) - date,
                segment_alpha = pmax(0, (20 - tail)/20),
                Year          = lubridate::year(date) %>% as.integer() %>% as.factor(),
                Steps         = 1:n())  %>%
  ggplot(aes(x = MA_VIX, y = MA_Slope, xend = MA_VIX_Lag, yend = MA_Slope_Lag, colour = Col_Legend  %>% as.character())) +
  geom_point() +
  geom_hline(yintercept = Mid_Point_Slope, linetype="dashed", color = "gray") +
  geom_vline(xintercept = Mid_Point_Vix, linetype="dashed", color = "gray") +
  geom_text(aes(x = max(MA_VIX), y = min(MA_Slope)), colour = "black", label = "Recession", size = 5, nudge_x = -0.03, nudge_y = 0.001) +
  geom_text(aes(x = max(MA_VIX), y = max(MA_Slope)), colour = "black", label = "Early Recovery", size = 5, nudge_x = -0.03, nudge_y = 0.001) +
  geom_text(aes(x = min(MA_VIX), y = max(MA_Slope)), colour = "black", label = "Mid-Expansion", size = 5, nudge_x = 0.03, nudge_y = 0.001) +
  geom_text(aes(x = min(MA_VIX), y = min(MA_Slope)), colour = "black", label = "Late-Expansion", size = 5, nudge_x = 0.03, nudge_y = 0.001) +
  geom_segment(alpha = 0.40, size = 3, show.legend = FALSE) +
  theme_bw() +
  scale_fill_viridis() +
  labs(title    = str_glue("Yield Curve Slope vs VIX cycle - using an {n_periods} Years Phase."),
       subtitle = ,
       caption  = "By: Carlos Jimenez",
       x        = "SMA of the VIX",
       y        = "SMA of the Slope (10Y - 2Y)") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  theme(axis.line    = element_line(colour = "black"),
        legend.title = element_blank())
