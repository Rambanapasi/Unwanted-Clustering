library(quantmod)
library(rugarch)
library(tidyquant)


# S&P 500 (SPY ETF proxy)
getSymbols("^GSPC", src = "yahoo", from = "2023-01-01", to = Sys.Date(), periodicity = "hourly")

head(SPY)

# Hourly SPY (S&P 500 ETF proxy)
spy <- tq_get("^N225",
              get = "stock.prices",
              from = "2015-01-01",
              to   = Sys.Date(),
              interval = "60m")   # hourly

# Hourly Nasdaq (QQQ ETF proxy)
qqq <- tq_get("QQQ", get = "stock.prices",
              from = "2023-01-01",
              interval = "60m")

# Hourly Gold (GLD ETF proxy)
gld <- tq_get("GLD", get = "stock.prices",
              from = "2023-01-01",
              interval = "60m")

# lets get straight the observations in the market 

# what is the distribution of weekly returns in terms of points. 
# with that what is the volatility and ultimate holding period


  weekly_points <- spy %>%
  select(-adjusted) %>%
  arrange(date) %>%
  mutate(points = high - low,
         week = floor_date(date, "week")) %>%   # bucket by week
  group_by(week) %>%
  summarise(
    Weekly_points = last(close) - first(open),
    last_date = max(date),
    .groups = "drop"
  )


mu <- mean(weekly_points$Weekly_points, na.rm = TRUE)
med <- median(weekly_points$Weekly_points, na.rm = TRUE)
sigma <- sd(weekly_points$Weekly_points, na.rm = TRUE)

ggplot(weekly_points, aes(x = Weekly_points)) +
  geom_histogram(bins = 50,
                 fill = "lightblue", alpha = 0.6, color = "white") +
  geom_vline(xintercept = mu, color = "blue", linetype = "solid", size = 1) +
  geom_vline(xintercept = med, color = "darkgreen", linetype = "solid", size = 1) +
  geom_vline(xintercept = mu + sigma, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = mu - sigma, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribution of Weekly Point Movements",
       subtitle = paste0("Mean = ", round(mu, 2),
                         " | Median = ", round(med, 2),
                         " | SD = ", round(sigma, 2)),
       x = "Weekly Points", y = "Frequency") +
  theme_minimal()


# lets study the clusters, in terms of volatility what sould i expect


quantiles_2pct <- quantile(
  weekly_points$Weekly_points,
  probs = seq(0.02, 0.98, 0.02),  # 2% steps
  na.rm = TRUE
)

quantile_table <- tibble(
  Percentile = paste0(names(quantiles_2pct)),
  Weekly_Points = as.numeric(quantiles_2pct)
)

quantile_table



direction_stats <- weekly_points %>%
  mutate(direction = case_when(
    Weekly_points > 0 ~ "Up",
    Weekly_points < 0 ~ "Down",
    TRUE ~ "Flat"
  )) %>%
  count(direction) %>%
  mutate(
    pct = n / sum(n) * 100
  )

direction_stats

weekly_returns <- spy %>%
  select(-adjusted) %>%
  arrange(date) %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(week) %>%
  summarise(
    open = first(open),
    close = last(close),
    .groups = "drop"
  ) %>%
  mutate(ret = log(close / lag(close))) %>%
  filter(!is.na(ret))

# Rolling volatility over 20 weeks (≈ 5 months)
spy_returns <- weekly_returns %>%
  mutate(roll_vol = rollapply(ret, 20, sd, fill = NA, align = "right"))

ggplot(spy_returns, aes(x = week)) +
  geom_line(aes(y = abs(ret)), color = "lightblue", alpha = 0.5) +
  geom_line(aes(y = roll_vol), color = "red", size = 1) +
  labs(title = "Volatility Clustering: Weekly Abs Returns (blue) vs Rolling Vol (red)",
       y = "Volatility / Abs Return", x = "Week") +
  theme_minimal()



# Define a standard GARCH(1,1) model
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "std"   # Student-t distribution captures fat tails
)

# Fit the model to your weekly returns
fit <- ugarchfit(spec, data = weekly_returns$ret)

fit