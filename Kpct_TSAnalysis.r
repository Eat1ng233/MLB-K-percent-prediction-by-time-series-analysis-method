install.packages("tseries")
library(tseries)
library(forecast)

# 聯盟歷年 K% 時間數列圖
par(mfrow = c(1, 1))
tsplot(x = league_kpct_MLB$yearID, y = league_kpct_MLB$Kpct_pct, 
       xlab = "Season", ylab = "MLB Strike Out Rate(%)")

# 轉成時間數列
kpct_ts <- ts(
  league_kpct_MLB$Kpct_pct,
  start=min(league_kpct_MLB$yearID),
  frequency=1
)

# summary statistics
summary(league_kpct_MLB$Kpct_pct)
sd(league_kpct_MLB$Kpct_pct)

hist(league_kpct_MLB$Kpct_pct,
     main = "Distribution of MLB Strikeout Rate",
     xlab = "Strikeout Rate (%)",
     freq = FALSE)

boxplot(league_kpct_MLB$Kpct_pct,
        main = "Boxplot of MLB Strikeout Rate",
        ylab = "Strikeout Rate (%)")

# 檢測是否為stationary
acf(
  kpct_ts,
  main = "ACF of MLB K%"
)

adf.test(kpct_ts) #Kpct is not stationary

# 對資料做差分, 並檢測差分後的資料是否為stationary
kpct_diff <- diff(kpct_ts)
plot(kpct_diff, main = "Differenced MLB K%")

# 差分後的基本統計量
summary(kpct_diff)
sd(kpct_diff)
boxplot(kpct_diff,
        main = "Boxplot of MLB Strikeout Rate Difference",
        ylab = "Strikeout Rate Difference (%)")

# 檢測ACF
acf(kpct_diff, main = "ACF of Differenced MLB K%")
adf.test(kpct_diff) # 差分後的資料為 stationary

# 檢測PACF
pacf(
  kpct_diff,
  main = "PACF of MLB K%"
)

# 後續可以考慮 ARIMA(0, 1, 1), ARIMA(1, 1, 1)
fit011 <- arima(league_kpct_MLB$Kpct_pct, order = c(0,1,1))
fit111 <- arima(league_kpct_MLB$Kpct_pct, order = c(1,1,1))

# model comparison: AIC, BIC, AICc
calc_ic <- function(fit, n){
  k <- length(fit$coef) + 1   # 參數數量 + sigma2
  aic <- AIC(fit)
  bic <- AIC(fit, k = log(n))
  aicc <- aic + (2 * k * (k + 1)) / (n - k - 1)
  
  data.frame(
    AIC = aic,
    BIC = bic,
    AICc = aicc
  )
}

n <- length(na.omit(y))

ic_table <- rbind(
  ARIMA_011 = calc_ic(fit011, n),
  ARIMA_111 = calc_ic(fit111, n)
)

# predict performance & model comparison: RMSE, MAE
h <- 10
train <- y[1:(length(y)-h)]
test  <- y[(length(y)-h+1):length(y)]

fit011_tr <- arima(train, order = c(0,1,1))
fit111_tr <- arima(train, order = c(1,1,1))

pred011 <- predict(fit011_tr, n.ahead = h)$pred
pred111 <- predict(fit111_tr, n.ahead = h)$pred

rmse <- function(actual, pred){
  sqrt(mean((actual - pred)^2, na.rm = TRUE))
}

mae <- function(actual, pred){
  mean(abs(actual - pred), na.rm = TRUE)
}

forecast_table <- data.frame(
  Model = c("ARIMA(0,1,1)", "ARIMA(1,1,1)"),
  RMSE = c(rmse(test, pred011), rmse(test, pred111)),
  MAE = c(mae(test, pred011), mae(test, pred111))
)

# residual analysis: by plot
par(mfrow = c(1, 2))

plot(residuals(fit011), main = "Residuals: ARIMA(0,1,1)")
acf(residuals(fit011), main = "ACF: ARIMA(0,1,1)")

plot(residuals(fit111), main = "Residuals: ARIMA(1,1,1)")
acf(residuals(fit111), main = "ACF: ARIMA(1,1,1)")

par(mfrow = c(1, 1))

# estimation
estimation_table <- function(fit){
  se <- sqrt(diag(fit$var.coef))
  t <- fit$coef/se
  p <- 2*(1-pnorm(abs(t)))
  
  coef_table <- data.frame(
    Estimate = fit$coef,
    Std_Error = se,
    t_value = t,
    p_value = p
  )
  
  round(coef_table,4)
}

estimation_table(fit111)


# normality check
# histogram
par(mfrow = c(1,2))
res <- residuals(fit111)

hist(res,
     breaks = 15,
     probability = TRUE,
     main = "Histogram of Residuals",
     xlab = "Residual")

curve(dnorm(x,
            mean = mean(res),
            sd = sd(res)),
      add = TRUE,
      lwd = 2)

# qqplot
qqnorm(res)
qqline(res, col = "red")

# forecasting
fc <- forecast(fit111, h = 10, level = c(80, 95))

par(mfrow = c(1,1))
plot(fc,
     main = "Forecast of MLB League Strikeout Rate",
     xlab = "Season",
     ylab = "Strikeout Rate (%)")
