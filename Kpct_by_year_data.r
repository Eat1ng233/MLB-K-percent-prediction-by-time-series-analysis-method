# 以Lahman Baseball資料集抓取 MLB 成立至今的每年聯盟 K% 數據
install.packages("Lahman")

library(Lahman)
library(dplyr)

data(Pitching)
head(Pitching)

# 計算每年 K% 數據
league_kpct <- Pitching %>%
  group_by(yearID) %>%
  summarise(
    SO = sum(SO, na.rm = TRUE),
    BFP = sum(BFP, na.rm = TRUE)
  ) %>%
  mutate(
    Kpct = SO / BFP
  )

# 轉成百分比
league_kpct <- league_kpct %>%
  mutate(
    Kpct_pct = Kpct * 100
  )

# 只抓取聯盟合併後的資料
league_kpct_MLB <- league_kpct[which(league_kpct$yearID >= 1903),]
head(league_kpct_MLB)