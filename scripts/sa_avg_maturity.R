# South Africa Weighted Average Term to Maturity - Fixed Rate Bonds
# Source: National Treasury Budget Review 2026, Chapter 7, Figure 7.2
# Data: Weighted average term to maturity of fixed-rate bonds (years)
# Update: Once a year when Budget Review is published (February/March)

library(dplyr)

# --- Config ---
output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\south_africa_avg_maturity.csv"

# --- Hardcoded series from Budget Review Figure 7.2 ---
# Source: National Treasury 2026 Budget Review, Chapter 7
avg_maturity <- data.frame(
  Fiscal_Year = c("2013/14","2014/15","2015/16","2016/17","2017/18",
                  "2018/19","2019/20","2020/21","2021/22","2022/23",
                  "2023/24","2024/25","2025/26"),
  WAM_Anos    = c(12.9, 14.1, 14.9, 15.5, 16.0,
                  15.4, 14.6, 13.9, 13.5, 12.8,
                  12.5, 12.0, 11.7)
)

cat("Rows:", nrow(avg_maturity), "\n")
print(avg_maturity)

# --- Save ---
write.csv(avg_maturity, output_path, row.names = FALSE)
cat("Saved to", output_path, "\n")