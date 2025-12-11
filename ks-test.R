# Performs the One-sample Kolmogorov-Smirnov test

library(dplyr)

data_list <- list(
  "All Pages" = navigate.all.pages,
  "Static Pages" = navigate.static,
  "FAQ Page" = subpage.faq,
  "Home Page" = subpage.home,
  "List Page" = subpage.list,
  "Live Page" = subpage.live,
  "Tooltips Page" = subpage.tooltips
)

all_data <- bind_rows(data_list, .id = "Source")

all_power_avg_data <- all_data$Average.Total.Power..J.

ks_result <- ks.test(all_power_avg_data, "pnorm", mean = mean(all_power_avg_data), sd = sd(all_power_avg_data))

print(ks_result)
