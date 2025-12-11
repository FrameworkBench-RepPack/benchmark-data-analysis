library(tidyverse)
library(dunn.test)

paths <- list.files(path = "combined-results", pattern = "\\.csv$", full.names = TRUE)

data <- paths %>%
  map_dfr(function(x) {
    read_csv(x, show_col_types = FALSE) %>%
      mutate(Site = tools::file_path_sans_ext(basename(x)))
  })

results_list <- data %>%
  group_split(Site) %>%
  set_names(map(., ~ unique(.x$Site))) %>% 
  map(function(df) {
    head(df)
    dunn <- dunn.test(x = df$`Total Power (J)`, 
                      g = df$Framework, 
                      method = "bh", 
                      altp = TRUE, 
                      table = FALSE, 
                      list = TRUE,
                      alpha=0.001)
    
    # Create the table for this specific site
    tibble(
      Site = unique(df$Site),
      Comparison = dunn$comparisons,
      Z_Statistic = dunn$Z,
      P_Value = dunn$altP,
      P_Adjusted = dunn$altP.adjusted
    )
  })

if(!dir.exists("output")) dir.create("output")
iwalk(results_list, ~ write_csv(.x, file.path("output", paste0(.y, ".csv"))))

print(results_list)
