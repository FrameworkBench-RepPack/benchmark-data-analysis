library(tidyverse)

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
    
    groups <- unique(df$Framework)
    
    pairwise_results <- combn(groups, 2, simplify = FALSE) %>%
      map_dfr(function(pair) {
        
        group_1 <- df %>% filter(Framework == pair[1]) %>% pull(`Total Power (J)`)
        group_2 <- df %>% filter(Framework == pair[2]) %>% pull(`Total Power (J)`)
        
        wt <- wilcox.test(group_1, group_2, conf.int = TRUE, conf.level = 0.999)
        
        tibble(
          Framework_1 = pair[1],
          Framework_2 = pair[2],
          W_Statistic = wt$statistic,
          P_Value = wt$p.value
        )
      })
    
    pairwise_results %>%
      mutate(
        Site = unique(df$Site),
        P_Adjusted = p.adjust(P_Value, method = "holm")
      ) %>%
      relocate(Site, Framework_1, Framework_2, W_Statistic, P_Value, P_Adjusted)
  })

if(!dir.exists("output")) dir.create("output")
iwalk(results_list, ~ write_csv(.x, file.path("output", paste0(.y, ".csv"))))

print(results_list)

