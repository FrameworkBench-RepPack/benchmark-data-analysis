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
    
    pairwise_results <- expand.grid(Group1 = groups, Group2 = groups, stringsAsFactors = FALSE) %>%
      filter(Group1 != Group2) %>%
      as_tibble() %>%
      mutate(
        data = map2(Group1, Group2, function(g1, g2) {
          
          group_1_data <- df %>% filter(Framework == g1) %>% pull(`Total Power (J)`)
          group_2_data <- df %>% filter(Framework == g2) %>% pull(`Total Power (J)`)
          
          #Either "greater" or "less"
          wt <- wilcox.test(group_1_data, group_2_data, 
                            alternative = "greater",
                            conf.int = TRUE, 
                            conf.level = 0.999)
          
          tibble(
            # Change to greater: ">" or less: "<"
            Framework_1 = g1,
            Framework_2 = g2,
            Comparison = paste(g1, ">", g2),
            W_Statistic = wt$statistic,
            P_Value = wt$p.value
          )
        })
      ) %>%
      unnest(data) %>%
      select(-Group1, -Group2)
    
    pairwise_results %>%
      mutate(
        Site = unique(df$Site),
        P_Adjusted = p.adjust(P_Value, method = "holm")
      ) %>%
      relocate(Site, Framework_1, Framework_2, Comparison, W_Statistic, P_Value, P_Adjusted)
  })

if(!dir.exists("output")) dir.create("output")
iwalk(results_list, ~ write_csv(.x, file.path("output", paste0(.y, ".csv"))))

print(results_list)

