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

# Calc CV
all_data <- all_data %>%
  mutate(
    CV = `Total.Power.SD..J.` / `Average.Total.Power..J.`
  )

max_cv_row <- all_data %>% 
  arrange(desc(CV)) %>%
  head(1)

print(max_cv_row)

max_sd <- max_cv_row %>%
  pull(Total.Power.SD..J.)

# Cochran's (1.96 for p=0.05 2.58 for p=0.01)
iterations <- ceiling(((3.35*max_sd)/0.005)^2)

print(iterations)
