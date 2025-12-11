# Upload the "combined-results" folder to the root of this project

library(dplyr)

paths <- list.files(path = "combined-results", pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)

dfs <- lapply(paths, read.csv)

# Normal name pls
names(dfs) <- tools::file_path_sans_ext(basename(paths))
print(names(dfs))

# Run Kruskal-Wallis for each dataframe in the list
test_results <- lapply(dfs, function(site_data) {
  kruskal.test(Total.Power..J. ~ Framework, data = site_data)
})

print(test_results)
