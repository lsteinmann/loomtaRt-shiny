df <- data.frame(
  identifier = paste0("LW_nr_", 1:1000),
  weight = round(abs(rnorm(n = 1000, mean = 90, sd = 90)) + 10, digits = 1),
  width = round(abs(rnorm(n = 1000, mean = 2.8, sd = 1)), digits = 1),
  findspot = sample(c("Area_1", "Area_2", "Area_3", "unknown"), size = 1000, replace = TRUE),
  category = sample(c("lentoid", "pyramidal", "diskoid"), size = 1000, replace = TRUE)
)

write.csv(df, "data/demo_data.csv", row.names = FALSE)
