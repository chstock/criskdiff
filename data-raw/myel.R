## Code to prepare `myel` dataset

mat <- read.table(
  file = "https://sites.williams.edu/bklingen/files/2013/06/myel.txt",
  header = TRUE
) |>
  as.matrix()

myel <- convert_matrix_to_array(mat, group_names = c("group1", "group2"))

usethis::use_data(myel, overwrite = TRUE)
