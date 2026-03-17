csv_files <- list.files(pattern = "\\.csv$")
for (file in csv_files) {
  df_name <- tools::file_path_sans_ext(file)
  assign(df_name, read.csv(file))
}


# min samples = 4 = b04_Ras_Suwaiyil

###