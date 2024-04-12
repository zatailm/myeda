

# export all data frame into file -------------------------------------------------------------

all_objects <- ls()

data_frames <- Filter(function(x) is.data.frame(get(x)), all_objects)

save_data <- function(data_list) {
  for (data_name in data_list) {
    data_object <- get(data_name) 
    file_name <- paste0("data/output_data/", data_name, ".rds")
    saveRDS(data_object, file_name)
  }
}

save_data(data_frames)

# export all functions into single file -------------------------------------------------------

functions <- all_objects[sapply(all_objects, function(x) typeof(get(x))) == "closure"]

file_path <- "script/functions.R"
file_conn <- file(file_path, open = "w")

for (func_name in functions) {
  func_def <- deparse(get(func_name))
  writeLines(paste("# Fungsi:", func_name), file_conn)
  writeLines(func_def, file_conn)
}

close(file_conn)


# export plot ---------------------------------------------------------------------------------

png(filename = "barevfat.png", width = 7.48, height = 2.667, units = "in", res = 300)
print(p_compare_reg)
dev.off()

