

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

