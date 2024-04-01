do.scan <- function(data) {
  column_types <- sapply(data, function(col) {
    if (is.factor(col) || is.character(col))
      "discrete"
    else if (is.numeric(col))
      "continuous"
    else "other"
  })
  num_discrete_columns <- sum(column_types == "discrete")
  num_continuous_columns <- sum(column_types == "continuous")
  missing_columns <- sum(colSums(is.na(data)) > 0)
  complete_rows <- sum(complete.cases(data))
  missing_observations <- sum(is.na(data))
  num_columns_in_dataset <- ncol(data)
  num_rows_in_dataset <- nrow(data)
  results_data <- data.frame(Metric = c("Discrete Columns", "Continuous Columns",
                                        "Missing Columns", "Complete Rows", "Missing Observations", "Number of Columns",
                                        "Number of Rows"), Count = c(num_discrete_columns, num_continuous_columns,
                                                                     missing_columns, complete_rows, missing_observations, num_columns_in_dataset,
                                                                     num_rows_in_dataset))
  return(results_data)
}

compare <- function(data, clean = TRUE) {
  p <- ggplot(data = data, aes(x = Metric, y = Count, fill = Metric)) +
    geom_bar(stat = 'identity', width = .7) +
    geom_text(aes(label = Count), hjust = -.2, size = 2.7) +
    scale_y_continuous(trans = 'log1p', expand = expansion(mult = c(0, .2))) +
    scale_fill_zata() +
    theme(legend.position = 'none', plot.title.position = 'plot', plot.title = element_text(size = 9),
          axis.text.x = element_blank(), axis.line.x = element_blank(),
          panel.grid.major.x = element_blank()) +
    coord_flip()
  if (clean) {
    p <- p + theme(axis.text.y = element_blank()) + 
      labs(x = NULL, y = NULL, title = 'Dataset Characteristics (Processed)', 
           caption = 'Logarithmic scaled bar')
  } else {
    p <- p + labs(x = NULL, y = NULL, title = 'Dataset Characteristics (Pre-processed)')
  }
  return(p)
}

p_raw <- compare(do.scan(dataraw), clean = FALSE)
p_clean <- compare(do.scan(acled))
