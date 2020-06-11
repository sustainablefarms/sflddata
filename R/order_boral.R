# create function to reorder rows of a coefsplot
# move this to boralis soon
order_boral <- function(
  data,  # data.frame from boral_coefs
  x # a name of a covariate in data
){
  data_list <- split(data, data$covname)
  data_order <- order(data_list[[x]]$x, decreasing = TRUE)
  data_ordered <- lapply(data_list, function(a, order){
    result <- a[order, ]
    result$labels <- as.character(result$labels)
    result$labels <- factor(
      seq_len(nrow(result)),
      levels = seq_len(nrow(result)),
      labels = result$labels
    )
    return(result)
  }, order = data_order)
  data_final <- do.call(rbind, data_ordered)
  return(data_final)
}