cluster_se <- function(model_result, data, cluster){
  model_variables   <- intersect(colnames(data), c(colnames(model_result$model), cluster))
  model_rows <- rownames(model_result$model) # changed to be able to work with mtcars, not tested with other data
  data <- data[model_rows, model_variables]
  cl <- data[[cluster]]
  M <- length(unique(cl))
  N <- nrow(data)
  K <- model_result$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(model_result), 2, function(x) tapply(x, cl, sum));
  vcovCL <- dfc*sandwich(model_result, meat=crossprod(uj)/N)
}