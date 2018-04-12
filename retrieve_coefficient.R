retrieve_coefficient <- function(estimation, coef_name)
{
  coef_matrix <- estimation@model$coef
  counter <- as.matrix(1:length(coef_matrix))
  coef_vector <- apply(counter, MARGIN = 1, FUN = function(i)
                {
                  return(coef_matrix[[i]][[2]][[coef_name,1]])
                })
  return(coef_vector)
}