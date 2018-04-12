retrieve_coefficient(estimation, coef_name)
{
  coef_matrix <- estimation@model$coef
  counter <- c(1:length(coef_matrix))
  coef_vector <- apply(counter, MARGIN = 1, FUN = function(i)
                {
                  return(coef_matrix[[i]][[2]][[name,1]])
                })
}