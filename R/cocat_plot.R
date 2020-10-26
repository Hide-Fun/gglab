#' Concatenate plots.
#'
#' Concatenate plots in one column. This makes more powerful by using with purrr::map function.
#' @param .obj data.frame
#' @export
cocat_plot = function(.obj) {
  concat_plot <- vector(mode = "list", length = length(.obj))
  for(i in 1:length(.obj)) {
    if(i == 1) {
      concat_plot[[i]] <- .obj[[i]]
    } else {
      concat_plot[[i]] <- concat_plot[[i - 1]] +.obj[[i]]
    }
  }
  return(concat_plot[[length(.obj)]])
}
