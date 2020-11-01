#' fixlmit
#'
#' fixlmit
#' @param .plot ggplot object.
#' @param .xlim xlim
#' @param .ylim ylim
#' @param .ratio aspect ratio
#' @export
fixlmit = function(.plot, .xlim, .ylim, .ratio) {
  stund <- abs((.xlim[[1]] - .xlim[[2]])/(.ylim[[1]] - .ylim[[2]]))
  rlt <- .plot + coord_fixed(ratio = stund*.ratio)
  return(rlt)
}
