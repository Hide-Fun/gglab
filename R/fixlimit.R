#' fixlmit
#'
#' fixlmit
#' @param .plot ggplot object.
#' @param .xlim xlim
#' @param .ylim ylim
#' @param .ratio aspect ratio
#' @export
fixlmit = function(.plot, .xlim, .ylim, .ratio) {
  stund <- (.xlim[2] - .xlim[1])/(.ylim[2] - .ylim[1])
  rlt <- .plot + coord_fixed(ratio = stund*.ratio)
  return(rlt)
}
