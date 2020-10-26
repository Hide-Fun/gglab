#' Make simple lm model equation.
#'
#' Make simple lm model equation as character.
#' @param .resp character, respose variable.
#' @param .intrspt intersept
#' @param .expl explanatory variable.
#' @param .parse parse
#' @param .digit digit
lm_str = function(.resp = "y", .intrspt = NULL, .expl = NULL , .parse = T, .digits = 2) {
  if(.parse == T) {
    .intrspt <- as.character(round(.intrspt, digits = .digits))
    .expl <- as.character(round(.expl, digits = .digits))
  }
  mod <- glue::glue("{.resp} = {.intrspt} + {.expl}x")
  if (stringr::str_detect(mod, "\\+ -")) {
    mod <- stringr::str_replace_all(mod, "\\+ -", "- ")
  }
  return(mod)
}
