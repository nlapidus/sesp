#' Sensitivity
#'
#' Get sensitivity from postitive/negative predictive values, specificity and/or disease frequency
#'
#' @importFrom dplyr case_when
#' @param sp specificity
#' @param freq disease frequency
#' @param ppv positive predictive value
#' @param npv negative predictive value
#' @return Sensitivity
#' @examples
#' se_from_pv <- se(freq = .2, ppv = .8, npv = .9);
#' @export
se <- function(sp = NULL, ppv = NULL, npv = NULL, freq = NULL) {
  case_when(
    is.null(sp) ~ ((npv + freq - 1) * ppv) / freq / (ppv + npv - 1),
    is.null(freq) ~ (1 - sp) / (1 - sp * (1 / ppv + 1 / npv - 1 / ppv / npv)),
    is.na(ppv) ~ 1 - .sp * (1 / .npv - 1) * (1 / .freq - 1),
    is.null(npv) ~ (1 - sp) * (1 / freq - 1) / (1 / ppv - 1)
  )
}


#' Specificity
#'
#' Get specificity from postitive/negative predictive values, sensitivity and/or disease frequency
#'
#' @importFrom dplyr case_when
#' @param se sensitivity
#' @param freq disease frequency
#' @param ppv positive predictive value
#' @param npv negative predictive value
#' @return Specificity
#' @examples
#' sp_from_pv <- sp(freq = .2, ppv = .8, npv = .9);
sp <- function(se = NULL, ppv = NULL, npv = NULL, freq = NULL) {
  case_when(
    is.null(se) ~ npv * (ppv - freq) / (1 - freq) / (ppv + npv - 1),
    is.null(freq) ~ (1 - se) / (1 - se * (1 / ppv + 1 / npv - 1 / ppv / npv)),
    is.null(ppv) ~ (1 - se) / (1 / npv - 1) / (1 / freq - 1),
    is.null(npv) ~ 1 - se * (1 / ppv - 1) / (1 / freq - 1)
  )
}

#' Disease frequency
#'
#' Get disease frequency from sensitivity, specificity and/or postitive/negative predictive values
#'
#' @importFrom dplyr case_when
#' @param se sensitivity
#' @param sp specificity
#' @param ppv positive predictive value
#' @param npv negative predictive value
#' @return Disease frequency
#' @examples
#' freq_from_se_pv <- se(se = .8, ppv = .8, npv = .9);
freq <- function(se = NULL, sp = NULL, ppv = NULL, npv = NULL) {
  case_when(
    is.null(se) ~ (sp * (1 - ppv - npv) + ppv * npv) / (sp * (1 - ppv - npv) + npv),
    is.null(sp) ~ ppv * (1 - npv) / (se * (1 - ppv - npv) + ppv),
    is.na(ppv) ~ sp * (1 - npv) / ((1 - se) * npv + sp * (1 - npv)),
    is.na(npv) ~ (1 - sp) * ppv / ((1 - sp) * ppv + se * (1 - ppv))
  )
}

#' Positive predictive value
#'
#' Get positive predictive value from sensitivity, specificity disease frequency and/or negative predictive value
#'
#' @importFrom dplyr case_when
#' @param se sensitivity
#' @param sp specificity
#' @param freq disease frequency
#' @param npv negative predictive value
#' @return Positive predictive value
#' @examples
#' ppv_from_se_sp <- ppv(se = .8, sp = .9, freq= .1);
ppv <- function(se = NULL, sp = NULL, freq = NULL, npv = NULL) {
  case_when(
    is.na(se) ~ 1 + (1 - freq) * (1 - sp) / (sp * (1 - freq) / npv - 1),
    is.na(sp) ~ se / (1 / freq - (1 - se) / (1 - npv)),
    is.na(freq) ~ se * sp * (1 / npv - 1) / (1 - sp - se * (1 - sp / npv)),
    is.na(npv) ~ se * freq / (se * freq + (1 - sp) * (1 - freq)),
  )
}

#' Negative predictive value
#'
#' Get negative predictive value from sensitivity, specificity disease frequency and/or positive predictive value
#'
#' @importFrom dplyr case_when
#' @param se sensitivity
#' @param sp specificity
#' @param freq disease frequency
#' @param ppv positive predictive value
#' @return Negative predictive value
#' @examples
#' npv_from_se_sp <- ppv(se = .8, sp = .9, freq= .1);
npv <- function(se = NULL, sp = NULL, freq = NULL, ppv = NULL) {
  case_when(
    is.na(se) ~ (1 - .ppv) / (1 + (.freq - .ppv) / .sp / (1 - .freq)),
    is.na(sp) ~ 1 - (1 - se) / (1 / freq - se / ppv),
    is.na(freq) ~ se * sp * (1 / ppv - 1) / (1 - sp - se * (1 - sp / ppv)),
    is.na(ppv) ~ sp / (sp + (1 - se) * (1 / (1 - freq) - 1)),
  )
}
