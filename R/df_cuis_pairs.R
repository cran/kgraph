#' A dataset containing CUIs pairs
#' 
#' @description The dataframe provides clinician-curated pairs of related of
#'              medical concepts, useful to evaluate the performance of a
#'              machine learning model. It's an extract of the PrimeKG database
#'              (see vignette for URL).
#' @name df_cuis_pairs
#' @docType data
#' @usage data("df_cuis_pairs")
#' @format A dataframe with 2358 rows and 4 columns.
#' @details 
#' Each row defines a relationship between two CUIs, along with their textual
#' descriptions.
#' 
#' @examples
#' data('m_embeds')
#' data('df_cuis_pairs')
#'
#' fit_kg = fit_embeds_kg(m_embeds, 'cosine', df_pairs = df_cuis_pairs[c(1, 3)])
#' pROC::plot.roc(fit_kg$roc, print.auc = TRUE)
#' 
NULL


