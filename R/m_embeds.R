#' A dataset containing medical word embeddings
#' 
#' @description The embedding matrix has been fitted using Glove word
#'              embeddings on 1,700 open-access publications related to
#'              mental health.
#' @name m_embeds
#' @docType data
#' @usage data("m_embeds")
#' @format A matrix with 1122 rows and 100 columns.
#' @details 
#' Each row is the embedding vector of a CUI in 100 Glove dimensions.
#' 
#' @examples
#' \dontrun{
#' data('m_embeds')
#'
#' fit_kg = fit_embeds_kg(m_embeds, 'cosine')
#' }
#' 
NULL



