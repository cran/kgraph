#' A dictionary for the m_embeds object
#' 
#' @description Dataframe with columns id (for the CUI), desc (textual
#'              description), group and color (higher level groups)
#' @name df_embeds_dict
#' @docType data
#' @usage data("df_embeds_dict")
#' @format A dataframe with 1118 rows and 4 columns.
#' @details 
#' Each row corresponds to one rowname of m_embeds.
#' 
#' @examples
#' \dontrun{
#' data('m_embeds')
#' data('df_embeds_dict')
#'
#' fit_kg = fit_embeds_kg(m_embeds, 'cosine')
#' target_nodes_idxs = grep('suicide', df_embeds_dict$desc) %>% head(2)
#' target_nodes = df_embeds_dict$id[target_nodes_idxs]
#'
#' kg_obj = build_kgraph_from_fit(target_nodes, m_embeds, fit_kg,
#'                                df_dict = df_embeds_dict)
#' }
#' 
NULL




