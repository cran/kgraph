#' A dictionary for the df_pval object
#' 
#' @description Dataframe with columns id (for the phenotype or SNP identifier),
#'              desc (textual description), group, and color
#' @name df_pval_dict
#' @docType data
#' @usage data("df_pval_dict")
#' @format A dataframe with 333 rows and 4 columns.
#' @details 
#' Row IDs correspond to the identifiers found in columns concept1 and concept2
#' of the df_pval object.
#' 
#' @examples
#' data('df_pval')
#' data('df_pval_dict')
#'
#' kg_obj = build_kgraph(c('EFO_0007623', 'EFO_0007624'), df_pval, df_pval_dict)              
#' 
NULL





