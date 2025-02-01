#' A dataset containing GWAS p-values
#' 
#' @description This dataframe provides association scores between SNPs and
#'              mantal health-related phenotypes.
#' @name df_pval
#' @docType data
#' @usage data("df_pval")
#' @format A dataframe with 364 rows and 3 columns
#' @details 
#' Each row defines an association between a SNP and a phenotype. Downloaded
#' from GWAS Catalog at https://www.ebi.ac.uk/gwas/efotraits/EFO_0007623.
#' 
#' @examples
#' \dontrun{
#' data('df_pval')
#'
#' kg_obj = build_kgraph('EFO_0007623', df_pval)              
#' }
#'
NULL




