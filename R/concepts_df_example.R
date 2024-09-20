
#' Reshape multiple traits in example data
#'
#' @param df_pval Data frame of p-values
#'
#' @return Reshaped data frame
reshape_multiple_traits = function(df_pval) {

  multiple_traits = grep(', ', df_pval$concept1)

  l_mult_traits = strsplit(df_pval$concept1[multiple_traits], ', ')

  df_mult = lapply(seq_along(multiple_traits), function(idx) {

      cbind.data.frame(concept1 = l_mult_traits[[idx]],
                       concept2 = df_pval$concept2[multiple_traits[idx]],
                       weight = df_pval$weight[multiple_traits[idx]])

    }) %>% do.call(rbind, .)

  df_pval = rbind(df_pval[-multiple_traits, ], df_mult)

  df_pval = df_pval[order(df_pval$weight, decreasing = TRUE), ]
  df_pval %<>% subset(!duplicated(paste0(concept1, concept2)))
}


#' Reshape multiple traits in example data dictionary
#'
#' @param df_dict Data frame of p-values dictionary
#'
#' @return Reshaped data frame
reshape_multiple_traits_dict = function(df_dict) {

  multiple_traits = grep(', ', df_dict$id)

  l_mult_traits = strsplit(df_dict$id[multiple_traits], ', ')
  l_mult_traits_desc = strsplit(df_dict$desc[multiple_traits], ', ')

  df_mult = lapply(seq_along(multiple_traits), function(idx) {
                       cbind.data.frame(id = l_mult_traits[[idx]],
                             desc = l_mult_traits_desc[[idx]],
                             group = l_mult_traits_desc[[idx]]) 
                 }) %>% do.call(rbind, .)

  df_dict = rbind(df_dict[-multiple_traits, ], df_mult)

  df_dict %<>% subset(!duplicated(id))
}

