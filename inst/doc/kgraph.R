## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
  library(kgraph)
  data('df_pval')
  head(df_pval)

## -----------------------------------------------------------------------------
  kg_obj = build_kgraph('EFO_0007623', df_pval)              

## -----------------------------------------------------------------------------
  ig_obj = sgraph::l_graph_to_igraph(kg_obj)

## -----------------------------------------------------------------------------
  sg_obj = sgraph::sgraph_clusters(ig_obj, node_size = 'weight',
                                   label = 'label',
                                   layout = igraph::layout_with_kk(ig_obj))

## -----------------------------------------------------------------------------
  sg_obj

## -----------------------------------------------------------------------------
  data('df_pval_dict')
  head(df_pval_dict)

## -----------------------------------------------------------------------------
  kg_obj = build_kgraph('EFO_0007623', df_pval, df_dict = df_pval_dict)              

## -----------------------------------------------------------------------------
  sg_obj = get_sgraph(kg_obj)
  sg_obj

## -----------------------------------------------------------------------------
  kg_obj = build_kgraph(c('EFO_0007623', 'EFO_0007624'), df_pval, df_pval_dict)

  sg_obj = get_sgraph(kg_obj)
  sg_obj

## -----------------------------------------------------------------------------
  data('m_embeds')
  dim(m_embeds)

## -----------------------------------------------------------------------------
  fit_kg = fit_embeds_kg(m_embeds, 'cosine', threshold_projs = 0.9)
  fit_kg$threshold_projs

## -----------------------------------------------------------------------------
  data('df_embeds_dict')
  head(df_embeds_dict)

## -----------------------------------------------------------------------------
  target_nodes_idxs = grep('suicide', df_embeds_dict$desc) %>% head(2)
  target_nodes = df_embeds_dict$id[target_nodes_idxs]

  kg_obj = build_kgraph_from_fit(target_nodes, m_embeds, fit_kg,
                                 df_dict = df_embeds_dict)

  sg_obj = get_sgraph(kg_obj)
  sg_obj

## -----------------------------------------------------------------------------
  data('df_cuis_pairs')

  fit_kg = fit_embeds_kg(m_embeds, 'cosine', df_pairs = df_cuis_pairs[c(1, 3)])

  pROC::plot.roc(fit_kg$roc, print.auc = TRUE)

