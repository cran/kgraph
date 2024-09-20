
#' Build a knowledge graph from a fit object
#'
#' Computes similarities for nodes of interest on the fly to avoid having to
#' deal with very large similarity matrices when number of features is large.
#'
#' @param selected_concepts Concepts of interest
#' @param m_embeds Embeddings matrix
#' @param fit_kg Fit object
#' @param ... Passed to build_kgraph
#'
#' @return Knowledge graph, list of slots df_nodes and df_links
#'
#' @export
build_kgraph_from_fit = function(selected_concepts, m_embeds, fit_kg, ...) {

  df_projs = if (fit_kg$is_full_projs) {
      fit_kg$df_projs
    } else {
      project_kgraph(m_embeds, selected_concepts,
                     fit_kg$similarity, fit_kg$threshold_projs)
    }

  build_kgraph(selected_concepts, df_projs, ...)
}

#' Build a knowledge graph
#'
#' @param selected_concepts Concepts of interest
#' @param df_weights Data frame with columns concept1, concept2, and weight;
#'                   typically the df_projs slot of the object returned by
#'                   function fit_embeds_to_pairs
#' @param df_dict Dictionary data frame, with columns id (matched to concepts
#'                in df_weights), desc (for labels), color, and optionally
#'                group.
#' @param rm_single_groups Should groups with only one element be removed
#' @param df_sup_nodes Data frame of supplementary nodes (work in progress)
#' @param display_val_digits Number of weight digits to be displayed in labels
#' @param display_val_str String to prefix to the displayed value
#' @param str_other String to use for missing groups
#' @param highlight_mult Highlight nodes connected to multiple nodes of
#'                       interest.
#' @param multiline_labs Use multiline labels (shown when hovered on)
#' @param autoscale Perform scaling with sgraph::scale_graph
#' @param spring_weights Use spring weights (reverts edges weights)
#' @param n_max_edges Threshold on number of edges
#' @param ... Passed to scale_kgraph
#'
#' @return Knowledge graph, list of slots df_nodes and df_links
#'
#' @export
build_kgraph = function(selected_concepts, df_weights, df_dict = NULL,
                        rm_single_groups = TRUE, df_sup_nodes = NULL,
                        display_val_digits = 3,
                        display_val_str = '\nCosine similarity: ',
                        str_other = 'Other',
                        highlight_mult = TRUE, multiline_labs = TRUE,
                        autoscale = TRUE, spring_weights = TRUE,
                        n_max_edges = 1e3, ...) {

  if (n_max_edges > 0) {
    df_weights %<>%
      subset(concept1 %in% selected_concepts |
             concept2 %in% selected_concepts) %>%
      head(n_max_edges)
  }

  uniq_concepts = unique(unlist(df_weights[1:2]))

  if (is.null(df_dict)) {

      df_dict = uniq_concepts %>%
          data.frame(id = ., desc = .)

  } else {

      map_idxs = match(uniq_concepts, df_dict$id)

      if (any(is.na(map_idxs))) {

          message('Concept(s) not found in dictionary')

          df_unknown = uniq_concepts[is.na(map_idxs)] %>%
              data.frame(id = ., desc = .)

          if ('color' %in% names(df_dict)) df_unknown$color = 'black'
          if ('group' %in% names(df_dict)) df_unknown$group = 'Other'

          df_dict %<>% plyr::rbind.fill(df_unknown)
      }
  }

  if (is.null(df_dict$color)) df_dict$color = 'black'

  kgs = selected_concepts %>% setNames(., .) %>%
    lapply(kgraph_single_node, df_weights, df_dict,
           rm_single_groups = rm_single_groups,
           add_sup_nodes = !is.null(df_sup_nodes),
           display_val_digits = display_val_digits,
           str_other = str_other)

 
  kg = merge_kgraphs(kgs, df_dict, df_sup_nodes = df_sup_nodes,
                     display_val_str = display_val_str, str_other = str_other)

  if (highlight_mult) {
    kg$df_links = kg %$%
        sgraph::highlight_multiple_connected(df_links, selected_concepts)
  }

  if (multiline_labs) {
    kg$df_nodes %<>% sgraph::multiline_labels(display_val_str)
  }

  if (autoscale) {
    #kg$df_links$weight %<>% sgraph::scale_graph(...)
    #kg$df_nodes$weight %<>% sgraph::scale_graph(...)
    kg %<>% scale_kgraph(...)
  }

  if (spring_weights) kg$df_links %<>% sgraph::convert_to_spring_weights()

  kg 
}

scale_kgraph = function(kg_obj, links_upper_bound = 1,
                        links_lower_bound = 0, exp_scale = 7.5, ...) {

  kg_obj$df_nodes$weight %<>%
    sgraph::scale_graph(exp_scale = exp_scale, ...)
  kg_obj$df_links$weight %<>%
    sgraph::scale_graph(upper_bound_mult = links_upper_bound,
                        lower_bound_const = links_lower_bound, ...)

  kg_obj
}

project_kgraph = function(m_embeds, target_cuis, similarity, threshold,
                          verbose = FALSE) {

  similarity_fun = switch(similarity, cosine = cosine_simi,
                          get(similarity))

  l_df_projs = lapply(target_cuis, function(target_cui) {

       if (verbose) message(paste('Projecting', target_cui))

       projs = similarity_fun(m_embeds[target_cui, , drop = FALSE], m_embeds)

       df_projs = data.frame(concept1 = target_cui,
                             concept2 = colnames(projs),
                             weight = as.vector(projs))

       subset(df_projs, concept1 != concept2 & weight > threshold)
    })
  
  df_projs = do.call(rbind, l_df_projs)
  df_projs %<>% order_dataframe
  df_projs = df_projs[order(df_projs$weight, decreasing = TRUE), ]

  df_projs %<>% subset(!duplicated(paste0(concept1, concept2)))
}

kgraph_single_node = function(selected_concept, df_weights, df_dict,
                              rm_single_groups = TRUE, add_sup_nodes = FALSE,
                              display_val_digits = 3, str_other = 'Other',
                              verbose = FALSE) {

  if (verbose) message(paste('Building', selected_concept))

  df_weights %<>% subset(concept1 == selected_concept |
                         concept2 == selected_concept)

  if (nrow(df_weights) == 0) {
    message(paste('NOTE: selected concept', selected_concept,
                  'not found in df_weights'))
    return()
  }

  df_weights %<>% order_dataframe(relevant_pattern = selected_concept)

  df_merge = merge(df_weights, df_dict, by.x = 'concept2', by.y = 'id')

  # link all nodes to central node
  first_order_links = df_merge[c('concept1', 'concept2', 'weight')] %>%
      setNames(c('from', 'to', 'weight'))

  # link nodes to groups
  if (!is.null(df_merge$group)) {

    l_df_merge = merge_per_groups(df_merge, rm_single_groups, str_other)

    second_order_links = switch_clusters_type(l_df_merge, selected_concept,
                                              type = 2) 
  } else {

    l_df_merge = list(df_merge)
    second_order_links = NULL
  }

  # link supplementary nodes
  if (add_sup_nodes) {

    df_drugs = subset(df_merge, !is.na(drug_name))
    l_df_drugs = split(df_drugs, df_drugs$drug_name)

    third_order_links = switch_clusters_type(l_df_drugs, selected_concept,
                                             type = 3, from_col = 'drug_name')

    third_order_groups = switch_clusters_type(l_df_drugs, selected_concept,
                                              type = 3, from_col = 'drug_name',
                                              to_col = 'group')

    # in case of rm_single_groups
    fix_rm_single_grps = !third_order_groups$to %in% second_order_links$from
    third_order_groups$to[fix_rm_single_grps] = str_other
    third_order_links %<>% rbind(third_order_groups) %>% unique

    wts = diff(range(df_merge$weight)) / c(5, 4, 3, 2)
    phase_idx = match(third_order_links$from, df_drugs$drug_name)
    drug_phase = df_drugs$max_phase[phase_idx]
    third_order_links$weight = wts[drug_phase]

  } else {
    third_order_links = NULL
  }

  df_nodes = build_kgraph_nodes(first_order_links, second_order_links,
                                third_order_links, df_weights, df_dict,
                                l_df_merge, selected_concept,
                                display_val_digits) 

  df_links = rbind(first_order_links, second_order_links, third_order_links)

  check_kgraph(df_nodes, df_links)
}

# TODO
kgraph_add_sup_nodes = function() {
}


build_kgraph_nodes = function(first_order_links, second_order_links,
                              third_order_links, df_weights, df_dict,
                              l_df_merge, selected_concept,
                              display_val_digits) {

  df_root = df_dict[c('id', 'desc', 'color')] %>%
      subset(id %in% selected_concept)

  df_merge = do.call(rbind, l_df_merge)
  leaf_idxs = match(first_order_links$to, df_merge$concept2)
  df_leafs = df_merge[leaf_idxs, c('concept2', 'desc', 'color')]
  colnames(df_leafs)[1] <- 'id'
  
  if (!is.null(second_order_links)) {

    group_idxs = match(unique(second_order_links$from), df_dict$group)

    df_groups = unique(second_order_links$from) %>%
        cbind.data.frame(id = ., desc = ., color = df_dict$color[group_idxs])

    df_groups$color = 'Groups'

  } else {
    df_groups = NULL
  }
  
  if (!is.null(third_order_links)) {

    uniq_drugs = unique(third_order_links$from)

    drugs_idxs = match(uniq_drugs, df_merge$drug_name)
    max_phase = df_merge$max_phase[drugs_idxs]

    df_drugs = cbind.data.frame(id = uniq_drugs, desc = max_phase,
              color = df_merge$color[drugs_idxs])

    df_drugs$color = 'Drugs'

  } else {
    df_drugs = NULL
  }

  df_nodes = rbind(df_root, df_leafs, df_groups, df_drugs) %>%
      setNames(c('id', 'desc', 'clusters'))

  display_val_idx = match(df_nodes$id, df_weights$concept2)
  df_nodes$display_val <- round(df_weights$weight[display_val_idx],
                                display_val_digits)
  df_nodes$display_val[1] = NA

  wts = first_order_links$weight
  
  if (!is.null(df_groups)) {
    wts %<>% c(second_order_links$weight[!duplicated(second_order_links$from)])
  }
  
  if (!is.null(df_drugs)) {
    wts %<>% c(third_order_links$weight[!duplicated(third_order_links$from)])
  }

  # add selected_concept as first weight
  df_nodes$weight = wts %>% c(max(.) + (diff(range(.)) / 4), .)
  df_nodes$selected_concept = selected_concept

  ## reduce group edges
  # group_edges = df_links$from %in% subset(df_nodes, clusters == 'Groups')$id
  # df_links$weight[group_edges] %<>% `/`(2)

  df_nodes
}

get_color_map = function(color_levels) {

  palette = RColorBrewer::brewer.pal(length(color_levels), 'Paired')

  sgraph::get_color_map(color_levels, palette)
}
