
merge_kgraphs = function(l_graphs, df_dict,
                         df_sup_nodes = NULL,
                         display_val_str = '\nCosine similarity: ',
                         str_other = 'Other') {

  # when a node is from a one-indiv group for one pheno (-> assigned to other)
  # and from a multiple indiv group for another (-> group retained)
  # -> if a node has a link to group != other, replace link with other by group
  # also replace node attrib
  # maybe best to add groups after merge

  df_links = do.call(rbind, lapply(l_graphs, `[[`, 'df_links'))
  df_nodes = do.call(rbind, lapply(l_graphs, `[[`, 'df_nodes'))

  if (!is.null(df_dict$group)) {
    l_graph = merge_kgraphs_groups(df_links, df_nodes, df_dict, str_other,
				   names(l_graphs))
    df_links = l_graph$df_links
    df_nodes = l_graph$df_nodes
  }


  df_links %<>% dplyr::group_by(from, to) %>%
      dplyr::summarize(weight = max(weight))

  df_nodes %<>% dplyr::group_by(id) %>%

    dplyr::summarize(weight = max(weight),

      display_val = ifelse(all(is.na(display_val)), NA,
        paste0(display_val, ' (',
          df_nodes$desc[match(selected_concept, df_nodes$id)], ')',
          collapse = display_val_str)),

      desc = desc[1], 
      clusters = ifelse(length(unique(clusters)) > 1,
        subset(clusters, clusters != str_other), clusters))

  df_nodes$weight[df_nodes$id %in% names(l_graphs)] = max(df_nodes$weight)

  if (!is.null(df_sup_nodes)) {
      l_graph = merge_kgraphs_sup_nodes(df_nodes, df_sup_nodes, df_links,
                                        str_other)
  } else {
      l_graph = list(df_nodes = df_nodes, df_links = df_links)
  }
  
  # NOTE may need to recompute group weights for fixed nodes/links
  # although rare edge cases

  l_graph %$% check_kgraph(df_nodes, df_links)
}

merge_kgraphs_sup_nodes = function(df_nodes, df_sup_nodes, df_links, str_other) {

  wts = diff(range(df_nodes$weight)) / c(5, 4.5, 3.5, 2.5)
  phase_idx = match(df_nodes$id, df_sup_nodes$drug_name)
  drug_phase = df_sup_nodes$max_phase[na.omit(phase_idx)]

  df_nodes$weight[!is.na(phase_idx)] = wts[drug_phase]

  # in case of reset singles
  nodes_other = df_links$to == str_other
  df_sup_nodes %<>% subset(id %in% df_links$to)

  grp_idxs = match(df_links$from[nodes_other], df_sup_nodes$drug_name)
  grps = df_sup_nodes$group[grp_idxs]

  grps[!grps %in% df_links$from] = str_other
  df_links$to[nodes_other] = grps
  
  df_links %<>% dplyr::group_by(from, to) %>%
      dplyr::summarize(weight = max(weight))

  ## reduce drug pheno edge (not great, messes things up)
  # drug_pheno_edge = df_links$from %in% c(names(l_graphs), df_sup_nodes$drug_name) &
  #   df_links$to %in% c(names(l_graphs), df_sup_nodes$drug_name)
  # df_links$weight[drug_pheno_edge] %<>% `/`(3)

  list(df_links = df_links, df_nodes = df_nodes)
}


check_kgraph = function(df_nodes, df_links) {

  # check dims
  if (nrow(df_nodes) != length(unique(unlist(df_links[1:2])))) {

    id_diff = setdiff(df_nodes$id, unique(unlist(df_links[1:2]))) %>%
	    paste(collapse = ', ')

    warning(paste('Number of nodes different than unique edges elements.',
		  'Id differences:', id_diff, sep = '\n'))
  }

  if (any(is.na(df_nodes$id))) warning('Node ids have NAs')

  list(df_links = df_links, df_nodes = df_nodes)
}

# having rm_single_groups TRUE can make the merge tricky
# consider the case where in the first graph a node is single and set to other,
## but in second graph the group exists
# a more tricky case is when two nodes of the same group are set to other in
## each graph -> need to add nodes (cf. "if (length(new_nodes))")
# finally, consider the possibility that the Other node has to be removed
merge_kgraphs_groups = function(df_links, df_nodes, df_dict, str_other,
			       	            target_names) {

  nodes_other = df_links$from == str_other
  grp_idxs = match(df_links$to[nodes_other], df_dict$id)
  df_links$from[nodes_other] = df_dict$group[grp_idxs]
 
  single_grps = names(which(table(df_links$from) == 1))
  reset_other = df_links$from[nodes_other] %in% single_grps
  df_links$from[nodes_other][reset_other] = str_other

  if (!any(reset_other)) {
    df_nodes %<>% subset(id != str_other)
  }

  df_nodes %<>% merge_groups_add_nodes(df_links, target_names)
 
  fix_links = df_links$to[nodes_other][!reset_other]
  links_idxs = match(fix_links, df_nodes$id)

  links_dict_idxs = match(fix_links, df_dict$id)
  df_nodes$clusters[links_idxs] = df_dict$color[links_dict_idxs]
 
  merge_groups_build_wts(df_nodes, df_links, df_dict, target_names)
}

merge_groups_add_nodes = function(df_nodes, df_links, target_names) {
 
  new_nodes = setdiff(unique(unlist(df_links[1:2])), unique(df_nodes$id))

  if (length(new_nodes)) {

    df_new_nodes = data.frame(id = new_nodes, desc = new_nodes,
                              clusters = 'Groups',
                              display_val = NA,
                              selected_concept = target_names[1])

    df_new_nodes$weight = subset(df_links, from %in% new_nodes) %>%
      dplyr::group_by(from) %>%
      dplyr::summarize(weight = max(weight)) %$% weight

    df_nodes %<>% rbind(df_new_nodes)
  }

  df_nodes
}

merge_groups_build_wts = function(df_nodes, df_links, df_dict, target_names) {

  df_grp_wts = subset(df_links,
                      from %in% target_names & to %in% df_dict$id)
  df_grp_wts = df_grp_wts %>% dplyr::group_by(to) %>%
      dplyr::summarize(weight = max(weight))

  df_grp_wts$group = df_dict$group[match(df_grp_wts$to, df_dict$id)]
  df_grp_wts = df_grp_wts %>% dplyr::group_by(group) %>%
      dplyr::summarize(weight = max(weight))
 
 
  grp_idxs = match(df_links$from, df_grp_wts$group)
  df_links$weight[!is.na(grp_idxs)] = df_grp_wts$weight[na.omit(grp_idxs)]

  grp_idxs = match(df_nodes$id, df_grp_wts$group)
  wts = df_grp_wts$weight[na.omit(grp_idxs)] #- (max(df_nodes$weight) / 5)
  df_nodes$weight[!is.na(grp_idxs)] = wts

  list(df_links = df_links, df_nodes = df_nodes)
}
