
merge_per_groups = function(df_merge, rm_single_groups, str_other) {

  l_df_merge = split(df_merge, df_merge$group)
  if (!rm_single_groups) return(l_df_merge)

  # use which in case there is only one
  single_nodes = which(sapply(l_df_merge, nrow) == 1)
  if (length(single_nodes) == 0) return(l_df_merge)

  # if the "other" group didn't exist yet
  if (is.null(l_df_merge[[str_other]])) single_nodes %<>% c(FALSE)

  l_df_merge[[str_other]] %<>% rbind(do.call(rbind, l_df_merge[single_nodes]))
  l_df_merge[[str_other]]$group = str_other

  l_df_merge[-single_nodes]
}




# three different ways of organizing groups
# could improve type 3 by removing group-target links when multiple
## targets matched by children
# but type 2 probably better by default
# could improve type 2 by shortening outliers
switch_clusters_type = function(l_df_merge, selected_concept, type = 2,
                                from_col = 'group', to_col = 'concept2') {

  # either get same group weighting for all group nodes
  if (type == 1) {

    df_merge = do.call(rbind, l_df_merge)
    second_order_links = df_merge[c(from_col, to_col)] %>%
      setNames(c('from', 'to'))

    second_order_links$weight = median(df_merge$weight)

  # or get group node weights per group
  } else {

    df_merge = do.call(rbind, l_df_merge)
    med_weights = median(df_merge$weight)

    second_order_links = lapply(l_df_merge, links_by_cluster, to_col,
                                med_weights) %>%
      reshape2::melt(id = to_col) %>%
      setNames(c('to', 'var', 'weight', 'from'))

    second_order_links = second_order_links[c(4, 1, 3)]

    # add an edge from group to central node (useful for small distant groups)
    if (type == 3) {

      third_order_links = second_order_links
      third_order_links$to = selected_concept
      third_order_links %<>% unique
      second_order_links %<>% rbind(third_order_links)
    }
  }

  second_order_links
}



links_by_cluster = function(df_merge_grp, to_col = 'concept2', med_wts) {

    #data.frame(df_merge_grp[[to_col]], med_wts) %>%
      ##
    data.frame(df_merge_grp[[to_col]], max(df_merge_grp$weight)) %>%
      ##
   # data.frame(df_merge_grp[[to_col]],
   #    diff(range(df_merge_grp$weight)) / 1.5) %>%
      ##
      setNames(c(to_col, 'weight'))
}

