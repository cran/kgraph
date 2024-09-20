
#' Wrapper to build a sgraph object fromk a kgraph object
#'
#' @param l_graph List of df_nodes and df_links dataframes
#' @param colors_mapping Output of get_colors_map
#' @param label_attrs Column name of df_nodes that will be displayed
#' @param igraph Intermediary igraph object, if already computed
#' @param ... Passed to sgraph::sgraph_clusters
#'
#' @return Sgraph htmlwidget object
#'
#' @export
get_sgraph = function(l_graph, colors_mapping = NULL,
                      label_attrs = 'label', igraph = NULL, ...) {

  if (is.null(igraph)) igraph = sgraph::l_graph_to_igraph(l_graph)

  sgraph = sgraph::sgraph_clusters(igraph, node_size = 'weight',
                                    label = label_attrs,
                                    color_map = colors_mapping,
                                    layout = igraph::layout_with_kk(igraph),
                                    ...)
  
  sgraph %<>% sgraph::add_edge_color(one_color = l_graph$df_links$color)
  sgraph %<>% sgraph::add_edge_zindex(zindex = l_graph$df_links$zindex)
  # add listener
}

# deprecated, already in sgraph, keep for ref as slight differences
#   multiline_labels = function(df_nodes, display_val_str = '\nP-value: ',
#                               replace_codes = TRUE) {

#     if (!replace_codes) {
#       df_nodes$label = df_nodes$desc %>% {
#           ifelse(df_nodes$id != .,
#                  paste0(ifelse(grepl('^[0-9]$', .), 'Max. phase: ', 'Label: '),
#                         ., '\n'),
#                  '')
#       }

#       df_nodes$label %<>% paste0('Group: ', df_nodes$clusters)

#     } else {

#       df_nodes$label = df_nodes$desc %>% ifelse(is.na(.), df_nodes$id, .)
#     }

#     val_labels = df_nodes$display_val %>%
#         { ifelse(!is.na(.), paste0(display_val_str, df_nodes$display_val), '') }

#     df_nodes$label %<>% paste0(val_labels)

#     df_nodes$label %<>% gsub(paste0(display_val_str, 'NA [^\n]+(.*)'), '\\1', .)

#     df_nodes
#   }
