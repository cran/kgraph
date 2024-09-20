
# The two main features provided by the kgraph package are building a knowledge graph based on a data frame of concept relationships, be it p-values or cosine similarities, and building the data frame of concept relationships from an embedding matrix, the second feature thus encapsulating the first one. The user can either provide a direct data frame of weighted relationships, as p-values or pre-computed similarities, to the `build_kgraph` function, or provide a data matrix on which the similarities will be directly computed and a threshold will be determined, to the `fit_embeds_kg` function.

# Building a fitted object

## For the `fit_embeds_kg` function operating on the data matrix, presumably an embedding matrix with concepts as rows and dimensions as columns, similarity method may vary and will usually be either the inner product or the cosine similarity, but two other methods are available. Threshold determining will usually be around 5% or 10% false positives (i.e. 0.95 or 0.9 specificity) but may be anywhere between 1% and 50% depending on the structure of the data (if many concepts are similar or not) and how the user wants to visualize the graph.

## Depending on the number of concepts in the data matrix (i.e. number of rows) and the amount of available RAM, it may not be possible to compute all pair-wise similarities in order to after be able to select any node of interest. In that case pair-wise similarities for concepts of interest are computed on the fly and the `fit_embeds_to_kg` is mainly here to determine the threshold. Otherwise the fitted object contains the complete dataframe of weighted relationships.

## This 'on-the-fly' computation is performed by the `project_graph` function, and the `build_kgraph_from_fit` function enables to dispatch automatically the two behaviors. The maximum number of concepts above which 'on-the-fly' computation is performed is determined by the `max_concepts` parameter, by default 1000, which is quite low thus should run easily on modest systems with concurrent applications. Since the number of nodes of interest is rarely above 100, the 'on-the-fly' computation should still be near instantaneous and barely noticeable.

## The threshold determining however can vary depending on the random sampling performed. By default ~20,000 pairs are sampled, and this number can be increased with the `n_notpairs` parameter. Increasing it will increase the computation time of the fitted object (but which is only performed once), and will increase the stability of the threshold and the resulting graph. The fitted object can then be reused to explore different nodes of interest, and is recomputed only when the similarity method or the threshold is changed.


# the underlying build_kgraph function has a few parameters to test, namely:

### single and multiple target nodes

### use of dictionary for additional node information

### use of group information for grouping of nodes (two main types)

#### The two different grouping methods change drastically how data is represented. Both will add groups as non-target nodes but the first kind will add edges from non-target nodes to groups, while the second will also add such edges, but will remove direct edges from targets to non-targets. In the latter case we usually want non-target nodes to be hidden and showed upon click of the group node. In the first case we usually want to use a spring layout, and in the second a force-directed.

#### We also usually want to remove groups if they have only one 'child' non-target node attached, i.e. if only one non-target node is part of the group. In the first case, we usually want to create a 'Other' group that will attach all such single child nodes, while in the second case we want to remove the group node.

#### Managing such single-child groups can prove challenging. I have chosen to implement the build_kgraph by first building independently a knowledge graph for each of the selected nodes, and then to merge them all at once. This is not the fastest way to proceed on the computational aspect, but I believe the implementation is easier that way. It does however prove challenging when taking into account the removal of single child nodes, and the implementation is rather complex. See the code comments for more details on the different edge cases we need to manage.

### To give a quick overview of the problematic, consider that two independent graphs may have an identical non-target node that is in the first case assigned to 'Other' but not in the second one. Multiple such problems arise when considering all the different possible edge cases. We at least want to test this case, and the one in which one group didn't even exist in any of the graphs taken independently.





data('df_cuis_pairs')
df_pairs = df_cuis_pairs
df_pairs = df_pairs[c('umls_id.x', 'umls_id.y')]

data('m_embeds')


select_cuis = intersect(rownames(m_embeds), unique(unlist(df_pairs)))

data('df_embeds_dict')
df_dict = df_embeds_dict
df_dict = subset(df_dict, id %in% select_cuis)

# selected_node = 'PheCode:290'
selected_node = 'C1332979'


####


l_fit_kg = list()

l_fit_kg$inprod = fit_embeds_kg(m_embeds, similarity = 'inprod',
                        threshold_projs = 0.95, n_notpairs = 10)

l_fit_kg$cosine = fit_embeds_kg(m_embeds, similarity = 'cosine',
                        threshold_projs = 0.95, n_notpairs = 10)

l_fit_kg$inprod_full = fit_embeds_kg(m_embeds[select_cuis, ], similarity = 'inprod',
                         threshold_projs = 0.95, n_notpairs = 10)

l_fit_kg$cosine_full = fit_embeds_kg(m_embeds[select_cuis, ], similarity = 'cosine',
                       threshold_projs = 0.95, n_notpairs = 10)

###


l_fit_kg$inprod_pairs = fit_embeds_kg(m_embeds, similarity = 'inprod',
                                      df_pairs = df_pairs,
                                      threshold_projs = 0.95)

l_fit_kg$cosine_pairs = fit_embeds_kg(m_embeds, similarity = 'cosine',
                                      df_pairs = df_pairs,
                                      threshold_projs = 0.95)

l_fit_kg$inprod_full_pairs = fit_embeds_kg(m_embeds[select_cuis, ],
                                           similarity = 'inprod',
                                           df_pairs = df_pairs,
                                           threshold_projs = 0.95)

l_fit_kg$cosine_full_pairs = fit_embeds_kg(m_embeds[select_cuis, ],
                                           similarity = 'cosine',
                                           df_pairs = df_pairs,
                                           threshold_projs = 0.95)


test_fit_embeds_kg = function() {

  expect_false(l_fit_kg$inprod$is_full_projs)

  expect_true(l_fit_kg$inprod_full$is_full_projs)

  expect_false(l_fit_kg$cosine$is_full_projs)

  expect_true(l_fit_kg$cosine_full$is_full_projs)
}
test_that('fit_embeds_kg', test_fit_embeds_kg())

test_fit_embeds_to_pairs = function() {

  expect_false(l_fit_kg$inprod_pairs$is_full_projs)

  expect_true(l_fit_kg$inprod_full_pairs$is_full_projs)

  expect_false(l_fit_kg$cosine_pairs$is_full_projs)

  expect_true(l_fit_kg$cosine_full_pairs$is_full_projs)
}
test_that('fit_embeds_to_pairs', test_fit_embeds_to_pairs())

test_build_kgraph = function() {

  # single node
  df_weights = l_fit_kg$inprod_full$df_projs
  kg_obj = build_kgraph(selected_node, df_weights)
  expect_true(TRUE)

  # with dictionary
  kg_obj = build_kgraph(selected_node, df_weights, df_dict)
  expect_true(TRUE)

  # with groups
  df_weights$group = round(runif(nrow(df_weights), 0, 1))
  kg_obj = build_kgraph(selected_node, df_weights, df_dict)
  expect_true(TRUE)

  # several nodes
  # selected_nodes = c('PheCode:290', 'PheCode:170')
  selected_nodes = c(selected_node, 'C1332206')
  kg_obj = build_kgraph(selected_nodes, df_weights, df_dict)
  expect_true(TRUE)

  # hidden child nodes
  # TODO
  expect_true(TRUE)

  # sup nodes
  ## done in test-sup_nodes.R
  expect_true(TRUE)
}
test_that('build_kgraph', test_build_kgraph())


test_build_kgraph_from_fit = function() {

  lapply(l_fit_kg, function(fit_kg) {
      build_kgraph_from_fit(selected_node, m_embeds, fit_kg)
      expect_true(TRUE)
    })
}
test_that('build_kgraph_from_fit', test_build_kgraph_from_fit())

test_scaling = function() {

    data_obj = data.table::fread('data/ukb_gae_traineval_noparent_newcui_newmap_newukb2_d512_seed112_markerfinal_0301_hongyi_new_data_addcuicui_dedup0501_weight_cui_phe_by_0.01_loss3_0.01_500.csv', data.table = FALSE)

    rownames(data_obj) = data_obj[, 1]
    names(data_obj) = data_obj[1, ]
    data_obj = as.matrix(data_obj[-1, -1])


    fit_kg = fit_embeds_kg(m_embeds, similarity = 'cosine',
                           threshold_projs = 0.95)

    # selected_nodes = c('Other lab:1200085445', 'C0392535')
    selected_nodes = c('X714.1', 'X709.3', 'X695.42')
    kg_obj = build_kgraph_from_fit(selected_nodes, m_embeds, fit_kg)

    plot(sort(kg_obj$df_nodes$weight))
    plot(exp(seq(0, 10, length.out = 100)))

    # with exp_scale = 7,
    # on 2 nodes too big, on 3 nodes too small
    # need to take target nodes out of scaling

    df_lm = kg_obj$df_nodes$weight %>% 
      { data.frame(y = sort(.), x = exp(seq(min(.), max(.), length.out = length(.)))) }
    sgraph:::get_poly_fit_r2(1, df_lm)

}
#test_that('scaling', test_scaling())
