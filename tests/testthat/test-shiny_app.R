
test_shiny_app = function() {

# cha embeds

#devtools::load_all()
dirpath = system.file('data', package = 'kgraph')
m_embeds = get(load(file.path(dirpath, 'cha_embeds', 'm_embeds.rds')))
df_dict = get(load(file.path(dirpath, 'cha_embeds', 'df_dict.rds')))

rownames(m_embeds) = seq_len(nrow(m_embeds))

set.seed(1)
target_concepts = sample(rownames(m_embeds), 2)

fit_kg = fit_embeds_kg(m_embeds)

kgraph = build_kgraph_from_fit(target_concepts, m_embeds, fit_kg,   
                               df_dict = df_dict)           


colors_mapping = get_color_map(c(unique(df_dict$color), 'Groups', 'Other'))

sgraph = get_sgraph(kgraph, colors_mapping)

gt_legend = sgraph:::get_legend(colors_mapping,
  			 unique(kgraph$df_nodes$clusters))

# cuis


# devtools::load_all()
dirpath = system.file('data', package = 'kgraph')
df_pairs = get(load(file.path(dirpath, 'df_cuis_pairs.rds')))
df_pairs_cols = c('umls_id.x', 'umls_id.y')

dirpath = system.file('data', package = 'kgraph')
m_embeds = get(load(file.path(dirpath, 'm_embeds.rds')))
df_dict = get(load(file.path(dirpath, 'df_dict.rds')))
#df_dict$desc = df_dict$label
#df_dict$color = df_dict$group
#df_dict$group = df_dict$group_details
#save(df_dict, file = 'data/df_dict.rds')
#data.table::fwrite(df_dict, file = 'data/cuis_dict.tsv', sep = '\t')

set.seed(1)
target_concepts = sample(rownames(m_embeds), 2)

fit_kg = fit_embeds_kg(m_embeds, df_pairs = df_pairs[df_pairs_cols],
  				   similarity = 'cosine',
  				   threshold_projs = 1 - 5 / 100)

kgraph = build_kgraph_from_fit(target_concepts, m_embeds, fit_kg,   
                               df_dict = df_dict)           
# kgraph = build_kgraph_from_fit(target_concepts, m_embeds, fit_kg)


colors_mapping = get_color_map(c(unique(df_dict$color), 'Groups', 'Other'))

igraph = sgraph:::l_graph_to_igraph(kgraph)
sgraph = get_sgraph(kgraph, colors_mapping, igraph = igraph)

gglegend = sgraph:::get_legend(colors_mapping,
  			 unique(kgraph$df_nodes$clusters))


# suqi embeds



dirpath = system.file('data', package = 'kgraph')
m_embeds = data.table::fread(file.path(dirpath, 'ukb_gae_traineval_noparent_newcui_newmap_newukb2_d512_seed112_markerfinal_0301_hongyi_new_data_addcuicui_dedup0501_weight_cui_phe_by_0.01_loss3_0.01_500.csv'), sep = ',', data.table = FALSE)

    rownames(m_embeds) = m_embeds[, 1]
    names(m_embeds) = m_embeds[1, ]
    m_embeds = as.matrix(m_embeds[-1, -1])

df_dict = data.table::fread(file.path(dirpath, 'cuis_dict.tsv'), sep = '\t', data.table = FALSE)

set.seed(1)
target_concepts = sample(rownames(m_embeds), 2)

fit_kg = fit_embeds_kg(m_embeds,
  				   similarity = 'cosine',
  				   threshold_projs = 1 - 5 / 100)

kgraph = build_kgraph_from_fit(target_concepts, m_embeds, fit_kg,   
                               df_dict = df_dict)           

}
#test_that('shiny_app', test_shiny_app())
