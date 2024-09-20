
test_sup_nodes = function() {

# devtools::load_all()
dirpath = system.file('data', package = 'kgraph')

pval <- readRDS(file.path(dirpath, "pval.rds")) %>% as.data.frame
#dict <- readRDS(file.path(dirpath, "df_dict_dg.rds")) %>% as.data.frame
df_dict <- get(load(file.path(dirpath, "df_dict_dg.rds")))

#df_drugs = data.table::fread(file.path(dirpath, 'drug_gene_phase_v32_17APR2024.csv'), data.table = FALSE)

#df_pheno = subset(dict, dataset %in% c('Labs', 'Others', 'PheCodes', 'Vital Signs'))
#df_gene = subset(dict, dataset == 'GENE')
#df_dict = rbind(df_pheno, df_gene)


  # df_gene %<>% druggablegenome:::add_drug_info(df_drugs)
  # df_drugs = subset(df_gene, !is.na(drug_name))

#df_gene = get(load(file.path(dirpath, 'df_gene.rds')))
df_drugs = get(load(file.path(dirpath, 'df_drugs.rds')))

  pval$PVAL = -log10(pval$PVAL)                                                 
  df_weights = pval %>% setNames(c('concept1', 'concept2', 'weight'))           

col_idx = match('type_color', names(df_dict))
names(df_dict)[col_idx] = 'color'

selected_phenos = c('Bicarbonate_Max', 'Chloride_BSP_Max')
l_graph = kgraph:::build_kgraph(selected_phenos, df_weights, df_dict,
                                spring_weights = TRUE, exp_scale = 15,        
                                display_val_str = '\nP-value: ',
                                str_other = 'Other gene',
                                lower_bound_const = 3)

  l_graph = kgraph:::build_kgraph(selected_phenos, df_weights, df_dict,
                                  df_sup_nodes = df_drugs, spring_weights = TRUE)
  igraph = sgraph:::l_graph_to_igraph(l_graph)
  sgraph = sgraph:::sgraph_clusters(igraph,
                      node_size = 'weight',
                                      label = label_attrs,
                                      layout = igraph::layout_with_kk(igraph))

  expect_true(TRUE)

    #  test_groups = function() {
    # FIXME
    # linked to both Other and to group (i think it's resolved now)
    l_graph$df_links %>% subset(to == 'AS3MT')
}
#test_that('sup_nodes', test_sup_nodes())

