devtools::load_all()

dirpath = system.file('portable_NILE', package = 'kgraph')
fpath = file.path(dirpath, 'project', 'test', 'input', 'test_dict.txt')

if (!file.exists(fpath)) {
  stop('Need to download NILE and populate folder "portable_NILE" in "inst" (URL in vignette)')
}

df_dict = data.table::fread(fpath, data.table = FALSE)
df_dict = df_dict[c(2, 1, 4, 5)] %>%
       	setNames(c('id', 'label', 'group', 'group_details'))

dirpath = system.file('data', package = 'kgraph')
m_embeds = get(load(file.path(dirpath, 'fusion_glove_fit_nile.rds')))

map_idxs = match(rownames(m_embeds), df_dict$id)
df_dict = df_dict[na.omit(map_idxs), ]
m_embeds = m_embeds[!is.na(map_idxs), ]

save(df_dict, file = file.path(dirpath, 'df_dict.rds'))
save(m_embeds, file = file.path(dirpath, 'm_embeds.rds'))

