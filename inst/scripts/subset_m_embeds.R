  devtools::load_all()

  dirpath = system.file('data', package = 'kgraph')

  df_dict = get(load(file.path(dirpath,'mh_embeds', 'df_dict.rds')))
  m_embeds = get(load(file.path(dirpath, 'm_embeds.rds'))) 

  df_pairs = get(load(file.path(dirpath, 'df_cuis_pairs.rds')))

  df_pairs = subset(df_pairs, umls_id.x %in% rownames(m_embeds) &
                                umls_id.y %in% rownames(m_embeds))

  # we keep only the CUIs we have in df_pairs, the suicide related,
  # and a few other mental health related
  target_strs = c('suicid', 'alcohol', 'sleep', 'delirium', 'shizo', 'bipolar',
                  'trauma', 'psychosis', 'borderline', 'personality',
                  'substance', 'gambling')

  target_idxs = grep(paste(target_strs, collapse = '|'), df_dict$label)
  target_nodes = df_dict$id[grep_idxs]

  uniq_nodes = unique(c(target_nodes, unlist(df_pairs[c(1, 3)])))

  m_embeds = m_embeds[uniq_nodes, ]
  save(m_embeds, file = file.path(dirpath, 'm_embeds.rda'))

  save(df_pairs, file = file.path(dirpath, 'df_cuis_pairs.rda'))

  #df_dict$color = grepl('^rs', df_dict$color)
  df_dict = subset(df_dict, id %in% rownames(m_embeds))
  df_embeds_dict = df_dict[c('id', 'desc')]
  df_embeds_dict$color = df_dict$group
  df_embeds_dict$group = df_dict$color

  save(df_embeds_dict, file = file.path(dirpath, 'df_embeds_dict.rda'))
