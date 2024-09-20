devtools::load_all()
dirpath = system.file('extdata', package = 'kgraph')
fpath = file.path(dirpath, 'gwas-association-downloaded_2024-07-24-EFO_0007623-withChildTraits.tsv')
df_gwas = data.table::fread(fpath, data.table = FALSE)

df_pval = df_gwas %$%
    data.frame(concept1 = MAPPED_TRAIT_URI, concept2 = SNPS,
               weight = PVALUE_MLOG) %>% kgraph:::reshape_multiple_traits()

df_pval$concept1 %<>% gsub('.*(EFO.*)', '\\1', .)

dirpath = system.file('data', package = 'kgraph')
save(df_pval, file = file.path(dirpath, 'df_pval.rda'))


df_dict = df_gwas %$% data.frame(id = c(MAPPED_TRAIT_URI, SNPS),
                                 desc = c(MAPPED_TRAIT, SNPS),
                                 group = c(MAPPED_TRAIT, MAPPED_GENE))

df_dict %<>% subset(!duplicated(id))
df_dict %<>% kgraph:::reshape_multiple_traits_dict()

df_dict$id %<>% gsub('.*(EFO.*)', '\\1', .)

df_dict$desc %<>% ifelse(. == '', NA, .)
df_dict$group %<>% ifelse(. == '', 'Other', .)
df_dict$color = ifelse(grepl('^EFO', df_dict$id), 'Phenotype', 'SNP')

df_pval_dict = df_dict
save(df_pval_dict, file = file.path(dirpath, 'df_pval_dict.rda'))

