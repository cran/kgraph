# in input is a triangular sparse matrix or a dataframe
get_pmi = function(spm_cooc) {

  if (!inherits(spm_cooc, 'data.frame')) spm_cooc = spm_to_df(spm_cooc)

  code_freqs = subset(spm_cooc, V1 == V2) %$% setNames(value, V1)
  code_freqs = code_freqs[order(names(code_freqs))]

  row_sums = spm_cooc %$% split(value, V1) %>% sapply(sum)
  row_sums = row_sums + sapply(split(spm_cooc$value, spm_cooc$V2), sum)

  # diagonal is counted twice
  row_sums = row_sums - code_freqs
  total_sum = sum(spm_cooc$value) * 2 - sum(code_freqs)

  denoms = row_sums[spm_cooc$V1] * row_sums[spm_cooc$V2]

  spm_cooc$value = log(spm_cooc$value * total_sum / denoms)

  as.matrix(build_spm_cooc_sym(spm_cooc))
}

get_svd = function(m_pmi, embedding_dim = 100, svd_rank = embedding_dim * 2) {


  row_names = rownames(m_pmi)
  m_pmi = rsvd::rsvd(m_pmi, svd_rank)

  idx = which(sign(m_pmi$u[1, ]) == sign(m_pmi$v[1, ]))

  if (embedding_dim > 0) {

    idx %<>% head(embedding_dim)

    if (length(idx) < embedding_dim) {
      warning('The output embedding has less dimensions than required. Try increasing the svd_rank parameter.')
    }
  }

  m_pmi = t(t(m_pmi$u[, idx, drop = FALSE]) * sqrt(m_pmi$d[idx]))

  rownames(m_pmi) = row_names

  m_pmi
}

build_spm_cooc_sym = function(df_cooc) {

  uniq_codes = sort(unique(unlist(df_cooc[1:2])))

  Matrix::sparseMatrix(match(df_cooc$V1, uniq_codes),
                       match(df_cooc$V2, uniq_codes), x = df_cooc$value,
                       symmetric = TRUE,
                       dims = c(length(uniq_codes), length(uniq_codes)),
                       dimnames = list(uniq_codes, uniq_codes))
}


spm_to_df = function(spm) {

  spm_names = rownames(spm)
  spm = setNames(as.data.frame(Matrix::summary(spm)), c('V1', 'V2', 'value'))
  
  data.frame(V1 = spm_names[spm$V1], V2 = spm_names[spm$V2],
             value = spm$value)
}
