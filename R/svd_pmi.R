# in input is a triangular sparse matrix or a dataframe
get_pmi = function(spm_cooc) {

  if (!inherits(spm_cooc, 'data.frame')) spm_cooc = spm_to_df(spm_cooc)

  code_freqs = subset(spm_cooc, V1 == V2) %$% setNames(value, V1)

  spm_cooc$value = spm_cooc$value / code_freqs[spm_cooc$V1] /
    code_freqs[spm_cooc$V2]

  as.matrix(build_spm_cooc_sym(spm_cooc))
}

get_svd = function(m_pmi, svd_rank = 100) {

  row_names = rownames(m_pmi)
  m_pmi = rsvd::rsvd(m_pmi, svd_rank)

  m_pmi = m_pmi$u %*% diag(m_pmi$d)
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
