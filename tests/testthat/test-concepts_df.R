
test_concepts_df = function() {

  data('df_pval')
  kg = build_kgraph('EFO_0007623', df_pval)
  expect_true(TRUE)

  data('df_pval_dict')
  kg = build_kgraph('EFO_0007623', df_pval, df_pval_dict)
  expect_true(TRUE)

  kg = build_kgraph(c('EFO_0007623', 'EFO_0007624'), df_pval, df_pval_dict)
  expect_true(TRUE)
}
test_that('concepts_df', test_concepts_df())
