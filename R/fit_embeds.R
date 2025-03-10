
#' Fit embeddings to a kgraph object
#'
#' Build a fit_kgraph object to act as an intermediate between the embeddings
#' and the knowledge graph. If possible (i.e. if number of features is not too
#' large) compute all pair-wise similarities, otherwise determine the similarity
#' threshold using a number of random pairs. If a data frame of known pairs is
#' available, call fit_embeds_to_pairs which will produce an AUC and use the
#' threshold_projs parameter as the specificity threshold (e.g. the default
#' specificity of 0.9 corresponds to 10 percent false positives). Otherwise take the
#' quantile of similarity values corresponding to threshold_projs.
#'
#' @param m_embeds Embedding matrix, rownames must be able to be matched to
#'                 concepts in df_pairs
#' @param similarity Similarity measure to be computed. One of 'inprod' (inner
#'                   product), 'cosine', 'cov_simi' (covariance similarity),
#'                   'norm_inprod' (normalized inner product).
#' @param threshold_projs Specificity threshold to use for projections.
#'                        (default 0.9 is equivalent to 10 percent false positives,
#'                        and 0.95 to 5 percent false positives)
#' @param df_pairs Known relationships data frame
#' @param df_pairs_cols Columns of df_pairs for identifiers, that map to
#'                      m_embeds rownames
#' @param max_concepts Maximum number of concepts to compute all pair-wise
#'                     similarities
#' @param ... Passed to gen_df_notpairs
#'
#' @return Knowledge graph, list of slots df_nodes and df_links
#'
#' @export
fit_embeds_kg = function(m_embeds,
  similarity = c('cosine', 'inprod', 'cov_simi', 'norm_inprod'),
  threshold_projs = 0.9, df_pairs = NULL, df_pairs_cols = 1:2,
  max_concepts = 1e3, ...) {         

  similarity = match.arg(similarity)
  m_embeds = as.matrix(m_embeds)

  if (is.null(rownames(m_embeds))) rownames(m_embeds) = seq_len(nrow(m_embeds))

  if (!is.null(df_pairs)) {

    fit_embeds_to_pairs(m_embeds, df_pairs, df_pairs_cols, similarity,
                        threshold_projs, max_concepts)

  } else {

    similarity_fun = switch(similarity, cosine = cosine_simi,
                            get(similarity))
 
    is_full_projs = nrow(m_embeds) < max_concepts
    if (is_full_projs) {
 
      m_simi = similarity_fun(m_embeds)
      threshold_5fp = quantile(m_simi, 0.95)
      threshold_projs = quantile(m_simi, threshold_projs)
      df_projs = project_pairs(m_simi, threshold_projs)
 
    } else {
 
      df_notpairs = gen_df_notpairs(rownames(m_embeds), ...)

      simis = similarity_fun(m_embeds[df_notpairs[[1]], ],
                             m_embeds[df_notpairs[[2]], ])

      threshold_5fp = quantile(simis, 0.95)
      threshold_projs = quantile(simis, threshold_projs)

      df_notpairs$weight = diag(simis)
      df_projs = subset(df_notpairs, weight > threshold_projs)
    }
 
    list(threshold_5fp = threshold_5fp, threshold_projs = threshold_projs,
         df_projs = df_projs, is_full_projs = is_full_projs,
         similarity = similarity)
  }
}

#' Fit embeds to pairs
#'
#' Fit an embeddings matrix to a dataframe of known pairs of related concepts.
#' Depending on matrix dimension, either compute all pair-wise similarities, or
#' only those existing in the known pairs.
#'
#' @param m_embeds Embedding matrix, rownames must be able to be matched to
#'                 concepts in df_pairs
#' @param df_pairs Known relationships data frame
#' @param df_pairs_cols Columns of df_pairs for identifiers, that map to
#'                      m_embeds rownames
#' @param similarity Similarity measure to be computed. One of 'inprod' (inner
#'                   product), 'cosine', 'cov_simi' (covariance similarity),
#'                   'norm_inprod' (normalized inner product).
#' @param threshold_projs Specificity threshold to use for projections.
#'                        (default 0.9 is equivalent to 10 percent false positives,
#'                        and 0.95 to 5 percent false positives)
#' @param max_concepts Maximum number of concepts to compute all pair-wise
#'                     similarities
#'
#' @return List object with slots roc (pROC::roc return), sims and truth (to
#'         recompute partial AUCs using pROC), threshold_5fp (5 percent false positive
#'         threshold), n_concepts (length of concepts in embeddings), and
#'         df_projs (data frame listing pair-wise concepts similarities above
#'         threshold_projs).
#'
#' @export
fit_embeds_to_pairs = function(m_embeds, df_pairs, df_pairs_cols = 1:2,
  similarity = c('inprod', 'cosine', 'cov_simi', 'norm_inprod'),
  threshold_projs = 0.9, max_concepts = 1e3) {

  # intersect pairs and embeds                                                  
  concepts_embeds = rownames(m_embeds)                                          
  pairs_intersect = intersect(concepts_embeds,                                  
                              unique(unlist(df_pairs[df_pairs_cols])))          

  df_pairs %<>% subset(df_pairs[[df_pairs_cols[1]]] %in% pairs_intersect &      
                       df_pairs[[df_pairs_cols[2]]] %in% pairs_intersect)       

  # if not too much concepts, get similarity on all for projections             
  # we might want distances from disease or target nodes to all others
  is_full_projs = nrow(m_embeds) <= max_concepts
  
  if (!is_full_projs) { 
    concepts_embeds = pairs_intersect
    m_embeds = m_embeds[concepts_embeds, ]                                    
  }
                                                                                
  similarity_fun = switch(similarity, cosine = cosine_simi,                  
                          get(similarity))                                      
  m_simi = similarity_fun(m_embeds)                                             
                                                                                
  # we need as many false relations to test AUC,
  # make sure true pairs are also ordered to remove them
  df_pairs[df_pairs_cols] %<>% order_dataframe                                  
  df_allpairs = df_pairs[df_pairs_cols] %>%                                     
      rbind(gen_df_notpairs(pairs_intersect, .), .)                             
                                                                                
  # not very elegant right                                                      
  sims = apply(df_allpairs, 1, function(pairs) m_simi[pairs[[1]], pairs[[2]]])  
                                                                                
  # fit roc                                                                     
  truth = rep(0:1, each = nrow(df_pairs))                                       
  roc_obj = pROC::roc(truth, sims, direction = '<', quiet = TRUE)               
                                                                                
  # return what's needed for partial AUC and projections                        
  threshold_5fp = get_cutoff_threshold(roc_obj)                                 
  threshold_projs = get_cutoff_threshold(roc_obj, threshold_projs)
  df_projs = project_pairs(m_simi, threshold_projs)      
                                                                                
  list(roc = roc_obj, sims = sims, truth = truth,
       threshold_5fp = threshold_5fp, threshold_projs = threshold_projs,
       n_concepts = length(concepts_embeds), df_projs = df_projs,
       is_full_projs = is_full_projs, similarity = similarity)
} 

#' Get cut-off threshold
#'
#' @param roc_obj Object returned by pROC::roc
#' @param specificity_lvl Specificity threshold
#'                        (default 0.95 is equivalent to 5 percent false positives,
#'                        and 0.9 to 10 percent false positives)
#'
#' @return Similarity value threshold
#'
#' @export
get_cutoff_threshold = function(roc_obj, specificity_lvl = 0.95) {

  idx = abs(roc_obj$specificities - specificity_lvl) %>% which.min

  roc_obj$thresholds[idx]
}

#' Covariance similarity
#'
#' @param m_data Data matrix
#' @return Similarity matrixd
#'
#' @export
cov_simi = function(m_data) cov(t(m_data))

#' Predict known pairs
#'
#' @param m_simi Similarity matrix
#' @param threshold Similarity value threshold
#'
#' @return Data frame with columns concept1, concept2, weight
#'
#' @export
project_pairs = function(m_simi, threshold) {

  simi_dimnames = dimnames(m_simi)
  m_simi = ifelse(lower.tri(m_simi), m_simi, 0)
  dimnames(m_simi) = simi_dimnames
  df_preds = reshape2::melt(m_simi) %>% subset(value > threshold)
  df_preds[[1]] %<>% as.character
  df_preds[[2]] %<>% as.character

  df_preds = df_preds[order(df_preds$value, decreasing = TRUE), ]

  setNames(df_preds, c('concept1', 'concept2', 'weight'))
}

#' Generate null pairs
#'
#' @param ids Identifiers to sample from
#' @param df_pairs Known pairs data frame, to make sure no null pairs are in
#' @param n_notpairs Direct parameter to set number of null pairs returned,
#'                   bypasses parameter type.
#'
#' @return Data frame with columns concept1, concept2, weight
#'
#' @export
gen_df_notpairs = function(ids, df_pairs = NULL,
			   n_notpairs = if (is.null(df_pairs)) 1e3 else nrow(df_pairs)) {

  df_notpairs = sample(ids, n_notpairs * 4, replace = TRUE) %>%
    matrix(ncol = 2) %>% as.data.frame %>% order_dataframe %>%
    subset(.[[1]] != .[[2]] & !duplicated(paste0(.[[1]], .[[2]])))

  if (is.null(df_pairs)) {

    df_notpairs %<>% setNames(c('concept1', 'concept2'))

  } else {

    df_notpairs %<>% setNames(names(df_pairs[1:2])) %>%
      setdiff_dataframe(df_pairs) %>%
      head(n_notpairs)
  }
}


# useful functions to compare pairs and build kgraphs
setdiff_dataframe = function(df_x, df_y, cols = 1:2) {
  df_x %>% subset(!Reduce('paste0', .[cols]) %in% Reduce('paste0', df_y[cols]))
}
# maybe could be optimized
order_dataframe = function(df_x, cols = 1:2, relevant_pattern = NULL) {

  # put strings matching relevant patterns in first column
  if (!is.null(relevant_pattern)) {

    df_x %<>% subset(grepl(relevant_pattern, .[[cols[1]]]) |
                     grepl(relevant_pattern, .[[cols[2]]]))

    uniq_lvls = unique(unlist(df_x[cols]))
    relevant_lvls = grep(relevant_pattern, uniq_lvls)
    uniq_lvls = c(uniq_lvls[relevant_lvls], uniq_lvls[-relevant_lvls])

    df_x[cols] %<>% lapply(function(lvls) factor(lvls, uniq_lvls) %>%
                           as.numeric)
    df_x %<>% order_dataframe
    df_x[cols] %<>% lapply(function(lvls) uniq_lvls[lvls])

  } else {
    df_x[cols] = apply(df_x[cols], 1, sort) %>%
      t %>% as.data.frame %>% setNames(names(df_x[cols]))
  }

  df_x
}

                                                     
# not used yet, for ref
get_ppv = function(l_fit_embeds,
                   threshold = get_cutoff_threshold(l_fit_embeds$roc)) {

  preds = ifelse(l_fit_embeds$sims > threshold, 1, 0)
  sum(preds == 1 & preds == l_fit_embeds$truth) / sum(preds == 1)
}

# move to kgraph_celehs
get_known_pairs = function() {

  dirpath = system.file('evaluations', package = 'psychclust')
  df_pairs = get(load(file.path(dirpath, 'pairs_arranged.Rdata')))

  df_pairs = subset(df_pairs, group == 'cui-cui')[c('code1', 'code2')]
}

# TODO export in opticskxi
# inner product
inprod <- function(x, y) {                                          if (missing(y)) y = x
  x %*% t(y)
}
