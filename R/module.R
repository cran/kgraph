kgraph_inputs = function(ns, type) {

  accept_filetypes = c('.csv', '.tsv', '.gz', '.rda', '.Rdata', '.rds')

  l_tags = list()

  l_tags$main = switch(type, cooc = {

    shiny::fileInput(ns('upload_cooc'), 'Co-occurrence matrix upload',
                     accept = accept_filetypes)

  }, embeds = {

    shiny::fileInput(ns('upload_embeds'), 'Embedding matrix upload',
                     accept = accept_filetypes)

  }, relations = {

    shiny::fileInput(ns('upload_relations'), 'Concepts relations data frame',
                     accept = accept_filetypes)
  })
 
  l_tags$dict = shiny::fileInput(ns('upload_dict'),
                                 'Dictionary data frame upload',
                                 accept = accept_filetypes)

  accordion_params = list(bslib::accordion_panel('Graph parameters',
                                                 
    shiny::selectizeInput(ns('targets'), choices = NULL,
                          label = 'Selected concepts', multiple = TRUE),
   
#   shiny::selectInput(ns('layout'), 'Layout type', selected = 'Springs',
#                      choices = c('Springs', 'Force')),
#  
#   shiny::selectInput(ns('groups'), 'Grouping type', selected = 'Floating',
#                      choices = c('Anchored', 'Floating')),
#  
#   shiny::checkboxInput(ns('log_fit'), 'Log linear transforms', TRUE),
#  
#   shiny::checkboxInput(ns('sqrt_fit'), 'Square root linear transforms', TRUE),
#  
    shiny::sliderInput(ns('exp_scale'),
                       'Exponential scaling base (higher = less large nodes)',
                       0, 20, 7.5, step = 0.1),
#  
#   shiny::sliderInput(ns('upper_bound'), 'Upper bound',
#                      5, 50, 25),
#  
#   shiny::sliderInput(ns('lower_bound'), 'Lower bound',
#                      1, 3, 10),
         
    shiny::sliderInput(ns('n_max_edges'), 'Max. edges (0 for all)',
      	     0, 5e3, 1e3),
    
    # probably could depend on window size
    shiny::sliderInput(ns('label_grid_size'),
                       htmltools::HTML('Label grid size</br>(Smaller = more labels)'),
                       1, 2e3, 150, ticks = FALSE)))
 
  if (type != 'relations') {

    fit_params = bslib::accordion_panel('Fit parameters',
      shiny::selectInput(ns('similarity'), 'Similarity', 
        #c('inprod', 'cosine', 'cov_simi', 'norm_inprod'), selected = 'inprod')
        c('inprod', 'cosine'), selected = 'cosine'),
      
      shiny::fileInput(ns('upload_pairs'), 'Known pairs data frame upload',
                       accept = c('.csv', '.tsv', '.rda', '.Rdata', '.rds')),
      
      shiny::sliderInput(ns('threshold_projs'), 'False positive threshold (%)',
        	             0, 50, 5))

    accordion_params %<>% append(list(fit_params), .)
    accordion_params$open = 'Graph parameters'


    if (type == 'cooc') {

     embeds_params = bslib::accordion_panel('Embeds parameters',
       shiny::sliderInput(ns('term_freq'),
                          'Filter terms on frequency (percent)', 0, 20, 0),
       shiny::sliderInput(ns('svd_rank'), 'SVD dimensions', 2, 2e3, 100))
 
     accordion_params %<>% append(list(embeds_params), .)
    }
  }
 
  l_tags %<>% append(list(do.call(bslib::accordion, accordion_params)))

  do.call(tagList, l_tags)
}

kgraph_outputs = function(ns, type) {

  graphout = list(
      sgraph::sgraphOutput(ns('kg'), height = '100%'),
      conditionalPanel(condition = "output.showlegend", ns = ns,
        absolutePanel(id = ns('legend'), class = 'panel panel-default',
                      fixed = TRUE, draggable = TRUE, top = 150,
                      left = 'auto', right = 40, bottom = 'auto',
                      width = 180, height = '600px',
                      plotOutput(ns('legend'), height = '600px'))))

  cards_args = list(full_screen = TRUE,
      bslib::nav_panel('Graph', do.call(tagList, graphout)),
      bslib::nav_panel('Dictionary', DT::DTOutput(ns('dict'))))

  if (type != 'relations') {
      auc_card = bslib::nav_panel('AUC',
                                  shiny::plotOutput(ns('auc'),
                                                    height = '100%'))

      cards_args %<>% append(list(auc_card))
  }

  do.call(bslib::navset_card_underline, cards_args)
}

kgraphUI = function(id, type) {

  ns = shiny::NS(id)
  panel_title = switch(type, cooc = 'Co-occurrence matrix',
                       embeds = 'Embedding matrix',
                       relations = 'Concepts relations data frame')

  bslib::nav_panel(panel_title,
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(kgraph_inputs(ns, type)),
      kgraph_outputs(ns, type)))
}

update_selectize = function(values, session) {

  shiny::updateSelectizeInput(session, 'targets', choices = sort(values),
                              selected = sample(values, 2), server = TRUE)
}

kgraphServer = function(id, type) {
    
  moduleServer(id, function(input, output, session) {

      ns = session$ns

      react_cooc <- reactive({
  
          df_input = input$upload_cooc
          if (is.null(df_input)) return()
  
          df_input = validate_input_file(df_input)

          nlpembeds::get_pmi(df_input)
        })

      react_embeds <- reactive({
  
          df_input = input$upload_embeds
          m_cooc = react_cooc()
          term_freq = input$term_freq
          svd_rank = input$svd_rank

          if (!is.null(df_input)) {
  
            m_embeds = validate_input_file(df_input, data_matrix = TRUE)
 
            if (is.null(rownames(m_embeds))) {
              rownames(m_embeds) = seq_len(nrow(m_embeds))
            }
 
            update_selectize(rownames(m_embeds), session)
 
            # for now just restart the session
            # session$sendCustomMessage(type = "resetFileInputHandler", ns("upload_dict"))
 
            m_embeds

          } else {

            if (is.null(m_cooc)) return()

            if (term_freq > 0) {
              # since we're doing it on the PMI it's the most frequent that have lowest value
              code_freqs = diag(m_cooc)
              freq_quant = quantile(code_freqs, 1 - term_freq / 100)
              rm_idxs = which(code_freqs >= freq_quant)
              m_cooc = m_cooc[-rm_idxs, -rm_idxs]
            }

            m_cooc = nlpembeds::get_svd(m_cooc, svd_rank)
 
            update_selectize(rownames(m_cooc), session)

            m_cooc
          }
        })
  
      react_relations = reactive({
          df_input = input$upload_relations
          if (is.null(df_input)) return()
  
          df_relations = validate_input_file(df_input)

          update_selectize(unique(unlist(df_relations[1:2])), session)

          df_relations
        })
  
      react_dict = reactive({
  
          df_input = input$upload_dict
          if (is.null(df_input)) return()
  
          df_dict = validate_input_file(df_input)
        })
  
      react_dt_dict = reactive({
  
          df_dict = react_dict()
          if (is.null(df_dict)) {
              validate('Please provide a dictionary data frame')
          }
  
          DT::datatable(df_dict, list(pageLength = 100),
                        filter = 'top', rownames = FALSE)
        })
  
  
      react_pairs = shiny::reactive({

          df_input = input$upload_pairs
          if (is.null(df_input)) return()
  
          df_pairs = validate_input_file(df_input)

          m_embeds = react_embeds()
  
          pairs_cols = 1:2
          for (pairs_col_idx in pairs_cols) {
            seq_test = seq_len(ncol(df_pairs))
            if (pairs_col_idx == 2) seq_test = tail(seq_test, -pairs_cols[1])
            for (col_idx in seq_test) {
              if (any(rownames(m_embeds) %in% df_pairs[[col_idx]])) {
                pairs_cols[pairs_col_idx] = col_idx
                break
              }
              if (col_idx == tail(seq_test, 1)) {
                validate('Embeds ids not found in known pairs')
              }
            }
          }
          df_pairs[pairs_cols]
      })
  
      react_fit = shiny::reactive({
  
          m_embeds = react_embeds()
          if (is.null(m_embeds)) return()
  
          df_pairs_fit = react_pairs()
          similarity = input$similarity
          threshold_projs = input$threshold_projs
  
          # similarity = 'cosine'; threshold_projs = 5; df_pairs_fit = NULL
          fit_kg = fit_embeds_kg(m_embeds, df_pairs = df_pairs_fit,
                                 similarity = similarity,
                                 threshold_projs = 1 - threshold_projs / 100)
        })
  
  
      react_kgraph = shiny::reactive({
  
          m_embeds = react_embeds()
          df_dict = react_dict()
          fit_kg = react_fit()
          df_relations = react_relations()
          target_concepts = input$targets
          n_max_edges = input$n_max_edges
          exp_scale = input$exp_scale

          if (is.null(m_embeds) && is.null(df_relations)) {
              validate('Please provide either a data matrix or a concepts relationships data frame')
          }
          if (is.null(target_concepts)) validate('Select target concepts')
  
          if (!is.null(m_embeds)) {
  
            kgraph = build_kgraph_from_fit(target_concepts, m_embeds, fit_kg,
                                           df_dict = df_dict,
                                           exp_scale = exp_scale,
                                           n_max_edges = n_max_edges)
          } else {
  
            kgraph = build_kgraph(target_concepts, df_relations,
                                  exp_scale = exp_scale,
                                  df_dict = df_dict, n_max_edges = n_max_edges)
          }
        })
  
      react_colors = shiny::reactive({
  
          df_dict = react_dict()
          if (is.null(df_dict)) return()
  
          colors_mapping = if ('color' %in% names(df_dict)) {
            get_color_map(c(unique(df_dict$color), 'Groups', 'Other'))
          }
      })
  
      react_sgraph = shiny::reactive({
  
          kgraph = react_kgraph()
          colors_mapping = react_colors()
          label_grid_size = input$label_grid_size
  
          sgraph = get_sgraph(kgraph, colors_mapping,
        		  label_grid_cell_size = label_grid_size)
        })
  
  
      react_legend = shiny::reactive({
  
          kgraph = react_kgraph()
          colors_mapping = react_colors()
  
          if (is.null(colors_mapping)) return()
  
          gt_legend = sgraph::get_legend(colors_mapping,
        				 unique(kgraph$df_nodes$clusters))
        })

      react_auc = shiny::reactive({

          df_pairs = react_pairs()
          if (is.null(df_pairs)) validate('Select a known pairs data frame')
  
          fit_kg = react_fit()
          if (is.null(fit_kg$roc)) return()
  
          pROC::plot.roc(fit_kg$roc, print.auc = TRUE)
        })

        output$kg = sgraph::renderSgraph(react_sgraph())
        output$dict = DT::renderDT(react_dt_dict())
        output$showlegend = shiny::reactive({ !is.null(react_legend()) })
        output$legend = shiny::renderPlot(grid::grid.draw(react_legend()))
        output$auc = shiny::renderPlot(react_auc(), res = 96)

        outputOptions(output, "showlegend", suspendWhenHidden = FALSE)
    })
}

validate_input_file = function(df_input, data_matrix = FALSE) {

  fpath = df_input$datapath
  fext = tools::file_ext(df_input$name)

  data_obj = switch(fext,
    csv = data.table::fread(fpath, sep = ",", data.table = FALSE),
    tsv = data.table::fread(fpath, sep = "\t", data.table = FALSE),
    gz = data.table::fread(fpath, data.table = FALSE),
    rda = get(load(fpath)),
    Rdata = get(load(fpath)),
    rds = get(load(fpath)),
    validate("Invalid file; Please upload a .csv, .tsv, .gx, .rda, .Rdata or .rds file"))

  if (data_matrix && fext %in% c('csv', 'tsv', 'gz')) {
    rownames(data_obj) = data_obj[, 1]
    names(data_obj) = data_obj[1, ]
    data_obj = as.matrix(data_obj[-1, -1])
  }

  data_obj
}
