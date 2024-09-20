#' @importFrom amap Dist
#' @importFrom bslib accordion accordion_panel card layout_sidebar nav_panel
#'                   navset_card_underline sidebar
#' @importFrom data.table fread
#' @importFrom dplyr group_by summarize
#' @importFrom grid grid.draw
#' @importFrom htmltools tagList
#' @importFrom igraph layout_with_kk
#' @importFrom Matrix sparseMatrix summary
#' @importFrom plyr rbind.fill
#' @importFrom RColorBrewer brewer.pal
#' @importFrom reshape2 melt
#' @importFrom rsvd rsvd
#' @importFrom sgraph convert_to_spring_weights get_color_map get_legend
#'                    highlight_multiple_connected multiline_labels scale_graph
#'                    sgraph_clusters
#' @importFrom shiny absolutePanel checkboxInput conditionalPanel fileInput
#'                   moduleServer NS outputOptions plotOutput
#'                   reactive renderPlot selectInput selectizeInput sliderInput
#'                   updateSelectizeInput validate
#' @importFrom stats cov median na.omit quantile setNames var
#' @importFrom utils head tail
NULL


#' Pipe
#'
#' Pipe an object forward into a function or call expression.
#' Magrittr imported function, see details and examples in the magrittr package.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return Result of rhs applied to lhs, see details in magrittr package.
#' @export
NULL

#' Assignment pipe
#'
#' Pipe an object forward into a function or call expression and update the
#' `lhs` object with the resulting value.
#' Magrittr imported function, see details and examples in the magrittr package.
#'
#' @importFrom magrittr %<>%
#' @name %<>%
#' @rdname compound
#' @param lhs An object which serves both as the initial value and as target.
#' @param rhs a function call using the magrittr semantics.
#' @return None, used to update the value of lhs.
#' @export
NULL

#' Exposition pipe
#'
#' Expose the names in `lhs` to the `rhs` expression.
#' Magrittr imported function, see details and examples in the magrittr package.
#'
#' @importFrom magrittr %$%
#' @name %$%
#' @rdname exposition
#' @param lhs A list, environment, or a data.frame.
#' @param rhs An expression where the names in lhs is available.
#' @return Result of rhs applied to one or several names of lhs.
#' @export
NULL


