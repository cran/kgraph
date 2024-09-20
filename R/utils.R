                                                                           
if (getRversion() >= "2.15.1") {                                                
  vars <- c('.', 'clusters', 'concept1', 'concept2', 'desc', 'df_links',
            # this one should be generalized
            'drug_name',
            'display_val', 'from', 'group', 'id', 'selected_concept', 'to',
            'V1', 'V2', 'value', 'weight')
  utils::globalVariables(vars)                                                  
}                                                                               

