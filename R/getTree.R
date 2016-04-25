

getTree <- function(study, add_data = FALSE){
  study_name <- getStudies(name.match = toupper(study), cull.columns = FALSE)$ontologyTerm.name
  study_as_list <- list(structure(list(), stopened=TRUE, stselected=TRUE, "concept_type" = "Study"))
  concepts <- getConcepts(study)
  names(study_as_list) <- study_name
  for (i in 1:nrow(concepts)){
    concept_type <- concepts[i,"type"]
    api_link <- concepts[i, "api.link.self.href"]
    if (concept_type != "UNKNOWN"){
      nodes <- unlist(strsplit(concepts[i,"fullName"], split = "\\", fixed = TRUE))
      # Give the highest node index to get the study object
      study_index <- min(match(toupper(c(study, study_name)), toupper(nodes)), na.rm = TRUE)
      study_nodes <- nodes[study_index:length(nodes)]
      study_as_list <- .add_path_to_tree(study_as_list, study_nodes, concept_type, api_link, add_data)
    }
  }
  study_as_list
}


.add_path_to_tree <- function(study_as_list, study_nodes, concept_type, api_link, add_data){
  accum_path <- c(study_nodes[1])
  for (node_index in 2:length(study_nodes)){
    current_node <- study_nodes[node_index]
    if (!(current_node %in% names(study_as_list[[accum_path]]))){
      if(node_index == length(study_nodes)){
        study_as_list[[c(accum_path, current_node)]] <- structure(api_link)
        if (concept_type == "NUMERIC"){
          attr_to_add <- list("sticon" = "signal", "apilink" = api_link, "concept_type" = "Numerical variable")
          attributes(study_as_list[[c(accum_path, current_node)]]) <- attr_to_add
          
          if (isTRUE(add_data) && is.null(attr(study_as_list[[c(accum_path, current_node)]], "data_values"))){
            data_values <- as.numeric(getObservations(names(study_as_list), concept.links = api_link)$observations[,2])
            attr(study_as_list[[c(accum_path, current_node)]], "data_values") <- data_values
          }
        } else if (concept_type == "CATEGORICAL_OPTION"){
          
          attr_to_add <- list("sticon" = "tag", "apilink" = api_link, "concept_type" = "Categorical option")
          attributes(study_as_list[[c(accum_path, current_node)]]) <- attr_to_add
          
          attr(study_as_list[[accum_path]], "concept_type") <- "Categorical variable"
          
          if (isTRUE(add_data) && is.null(attr(study_as_list[[accum_path]], "data_values"))){
            data_values <- table(getObservations(names(study_as_list), concept.links = dirname(api_link))$observations[,2])
            attr(study_as_list[[accum_path]], "data_values") <- data_values
          }
          
        }
        else {
          attr_to_add <- list("sticon" = "th", "apilink" = api_link, "concept_type" = "High-dimensional")
          attributes(study_as_list[[c(accum_path, current_node)]]) <- attr_to_add
        }
      } else {
        study_as_list[[c(accum_path, current_node)]] <- structure(list())
        attr_to_add <- list("apilink" = api_link, "concept_type" = "Directory")
        attributes(study_as_list[[c(accum_path, current_node)]]) <- attr_to_add
      }
    }
    accum_path <- c(accum_path, current_node)
  }
  return(study_as_list)
}