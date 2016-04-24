#' @title Or node
#'
#' @description Calculate path dictionary for or node based on the children's dictionaries
#'
#' @export or_node


or_node <- function(children) {
	final_paths <- list()
	binary_grid <- combos(length(children))$binary %>% data.frame
	for(i in 1:nrow(binary_grid)){
		selection <- (binary_grid[i,] %>% t %>% as.vector == 1)
		selected_paths <- children[selection]
		result <- selected_paths[[1]]
		if(length(selected_paths)>1) {
			for(j in 2:length(selected_paths)){
				result <- merge_parrallel_paths(result, selected_paths[[j]]) %>% simplify
			}
		}
		final_paths[[i]] <- result
	}
	final_paths <- bind_rows(final_paths) %>% simplify
	return(final_paths)
}
