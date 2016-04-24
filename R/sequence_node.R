#' @title Sequence node
#'
#' @description Calculate path dictionary for Sequence node based on the children's dictionaries
#'
#' @export sequence_node

sequence_node <- function(children){
	results <- children[[1]]
	if(length(children) > 1) {
		for(i in 2:length(children)) {
			results <- merge_sequential_paths(results, children[[i]]) %>% simplify
		}
	}
	return(results)
}
