#' @title Parallel node
#'
#' @description Calculate path dictionary for Parallel node based on the children's dictionaries
#'
#' @export parallel_node


parallel_node <- function(children){
	results <- children[[1]]
	if(length(children) > 1){
		for(i in 2:length(children)){
			results <- merge_parrallel_paths(results, children[[i]]) %>% simplify
		}
	}
	return(results)
}
