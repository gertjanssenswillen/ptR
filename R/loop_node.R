#' @title Loop node
#'
#' @description Calculate path dictionary for a loop node based on the children's dictionaries
#'
#' @export loop_node


loop_node <- function(children, n_iterations_loop, p_redo){

	do <- children[[1]]
	redo <- children[[2]]
	out <- children[[3]]
	results_iter <- list()
	for(iterations in 0:n_iterations_loop){
		results <- do %>% mutate(p = p*(1-p_redo))
		for(i in 0:iterations){
			if(i > 0){
				results <- merge_sequential_paths(results, redo) %>%
					mutate(p = p*(p_redo)) %>%
					simplify
				results <- merge_sequential_paths(results, do) %>% simplify
			}
		}
		results <- merge_sequential_paths(results, out) %>% simplify
		results_iter[[iterations+1]] <- results
	}
	results <- bind_rows(results_iter) %>%
		mutate(p = p/sum(n*p)) %>%
		simplify()

	return(results)
}
