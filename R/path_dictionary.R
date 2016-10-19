#' @title Path dictionary
#'
#' @description Calculate path dictionary for a process tree
#'
#' @param tree A process tree
#' @param maximum iteration threshold for loops
#' @param p_redo Redo probability for loops
#' @param xor_probabilities Names list of vectors with probability distribution of all xor constructs
#'
#' @export path_dictionary


path_dictionary <- function(tree, n_iterations_loop, p_redo, xor_probabilities){
	if(is.null(tree$children)){
		if(tree$name == "tau")
			return(data.frame( length = 0,n = 1, p = 1))
		else
			return(data.frame(length = 1,n = 1, p = 1))
	}
	else {
		children <- list()
		for(i in 1:length(tree$children)){
			children[[i]] <- Recall(tree$children[[i]], n_iterations_loop = n_iterations_loop, p_redo = p_redo, xor_probabilities = xor_probabilities)
		}

		if(grepl("sequence", tree$name)){
			sequence_node(children) %>% return
		}
		else if(grepl("xor", tree$name) & !grepl("xorLoop", tree$name)){
			choice_node(children, xor_probabilities = xor_probabilities[[tree$name]]) %>% return
		}
		else if(grepl("and", tree$name)){
			parallel_node(children) %>% return
		}
		else if(grepl("xorLoop", tree$name)) {
			loop_node(children, n_iterations_loop = n_iterations_loop, p_redo = p_redo) %>% return
		}
		else if(grepl("or", tree$name)){
			or_node(children) %>% return
		}
	}
}
