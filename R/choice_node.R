
#' @title Choice node
#'
#' @description Calculate path dictionary for choice node based on the children's dictionaries
#'
#' @export choice_node

choice_node <- function(children, xor_probabilities ){

	execution_probabilties <- xor_probabilities


	for(i in 1:length(children)){
		children[[i]] <- children[[i]] %>% mutate(p = p*execution_probabilties[[i]])
	}
	bind_rows(children) %>%
		simplify %>% return()
}

