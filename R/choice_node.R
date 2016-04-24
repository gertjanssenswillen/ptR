
#' @title Choice node
#'
#' @description Calculate path dictionary for choice node based on the children's dictionaries
#'
#' @export choice_node

choice_node <- function(children){
	bind_rows(children)  %>% simplify %>% return
}

