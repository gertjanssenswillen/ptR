#' @title Loop node
#'
#' @description Calculate path dictionary for a loop node based on the children's dictionaries
#'
#' @export loop_node


loop_node <- function(children, n_iterations_loop){
	do <- children[[1]]
	redo <- children[[2]]
	exit <- children[[3]]


	Repeat <- data.frame(length = 0,n = 1)
	XORset <- data.frame(length = 0,n = 1)

	for(iterations in 1:n_iterations_loop){
		Repeat = sequence_node(list(Repeat, redo, do))
		XORset = choice_node(list(XORset, Repeat))
	}

	sequence_node(list(do, XORset, exit)) %>% simplify %>% return

}
