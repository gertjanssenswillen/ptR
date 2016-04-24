#' @title Number of paths
#'
#' @description Calculate number of execution paths in a process tree
#'
#' @export number_of_paths

number_of_paths <- function(tree, n_iterations_loop = 2) {
	path_dictionary(tree, n_iterations_loop = n_iterations_loop)$n %>% sum %>% return()
}










