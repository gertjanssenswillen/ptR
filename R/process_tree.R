#' @title Process Tree
#'
#' @description Read process tree from newick file
#'
#' @export process_tree

process_tree <- function(nw_file = file.choose()) {
	read.tree(nw_file) %>% as.Node
}
