merge_sequential_paths <- function(a,b) {
	names(a) <- paste0("a_", names(a))
	names(b) <- paste0("b_", names(b))
	ab <- merge(a,b)
	ab$n <- ab$a_n * ab$b_n
	ab$length <- ab$a_length + ab$b_length
	return(ab %>% select(n,length))
}

par_seq <- function(n,k){
	return(choose((n+k),k))
}

merge_parrallel_paths <- function(a,b) {
	names(a) <- paste0("a_", names(a))
	names(b) <- paste0("b_", names(b))
	ab <- merge(a,b)
	ab$n <- ab$a_n * ab$b_n * mapply(par_seq, ab$a_length, ab$b_length)
	ab$length <- ab$a_length + ab$b_length
	return(ab %>% select(length,n))
}
