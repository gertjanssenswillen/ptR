
simplify <- function(paths){
	paths %>% group_by(length) %>% summarize(n = sum(n)) %>% select(length,n)
}
