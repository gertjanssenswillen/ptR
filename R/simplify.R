
simplify <- function(paths){
	paths %>% group_by(length, p) %>% summarize(n = sum(n)) %>% select(n, length, p) %>% ungroup()
}
