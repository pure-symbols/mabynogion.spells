
collase_0 = function (.na = NA, .default = NA) function (.xs = base::list(...), ...) .xs |> 
	base::Filter(x = _, f = \ (x) !(x %in% base::unlist(.na))) |> 
	base::append(.default) |> 
	magrittr::'%>%'(.[[1]]) |> 
	base::identity()

collase = function (
		..., 
		.xs = base::list(...), 
		.na = NA, 
		.default = NA, 
		.simplify = T, 
		.use_names = F) .xs |> 
	xrosslist(.silent = T) |> 
	base::lapply(collase_0 (.na, .default)) |> 
	magrittr::'%>%'({if (.simplify) base::unlist(.) else .}) |> 
	magrittr::'%>%'({if (.use_names) . else . |> name_as(NULL)}) |> 
	base::identity()

pcollase = function (
		..., 
		.na = NA, 
		.default = NA) ... |> 
	collase(
		.na = .na, 
		.default = .default, 
		.simplify = T, 
		.use_names = F)

# collase(c(T,NA,NA), c(3,NA,2), c(NA,9,5)) #> [1] 1 9 2
# pcollase(c(T,NA,NA), c(3,NA,2), c(NA,9,5)) #> [1] 1 9 2
# pcollase(c(T,NA,NA), c("a",NA,""), c(NA,9,5), .na = list(NA,"")) #> [1] 1 9 5
