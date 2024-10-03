
#' List update
#' 
list_upsert = function (.ls, key, value) base::'[[<-'(.ls, key, value)
list_update = function (.ls, ..., .news = base::list(...)) base::c(.ls, .news) |> 
	magrittr::'%>%'(base::split(base::names(.))) |> 
	base::lapply(\ (terms) terms[[base::length(terms)]]) |> 
	base::identity()

#| > list(A = 1, B = 2) |> list_update(A = 3, D = 4)
#| $A
#| [1] 3
#| 
#| $B
#| [1] 2
#| 
#| $D
#| [1] 4
#| 



list_append = function (.lst, ..., ..pairs = base::list(...)) .lst |> concat_tail(..pairs)
list_follow = function (.lst, ..., ..pairs = base::list(...)) .lst |> concat_head(..pairs)

list_pairs = function (.name, .value) base::list(.value) |> name_as(.name)

#| > list(a = 1, b = 2) |> list_append(c = 7)
#| $a
#| [1] 1
#| 
#| $b
#| [1] 2
#| 
#| $c
#| [1] 7
#| 


list_isnested = function (lst) base::is.list(lst) && lst |> 
	base::lapply(base::is.list) |> 
	base::Reduce(
		# f = base::.Primitive("||"),
		f = \ (a, b) a || b,
		x = _)


concat_head = function (.x, .head) .head |> base::c(.x)
concat_tail = function (.x, .tail) .x |> base::c(.tail)


list_headis = function (
		.list, 
		..., 
		.comps = base::list(...), 
		.n = base::length(.comps), 
		.comparer = base::identical) .list |> 
	utils::head(.n) |> 
	.comparer(.comps) |> 
	base::identity()

#| > list(quote(`+`), quote(`*`), quote(`::`)) |> list_headis(quote(`+`))
#| [1] TRUE
#| > list(quote(`+`), quote(`*`), quote(`::`)) |> list_headis(quote(`+`), quote(`*`))
#| [1] TRUE

liapply = function (
		.x, 
		.f, 
		..., 
		.i = name_i(.x), 
		.use_names = T, 
		.simplify = F, 
		.args_more = NULL) .f |> 
	base::match.fun() |> 
	base::mapply(
		FUN = _, 
		.x, 
		.i, 
		..., 
		MoreArgs = .args_more, 
		SIMPLIFY = .simplify, 
		USE.NAMES = .use_names) |> 
	base::identity()

#| > base::c(a = 1, b = 2) |> liapply(\ (.x, .i) paste(.i, .x))
#| $a
#| [1] "a 1"
#| 
#| $b
#| [1] "b 2"
#| 
#| > base::c(a = 1, b = 2) |> liapply(paste)
#| $a
#| [1] "1 a"
#| 
#| $b
#| [1] "2 b"
#| 
#| > base::c(a = 1, b = 2) |> liapply(\ (x, i) paste(i, x))
#| $a
#| [1] "a 1"
#| 
#| $b
#| [1] "b 2"
#| 


