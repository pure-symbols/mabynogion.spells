
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
list_concat = function (
		.lst, 
		..., 
		..pairs = base::list(...), 
		.tail_order = F, 
		..concater = if (!.tail_order) list_append else list_follow) .lst |> 
	..concater(..pairs = ..pairs) |> 
	base::identity()

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

# todo: 这边也可以补一个短路版本，和 conds_apply 一样，不是用 lapply 而是用 reduce 、并在里面对 lst 下的每一个判断都用短路或。
list_isnested = function (lst) base::is.list(lst) && lst |> 
	base::lapply(conds_apply (
		.conds = base::c(
			base::is.list, 
			base::is.pairlist), 
		.all_conds = F)) |> 
	base::unlist() |> 
	base::any()


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


elem_i = function (.x, .i) .x |> 
	utils::head(.i) |> 
	utils::tail(1) |> 
	base::identity()

nil = base::list()

repeats = function (.x = NULL, .n = 0) .n |> 
	base::abs() |> 
	len_as(.len = _) |> 
	looper_reduce(
		\ (a, b) a |> list_append(if (!.n < 0) .x else nil), 
		.init = nil) |> 
	base::identity()




