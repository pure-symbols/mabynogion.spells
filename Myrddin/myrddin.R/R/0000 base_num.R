

# sequence_div = '%seqdiv%' = function (whole_len, group_len) (
#   base::sequence(whole_len) + group_len - 1) %/% group_len

div_sup = `%///%` = function (a, b) a |> 
	magrittr::'%>%'({. + group_len - 1}) |> 
	magrittr::'%>%'({. %/% group_len}) |> 
	base::as.integer() |> 
	base::identity()

sequence_div = `%seqdiv%` = function (whole_len, group_len) (base::sequence(whole_len) %///% group_len)

#' 此处是数字，没必要转成 factor 。
#' 就算是 factor 本身也是数字实现的，没有意义。
#' 下面有它们空间占用的 benchmark 。
#' 
#'   base::as.factor() |> 
#' 
#' 唯一的优化空间在于，运算符 %/% 做得不够好。
#' 它明明确实只可能返回整数，它却还是返回浮点数。
#' 
#' 占用大小：
#' 整数 < 浮点数 << 浮点数 factor < 整数 factor
#' 
#' 但这个问题应当是运算符 %/% 自己去搞定的。
#' 

#| > (base::sequence(200) + 3) %/% 4 -> x
#| > x |> as.factor() -> y
#| > lobstr::obj_size(x)
#| 1.65 kB
#| > lobstr::obj_size(y)
#| 4.43 kB
#| > x |> forcats::as_factor() -> y
#| > lobstr::obj_size(y)
#| 4.43 kB
#| > x |> head()
#| [1] 1 1 1 1 2 2
#| > x[length(x)]
#| [1] 50
#| > base::gl(50,4,200) -> z
#| > y |> identical(z)
#| [1] TRUE
#| > lobstr::obj_size(z)
#| 5.02 kB
#| > lobstr::obj_size(as.integer(x))
#| 848 B
#| > lobstr::obj_size(as.integer(y))
#| 848 B
#| > lobstr::obj_size(as.integer(z))
#| 848 B
#| > base::gl(50,4.1,200) |> identical(base::gl(50.1,4,200))
#| [1] TRUE
#| > base::gl(50,4.1,200) |> identical(base::gl(50.1,3.9,200))
#| [1] FALSE
#| > base::gl(50,4.1,200) |> identical(base::gl(50.1,4.9,200.1))
#| [1] TRUE
#| > as.factor(as.integer(y)) -> k
#| > lobstr::obj_size(k)
#| 5.02 kB
#| > as.factor(as.numeric(z)) -> j
#| > lobstr::obj_size(j)
#| 4.43 kB

between = function (
		.x, 
		.morethan, 
		.lessthan, 
		.opcls = base::c(T, F), 
		..than_fns = base::list(
			.more = base::list(`>`, `>=`), 
			.less = base::list(`<`, `<=`)), 
		..fn_choosed = ..than_fns |> 
			liapply(
				.f = \ (x, i) x[[i]], 
				.i = .opcls + 1), 
		..than_choosed = base::list(
			.more = .morethan, 
			.less = .lessthan)) ..fn_choosed |> 
	liapply(
		.f = \ (f, than) .x |> f(than), 
		.i = ..than_choosed) |> 
	base::unlist() |> 
	base::all() |> 
	base::identity()


#| > 1 |> between(2,3)
#| [1] FALSE
#| > 1 |> between(0,3)
#| [1] TRUE
#| > "22q3" |> between('',3)
#| [1] TRUE
#| > "22q3" |> between('23q1','23q3')
#| [1] FALSE
#| > "23q2" |> between('23q1','23q3')
#| [1] TRUE


n_pom = function (n) if 
(base::is.infinite(n)) (n > 0) * 2 - 1 else if 
(n == 0) n else if 
(T) base::as.integer(n / base::abs(n))

n_lpom = function (n) base::as.logical(n_pom(n) + 1)


weisavg = function (xs, weis) xs * weis / sum(weis)

