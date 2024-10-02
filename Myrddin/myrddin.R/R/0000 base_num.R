

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
