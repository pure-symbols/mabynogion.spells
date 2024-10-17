


kount = function (
		nums, 
		.base = base::length(nums) * (.by * 2), 
		.by = 1, 
		.unbase = F) (nums) |> 
	looper_reduce(
		.f = \ (a, b) base::seq.int(from = a$n, length.out = b, by = .by) |> 
			magrittr::'%>%'({base::list(
				m = a$m |> list_append(..pairs = b |> list_pairs(.)), 
				n = .[base::length(.)])}),
		.init = base::list(m = base::list(), n = 0)) |> 
	(if (.unbase) base::identity else \ (.) base::list(
		m = .$m |> base::lapply(\ (x) x %% .base), 
		n = .$n %% .base))() |> 
	base::identity()


#| > kount(base::c(3,6,9))
#| $m
#| $m$`3`
#| [1] 0 1 2
#| 
#| $m$`6`
#| [1] 2 3 4 5 0 1
#| 
#| $m$`9`
#| [1] 1 2 3 4 5 0 1 2 3
#| 
#| 
#| $n
#| [1] 3
#| 
#| > kount(base::c(3,6,9), .unbase = T)
#| $m
#| $m$`3`
#| [1] 0 1 2
#| 
#| $m$`6`
#| [1] 2 3 4 5 6 7
#| 
#| $m$`9`
#| [1]  7  8  9 10 11 12 13 14 15
#| 
#| 
#| $n
#| [1] 15
#| 

