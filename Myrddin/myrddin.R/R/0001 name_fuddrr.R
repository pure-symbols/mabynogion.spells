
flat_names = function (.ls) base::names(.ls) |> 
	base::Map(
		f = \ (x, n) name_as(
			as_name = base::rep(n, base::length(x)),
			.x = x), 
		x = .ls, 
		n = _) |> 
	name_as(NULL) |> 
	base::unlist(use.names = T)

#| > list(a = seq(2), b = 1) |> flat_names()
#| a a b 
#| 1 2 1 

freq_names = function (
		.x, 
		..flx = flat_names(.x)) base::unique(..flx) |> 
	name_asself() |> 
	base::lapply(\ (x) ..flx[..flx %in% x]) |> 
	base::lapply(base::names)

#| > list(a = seq(2), b = 1) |> freq_names()
#| $`1`
#| [1] "a" "b"
#| 
#| $`2`
#| [1] "a"
#| 
