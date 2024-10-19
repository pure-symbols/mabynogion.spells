
currr_pair = \ (a) \ (b) \ (c) c (a) (b)
currr_tuple = \ (n, p = base::list()) if 
(n != 0) \ (...) currr_tuple (
	n = n - n_pom(n), 
	p = p |> list_concat(..., .tail_order = !n_lpom(n))) else if 
(n == 0) \ (f) p |> reduce(fn_apply, .init = f)

#| > currr_tuple (2) (1) ("A") (\ (x) \ (y) base::list(x = x, y = y))
#| $x
#| [1] 1
#| 
#| $y
#| [1] "A"
#| 
#| > currr_pair (1) ("A") (\ (x) \ (y) base::list(x = x, y = y))
#| $x
#| [1] 1
#| 
#| $y
#| [1] "A"
#| 

