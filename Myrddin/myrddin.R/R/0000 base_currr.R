
currr_pair = \ (a) \ (b) \ (c) c (a) (b)
currr_tuple = \ (n, p = base::list()) if 
	(n > 0) \ (...) currr_tuple (n - 1, p |> list_append(...)) else if 
	(T) \ (f) p |> looper_reduce(fn_apply, .init = f)
