
currr_pair = \ (a) \ (b) \ (c) c (a) (b)
currr_tuple = \ (n, p = base::list()) if 
	(n != 0) \ (...) (
		\ (..param_concater) currr_tuple (n - n_pom(n), p |> ..param_concater(...))) (
		..param_concater = if (n_lpom(n)) list_append else list_follow) else if 
	(n == 0) \ (f) p |> looper_reduce(fn_apply, .init = f)
