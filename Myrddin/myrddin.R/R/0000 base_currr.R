
currr_pair = \ (a) \ (b) \ (c) c (a) (b)
currr_invpair = \ (a) \ (b) currr_pair (b) (a) # inverting pair

currr_pair.head = \ (t) t (\ (a) \ (b) a)
currr_pair.tail = \ (t) t (\ (a) \ (b) b)

#| > currr_pair (2) ('X') |> currr_pair.head ()
#| [1] 2
#| > currr_pair (2) ('X') |> currr_pair.tail ()
#| [1] "X"
#| > currr_invpair (2) ('X') |> currr_pair.head ()
#| [1] "X"
#| > currr_invpair (2) ('X') |> currr_pair.tail ()
#| [1] 2

# currr_stac.na = currr_pair (base::c) (base::c)
# currr_stac.isna = \ (t) t (
# 	\ (a) \ (b) base::c |> base::identical(a) && 
# 		base::c |> base::identical(b))
# 
# #| > currr_wild.na |> currr_wild.isna ()
# #| [1] TRUE
# #| > currr_pair (base::c) (base::c) |> currr_wild.isna ()
# #| [1] TRUE
# #| > currr_pair ('') (base::c) |> currr_wild.isna ()
# #| [1] FALSE
# #| > currr_pair (base::c) ('') |> currr_wild.isna ()
# #| [1] FALSE


currr_stac.na = currr_pair (NA) (NA)

currr_stac.pip = \ (x) \ (t) currr_pipe (t) (currr_pair (x))
currr_stac.pop = \ (f) \ (t) {currr_pair.head (t) |> f (); currr_pair.tail (t)}
currr_stac.ls = \ (
		.outr = base::list(), 
		.outr_follow = \ (.ls, .x) .ls |> list_follow(..pairs = .x), 
		.is_na = base::is.na) \ 
(t) if 
(.is_na (currr_pair.head (t))) .outr else if 
(!.is_na (currr_pair.head (t))) currr_pipe (
	currr_pair.tail (t)) (currr_stac.ls (
		currr_pair.head (t) |> .outr_follow(.outr)))

#| > currr_pipeline (currr_stac.na) (currr_stac.pip (1)) (currr_stac.pip ("a")) (currr_stac.ls ()) |> currr_pipeline.broadcast()
#| [[1]]
#| [1] "a"
#| 
#| [[2]]
#| [1] 1
#| 


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

currr_pipe = \ (x) \ (f) f (x)
# currr_pipeline = \ (x) \ (f) currr_pipeline (f (x))
currr_pipeline = \ (x) {base::force(x); \ (f) currr_pipeline (f (x))}
currr_pipeline.broadcast = \ (p) {r = base::c(); p (\ (x) r <<- x); r}

#| currr_pipeline (0) (\ (x) x + 1) (\ (x) x * 2) |> currr_pipeline.broadcast()
#| [1] 2
#| > currr_pipeline (currr_pair (1) (2)) (currr_pair.head) |> currr_pipeline.broadcast()
#| [1] 1
#| > currr_pipeline (currr_pair (1) (2)) (currr_pair.tail) |> currr_pipeline.broadcast()
#| [1] 2





# /////////// 下方施工


currr_wildconcat = \ (p) \ (a) \ (b) p (a) (b)

currr_wildc = currr_wildconcat (currr_pair)
currr_wildcinv = currr_wildconcat (currr_invpair)

currr_wildcon = \ (.inverting = F) if 
(!.inverting) currr_wildc else if 
(.inverting) currr_wildcinv


currr_wildtuple.append = \ (x) \ (t) currr_wildcon (T) (x) (t)
currr_wildtuple = \ (n, p = currr_wildtuple.na) if 
(n != 0) \ (...) currr_tuple (
	n = n - n_pom(n), 
	p = p |> list_concat(..., .tail_order = !n_lpom(n))) else if 
(n == 0) \ (f) p |> reduce(fn_apply, .init = f)















