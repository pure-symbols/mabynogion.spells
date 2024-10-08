


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



# todo: add .lapplier, wait to test ...
lapply_if = function (
		.ls, 
		.conds, 
		.f, 
		.lapplier = base::lapply, 
		..., 
		.all_conds = T, 
		.else_fn = base::identity) .ls |> 
	.lapplier(
		\ (x) (if (.conds |> conds_apply(.all_conds = .all_conds) |> fn_apply(x)) 
			.f else .else_fn) (x), 
		...) |> 
	base::identity()

#| > base::seq(4) |> lapply_if(\ (x) x %% 2 == 0, `-`) |> base::unlist()
#| [1]  1 -2  3 -4
#| > base::seq(4) |> lapply_if(base::c(\ (x) x %% 2 == 0), `-`) |> base::unlist()
#| [1]  1 -2  3 -4
#| > base::seq(4) |> lapply_if(base::c(\ (x) x %% 2 == 0, \ (x) x %% 2 != 0), `-`) |> base::unlist()
#| [1] 1 2 3 4
#| > base::seq(4) |> lapply_if(base::c(\ (x) x %% 2 == 0, \ (x) x %% 2 != 0), `-`, .all_conds = F) |> base::unlist()
#| [1] -1 -2 -3 -4

# todo: 除了分支 all any 也可分支短路 与或 : %&|% 并不用 lapply 而是在 reduce 里完成操作。
conds_apply = function 
(..., .conds = base::c(...), .all_conds = T) function 
(.term) base::c(.conds) |> 
	base::lapply(\ (f) f(.term)) |> 
	base::unlist() |> 
	(if (.all_conds) base::all else base::any)() |> 
	base::identity()


#| > conds_apply (base::c(\ (x) x %% 2 == 0)) (2)
#| [1] TRUE
#| > conds_apply (base::c(\ (x) x %% 2 == 0)) (3)
#| [1] FALSE
#| > base::Vectorize(conds_apply (base::c(\ (x) x %% 2 == 0))) (base::c(2,3))
#| [1]  TRUE FALSE
#| > conds_apply (base::c(\ (x) x %% 2 == 0, \ (x) x %% 2 != 0)) (3)
#| [1] FALSE
#| > conds_apply (base::c(\ (x) x %% 2 == 0, \ (x) x %% 2 != 0), .all_conds = F) (3)
#| [1] TRUE



