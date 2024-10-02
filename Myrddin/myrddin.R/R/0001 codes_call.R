

calls_trby = function (
		calls, 
		.transer, 
		.f) calls |> 
	codes_call2ast() |> 
	.transer(.f) |> 
	codes_ast2call()

calls_elemtr = function (calls, .f) calls_trby(
	.transer = ast_elemapply,
	.f = .f,
	calls = calls)

calls_asttr = function (calls, .f) calls_trby(
	.transer = ast_astapply,
	.f = .f,
	calls = calls)

#| > list(1,2,3+1-4*5) |> quote() |> calls_elemtr(\ (a) if (a |> identical(quote(`*`))) quote(`/`) else a) |> codes_ast2call()
#| list(1, 2, 3 + 1 - 4/5)
#| > list(1*2,3+1-4/5,list(6*7)) |> quote() |> calls_asttr(\ (a) if (a[[1]] |> identical(quote(`*`))) a |> df_colmutate(2, 666) else a)
#| list(666 * 2, 3 + 1 - 4/5, list(666 * 7))



calls_lsby = function 
(listers) function 
(calls, ...) calls |> 
	base::lapply(codes_call2ast) |> 
	looper_reduce(
		.f = \ (a, lister) a |> base::lapply(lister),
		.iter = listers,
		.init = _) |> 
	base::lapply(base::unlist) |> 
	base::lapply(base::unique) |> 
	base::identity()

calls_varls = function (calls, ...) calls |> 
	calls_lsby(base::list(
		head_rm = \ (x) x |> ast_astapply(\ (a) a |> utils::tail(-1) |> list_follow(base::list())), 
		tail_symbol = \ (x) x |> ast_elemapply(\ (x) if (base::is.symbol(x)) x else base::list()), 
		.z = base::identity))(...) |> 
	base::identity()

calls_funls = function (calls, ...) calls |> 
	calls_lsby(base::list(
		leave_func = \ (x) x |> ast_astapply(.f = base::identity, .f_aleaf = \ (x) x |> utils::head(1)), 
		.z = base::identity))(...) |> 
	base::identity()

# todo ...
calls_pkgls = function (calls, ...) calls |> 
	calls_lsby(base::list(
		leave_func = \ (x) x |> ast_astapply(
			\ (a) if (
				FALSE 
				|| a |> list_headis(base::quote(`::`)) 
				|| a |> list_headis(base::quote(`library`)) 
				|| F) a |> utils::tail(-1) |> utils::head(1) else base::list()), 
		.z = base::identity))(...) |> 
	base::identity()


# https://wiki.mabinogiworld.com/view/Finest_Finishing_Thread
# https://gweiadur.com/welsh-dictionary/cloi;1



calls_vartr = function (calls, .f, ...) calls |> 
	base::lapply(codes_call2ast) |> 
	base::lapply(\ (a) a |> ast_varsapply(.f)) |> 
	base::lapply(codes_ast2call) |> 
	base::identity()
