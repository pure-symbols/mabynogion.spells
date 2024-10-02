

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
#| > list(1*2,3+1-4/5,list(6*7)) |> quote() |> calls_asttr(\ (a) if (a[[1]] |> identical(quote(`*`))) `[[<-`(a, 2, value = 666) else a)
#| list(666 * 2, 3 + 1 - 4/5, list(666 * 7))





calls_varls = function (calls) calls |> 
	base::lapply(codes_call2ast) |> 
	base::lapply(\ (x) x |> ast_astapply(\ (a) base::list(base::list()) |> base::c(a |> utils::tail(-1)))) |> 
	base::lapply(\ (x) x |> ast_elemapply(\ (x) if (base::is.symbol(x)) x else base::list())) |> 
	base::lapply(base::unlist) |> 
	base::lapply(base::unique) |> 
	base::identity()

calls_funls = function (calls) calls |> 
	base::lapply(codes_call2ast) |> 
	base::lapply(\ (x) x |> ast_astapply(.f = base::identity, .f_aleaf = \ (x) x |> utils::head(1))) |> 
	base::lapply(base::unlist) |> 
	base::lapply(base::unique) |> 
	base::identity()



calls_vartr = function (calls, .f) calls |> 
	base::lapply(codes_call2ast) |> 
	base::lapply(\ (a) a |> ast_varsapply(.f)) |> 
	base::lapply(codes_ast2call) |> 
	base::identity()
