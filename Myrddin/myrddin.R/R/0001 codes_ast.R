
ast_astapply = function (
		a, 
		.f = base::identity, 
		.f_atree = .f, 
		.f_aleaf = .f, 
		.f_elem = base::identity) if 
(!base::is.list(a)) a else a |> 
	base::lapply (
		\ (as) if 
		(list_isnested(as)) as |> 
			ast_astapply(
				.f = .f, 
				.f_atree = .f_atree, 
				.f_aleaf = .f_aleaf, 
				.f_elem = .f_elem) |> .f_atree() else if 
		(base::is.list(as)) as |> .f_aleaf() else 
			as |> .f_elem()) |> 
	.f_atree() |> 
	base::identity()

ast_elemapply = function (ast, f) if 
(!base::is.list(ast)) ast else ast |> 
	purrr::map(
		\ (x) if 
		(!base::is.list(x)) f(x) else x |> 
			ast_elemapply(f)) |> 
	base::identity()


#| > list(1,2,3+1-4*8) |> quote() |> codes_call2ast() |> ast_elemapply(\ (a) if (identical(a,`*` |> quote())) `/` |> quote() else a) |> codes_ast2call()
#| list(1, 2, 3 + 1 - 4/8)


ast_varsapply = function (ast, f) if 
(!base::length(ast) > 1) ast |> base::lapply(f) else ast |> 
	ast_astapply(
		\ (a) a |> 
			utils::tail(-1) |> 
			base::lapply(\ (x) if (base::is.symbol(x) && !symbol_isgraved(x)) f(x) else x) |> 
			concat_head(base::list(a[[1]]) |> base::c())) |> 
	base::identity()





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
