
codes_astapply_ast = function (
		a, 
		.f = base::identity, 
		.f_atree = .f, 
		.f_aleaf = .f, 
		.f_elem = base::identity) if 
(!base::is.list(a)) a else a |> 
	base::lapply (
		\ (as) if 
		(list_isnested(as)) as |> 
			codes_astapply_ast(
				.f = .f, 
				.f_atree = .f_atree, 
				.f_aleaf = .f_aleaf, 
				.f_elem = .f_elem) |> .f_atree() else if 
		(base::is.list(as)) as |> .f_aleaf() else 
			as |> .f_elem()) |> 
	.f_atree() |> 
	base::identity()

codes_astapply_elem = function (ast, f) if 
(!base::is.list(ast)) ast else ast |> 
	purrr::map(
		\ (x) if 
		(!base::is.list(x)) f(x) else x |> 
			codes_astapply_elem(f)) |> 
	base::identity()


#| > list(1,2,3+1-4*8) |> quote() |> codes_call2ast() |> codes_astapply_elem(\ (a) if (identical(a,`*` |> quote())) `/` |> quote() else a) |> codes_ast2call()
#| list(1, 2, 3 + 1 - 4/8)


codes_astapply_var = function (ast, f) if 
(!base::length(ast) > 1) ast |> base::lapply(f) else ast |> 
	codes_astapply_ast(
		\ (a) a |> 
			utils::tail(-1) |> 
			base::lapply(\ (x) if (base::is.symbol(x) && !symbol_isgraved(x)) f(x) else x) |> 
			concat_head(base::list(a[[1]]) |> base::c())) |> 
	base::identity()





codes_calltransby = function (
		calls, 
		.transer, 
		.f) calls |> 
	codes_call2ast() |> 
	.transer(.f) |> 
	codes_ast2call()

codes_calltr_elem = function (calls, .f) codes_calltransby(
	.transer = codes_astapply_elem,
	.f = .f,
	calls = calls)

codes_calltr_ast = function (calls, .f) codes_calltransby(
	.transer = codes_astapply_ast,
	.f = .f,
	calls = calls)

#| > list(1,2,3+1-4*5) |> quote() |> codes_calltr_elem(\ (a) if (a |> identical(quote(`*`))) quote(`/`) else a) |> codes_ast2call()
#| list(1, 2, 3 + 1 - 4/5)
#| > list(1*2,3+1-4/5,list(6*7)) |> quote() |> codes_calltr_ast(\ (a) if (a[[1]] |> identical(quote(`*`))) `[[<-`(a, 2, value = 666) else a)
#| list(666 * 2, 3 + 1 - 4/5, list(666 * 7))
