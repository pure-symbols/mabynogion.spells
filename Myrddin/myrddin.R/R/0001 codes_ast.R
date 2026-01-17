
ast_astapply = function (
		a, 
		.f = base::identity, 
		.f_atree = .f, 
		.f_aleaf = .f, 
		.f_elem = base::identity) if 
(!base::is.list(a)) a else a |> 
	base::lapply(
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



# ast_astfold = function (
# 		.a, 
# 		.f, 
# 		.init, 
# 		.f_atree = .f, 
# 		.f_aleaf = .f, 
# 		.f_elem = base::identity) if 
# (!base::is.list(a)) .init else a |> 
# 	looper_reduce(
# 		.iter = _, 
# 		.f = , 
# 		.init = .init) |> 
# 	base::identity()





ast_elemapply = function (ast, f) if 
(!base::is.list(ast)) ast else ast |> 
	base::lapply(
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



astelem_i = function (.a, .i) if (!.i < 0) .a |> elem_i(.i + 1) else repeats(.n = .i)
astelem_body = function (.a, .drop_num = 1) .a |> utils::tail(- .drop_num)

