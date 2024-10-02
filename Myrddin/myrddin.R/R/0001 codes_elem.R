





calls_varls = function (calls) calls |> 
	base::lapply(codes_call2ast) |> 
	base::lapply(\ (x) x |> codes_astapply_ast(\ (a) base::list(base::list()) |> base::c(a |> utils::tail(-1)))) |> 
	base::lapply(\ (x) x |> codes_astapply_elem(\ (x) if (base::is.symbol(x)) x else base::list())) |> 
	base::lapply(base::unlist) |> 
	base::lapply(base::unique) |> 
	base::identity()

calls_funls = function (calls) calls |> 
	base::lapply(codes_call2ast) |> 
	base::lapply(\ (x) x |> codes_astapply_ast(.f = base::identity, .f_aleaf = \ (x) x |> utils::head(1))) |> 
	base::lapply(base::unlist) |> 
	base::lapply(base::unique) |> 
	base::identity()

codes_lsby = function (strs, .lser) strs |> 
	base::Vectorize(codes_str2call)() |> 
	warpper_vec(.vec_ref = strs)(.lser)() |> 
	base::unlist() |> 
	base::unique() |> 
	base::lapply(base::as.character) |> 
	base::unlist()

codes_varls = function (strs) strs |> codes_lsby(calls_varls)
codes_funls = function (strs) strs |> codes_lsby(calls_funls)




calls_vartr = function (calls, .f) calls |> 
	base::lapply(codes_call2ast) |> 
	base::lapply(\ (a) a |> codes_astapply_var(.f)) |> 
	base::lapply(codes_ast2call) |> 
	base::identity()

codes_vartr = function (strs, .f) name_asself(strs) |> 
	base::lapply(
		\ (str) str |> 
			codes_str2call() |> 
			calls_vartr(.f) |> 
			codes_call2str() |> 
			base::identity()) |> 
	base::identity()


#| > base::c('1+2-3*x~4/5^6;{abc(def)};ghi(jkl = mno,.pqr = stu)', '789', '10 * 1', '11/R') |> codes_vartr(symbol_gravewarp)
#| $`1+2-3*x~4/5^6;{abc(def)};ghi(jkl = mno,.pqr = stu)`
#| [1] "1 + 2 - 3 * `\\`x\\`` ~ 4/5^6"              "{\n    abc(`\\`def\\``)\n}"                 "ghi(jkl = `\\`mno\\``, .pqr = `\\`stu\\``)"
#| 
#| $`789`
#| [1] "(`\\`789\\``)"
#| 
#| $`10 * 1`
#| [1] "10 * 1"
#| 
#| $`11/R`
#| [1] "11/`\\`R\\``"
