



codes_lsby = function 
(lister) function 
(strs, ...) strs |> 
	base::Vectorize(codes_str2call)() |> 
	warpper_vec(.vec_ref = strs)(lister)(...) |> 
	base::lapply(
		base::unlist %bind% 
			base::unique %bind% 
			base::as.character) |> 
	base::identity()

codes_varls = function (strs, ...) strs |> codes_lsby(calls_varls)(...)
codes_funls = function (strs, ...) strs |> codes_lsby(calls_funls)(...)




codes_trby = function 
(transer) function 
(strs, ...) name_asself(strs) |> 
	base::lapply(
		\ (str) str |> 
			codes_str2call() |> 
			transer(...) |> 
			codes_call2str() |> 
			base::identity()) |> 
	base::identity()

codes_vartr = function (strs, .f, ...) strs |> codes_trby(calls_vartr)(.f = .f, ...)


#| > base::c('1+2-3*x~4/5^6;{abc(def)};ghi(jkl = mno,.pqr = stu)', '789', '10 * 1', '11/R') |> codes_vartr(symbol_gravewarp)
#| $`1+2-3*x~4/5^6;{abc(def)};ghi(jkl = mno,.pqr = stu)`
#| [1] "1 + 2 - 3 * `\\`x\\`` ~ 4/5^6"             
#| [2] "{\n    abc(`\\`def\\``)\n}"                
#| [3] "ghi(jkl = `\\`mno\\``, .pqr = `\\`stu\\``)"
#| 
#| $`789`
#| [1] "(`\\`789\\``)"
#| 
#| $`10 * 1`
#| [1] "10 * 1"
#| 
#| $`11/R`
#| [1] "11/`\\`R\\``"
#| 



codes_pkgls = function (strs, ...) strs |> codes_lsby(calls_pkgls)(...)

