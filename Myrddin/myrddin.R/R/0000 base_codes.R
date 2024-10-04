
codes_call2ast = function (calls) calls |> 
	base::as.list() |> 
	lapply_if(
		.conds = conds_apply (
			base::is.pairlist, 
			base::is.call, 
			.all_conds = F), 
		.f = codes_call2ast)

codes_ast2call = function (ast) ast |> 
	magrittr::'%>%'({if 
		(!base::length(.) > 1) base::quote(`(`) |> 
			base::list() |> 
			base::c(.) else .}) |> 
	base::lapply(
		\ (xs) if 
		(list_isnested(xs)) codes_ast2call(xs) else if 
		(base::is.list(xs)) base::as.call(xs) else 
			xs) |> 
	base::as.call() |> 
	base::identity()


#| > code <- {res = case_when (.size = NULL, numcase==1~ dplyr ::case_when(numcase==1~1.33, numcase == 3 ~ 1.30, numcase >=5~ 1.24) ); 1} |> base::quote()
#| > code |> codes_call2ast() |> codes_ast2call()
#| {
#|     res = case_when(.size = NULL, numcase == 1 ~ dplyr::case_when(numcase == 
#|         1 ~ 1.33, numcase == 3 ~ 1.3, numcase >= 5 ~ 1.24))
#|     1
#| }


codes_str2call = function (
		str, 
		.exp = if 
		(base::is.null(path)) base::parse(text = str) else 
			base::parse(file = path), 
		path = NULL) if 
(!base::length(.exp) > 0) base::list() else base::as.call(.exp) |> 
	magrittr::'%>%'({base::'names<-'(base::as.list(.), base::as.character(.))}) |> 
	base::identity()

codes_call2str = function (calls) base::as.character(base::as.expression(calls))

