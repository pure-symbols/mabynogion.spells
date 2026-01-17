


`%l1apply%` = l1apply = function (.xs, .f, ...) .xs |> base::lapply (.f, ...)
`%l2apply%` = l2apply = function (.xss, .f, ...) .xss |> l1apply (\ (.xs) .xs |> l1apply (.f, ...), ...) # .xss %l1apply% (\(.xs) .xs %l1apply% .f)
`%l3apply%` = l3apply = function (.xsss, .f, ...) .xsss |> l1apply (\ (.xss) .xss |> l2apply (.f, ...), ...) # .xsss %l1apply% (\(.xss) .xss %l2apply% .f)


lnapplyer = function (n, .by = 1) base::seq(from = 1, to = n, by = .by) |> 
	magrittr::'%>%'({base::list(
		n_by = .by,
		n_next = . + .by,
		n_curr = .)}) |> 
	glue::glue_data_safe(
		'`%l{n_next}apply%` <- l{n_next}apply <- \\
		function (.termsls, .f, ...) .termsls |> l{n_by}apply (\\ (.terms) .terms |> \\
		l{n_curr}apply (.f, ...), ...)') |> 
	base::append('`%l1apply%` <- l1apply <- function (.terms, .f, ...) .terms |> base::lapply (.f, ...)') |> 
	# base::paste(collapse = '; ') |> 
	# base::paste0('{',codes = _,'}') |> 
	base::parse(text = _) |>
	base::identity()

# base::eval(lnapplyer(3))


