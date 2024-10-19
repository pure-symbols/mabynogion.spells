


#' The Loops are having bugs with a curring function acc for now, see: 
#' - https://stackoverflow.com/questions/79105064/r-the-for-loop-may-have-bug-while-work-with-the-acc-is-a-function-but-why
#' - https://github.com/r-wasm/webr/issues/493
#' 
looper_reduce = function (.iter, .f, .init = NULL) 
{
	if (base::is.null(.init)) 
	{
		.init = .iter |> utils::head(1)
		.iter = .iter |> utils::tail(-1)
	}
	
	for (.i in .iter) {.init <- .f(.init, .i)}
	base::return(.init)
}


fold = function (
		.iter, .f, .init, 
		.iter_fn = if 
		(base::is.data.frame(.iter)) base::c(
			len = base::nrow, 
			head = \ (.x) .x |> utils::head(1), 
			tail = \ (.x) .x |> utils::tail(-1)) else if 
		(base::is.list(.iter)) base::c(
			len = base::length, 
			head = \ (.x) .x |> elem_i(1, .unlist = T), 
			tail = \ (.x) .x |> utils::tail(-1)) else if 
		(base::is.vector(.iter)) base::c(
			len = base::length, 
			head = \ (.x) .x |> utils::head(1), 
			tail = \ (.x) .x |> utils::tail(-1)) else if 
		(T) base::c(
			len = base::length, 
			head = \ (.x) .x |> utils::head(1), 
			tail = \ (.x) .x |> utils::tail(-1))) if 
(.iter_fn$len(.iter) != 0) .iter_fn$tail(.iter) |> 
	fold(.f, .init |> .f(.iter_fn$head(.iter))) |> 
	base::identity() else if 
(.iter_fn$len(.iter) == 0) .init |> 
	base::identity()

reduce = function (.iter, .f, .init = NULL) 
{
	if (base::is.null(.init)) 
	{
		.init = .iter |> utils::head(1)
		.iter = .iter |> utils::tail(-1)
	}
	
	.iter |> fold(.f, .init) |> base::return()
}


#| > seq(3) |> looper_reduce(`+`)
#| [1] 6
#| > seq(3) |> looper_reduce(`+`, .init = 7)
#| [1] 13

#| > seq(3) |> reduce(`+`)
#| [1] 6
#| > seq(3) |> reduce(`+`, .init = 7)
#| [1] 13

#| > tibble::tibble(x = 1:4, y = 2:5)
#| # A tibble: 4 × 2
#|       x     y
#|   <int> <int>
#| 1     1     2
#| 2     2     3
#| 3     3     4
#| 4     4     5
#| > tibble::tibble(x = 1:4, y = 2:5) |> reduce(\ (rowa, rowb) rowb |> base::rbind(rowa))
#| # A tibble: 4 × 2
#|       x     y
#|   <int> <int>
#| 1     4     5
#| 2     3     4
#| 3     2     3
#| 4     1     2




scanover = function 
(f) function 
(field, row_number = dplyr::row_number()) row_number |>
	base::lapply(base::seq) |>
	base::lapply(\ (n) field[n]) |>
	base::lapply(f) |>
	base::unlist()

overacc_sum = function (field) scanover(base::sum)(field)

