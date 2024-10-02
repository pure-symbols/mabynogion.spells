
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


#| > seq(3) |> looper_reduce(`+`)
#| [1] 6
#| > seq(3) |> looper_reduce(`+`, .init = 7)
#| [1] 13
