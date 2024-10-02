
looper_reduce = function (.itr, .f, .init = NULL) 
{
	if (base::is.null(.init)) 
	{
		.init = .itr |> utils::head(1)
		.itr = .itr |> utils::tail(-1)
	}
	
	for (.i in .itr) {.init <- .f(.init, .i)}
	base::return(.init)
}


#| > seq(3) |> looper_reduce(`+`)
#| [1] 6
#| > seq(3) |> looper_reduce(`+`, .init = 7)
#| [1] 13
