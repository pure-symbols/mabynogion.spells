
#' Bind function
#' 
`%bind%` = `%.%` = function (f.a, f.b) \ (...) f.b(f.a(...))

binder = function (..., .fns = base::list(...)) .fns |> looper_reduce(`%bind%`)


# todo: not very good inside other func define.
fn_name = function (.x) .x |> 
	base::substitute() |> 
	base::as.expression() |> 
	base::as.character()


fn_apply = function (.f, ...) .f(...)
