
#' Bind function
#' 
`%bind%` = function (f.a, f.b) \ (...) f.b(f.a(...))

binder = function (..., .fns = base::list(...)) .fns |> looper_reduce(`%bind%`)




