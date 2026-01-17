

diffcols_ls = function (.df) .df |> 
	base::lapply(base::unique) |> 
	base::lapply(base::length) |> 
	base::lapply(\ (x) x > 1) |> 
	base::unlist() |> 
	magrittr::'%>%'({name_asi(base::names(.))[.]}) |> 
	base::identity()

diffcols_select = function (.df, .cols = diffcols_ls(.df)) .df |> 
	dplyr::select_at(.cols) |> 
	base::identity()
