
time_stamp = function (
		prec = 3, 
		.time = base::Sys.time()) .time |> 
	base::as.numeric() |> 
	magrittr::'%>%'({. * 10^prec}) |> 
	base::round(digits = 0) |> 
	base::identity()

time_costed = `%time_cost%` = function (
		now = base::Sys.time(), 
		bgn, 
		rnd = 3, 
		format = T, 
		.f_fmtr = base::list(base::identity, base::format)[[1 + format]]) now |> 
	base::difftime(bgn) |> 
	base::round(rnd) |> 
	# base::as.character.numeric_version() |> 
	.f_fmtr() |> 
	base::identity()
