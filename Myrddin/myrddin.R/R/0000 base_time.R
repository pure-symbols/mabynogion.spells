


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




#| > lubridate::as_datetime('2020-05-01T10:11:12.345+08:00') |> timechange::time_add(month = 1, day = -1)
#| [1] "2020-05-31 02:11:12 UTC"
#| > lubridate::as_datetime('2020-05-01T10:11:12.345+08:00') |> timechange::time_add(day = -1, month = 1)
#| [1] "2020-05-31 02:11:12 UTC"
#| > lubridate::as_datetime('2020-05-01T10:11:12.345+08:00') |> timechange::time_add(day = -1) |> timechange::time_add(month = 1)
#| [1] "2020-05-30 02:11:12 UTC"



