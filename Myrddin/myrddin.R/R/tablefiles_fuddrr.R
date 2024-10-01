
combinecsv_workbook = function (
		path_csvdir, 
		path_xl, 
		.mc.cores = 6, 
		...) read_csvs(path_csvdir, .mc.cores = .mc.cores, ...) |> 
	writexl::write_xlsx(path_xl, ...)
