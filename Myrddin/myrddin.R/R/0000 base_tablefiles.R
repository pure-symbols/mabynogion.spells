
# TODO use rio to read ? ...
# https://gesistsa.github.io/rio/
# https://github.com/gesistsa/rio
# https://cran.r-project.org/web/packages/rio/

read_csvs = function (path, .mc.cores = 6, ...) list_files(path) |> 
	parallel::mclapply(
		mc.cores = .mc.cores, 
		\ (filepath) filepath |> readr::read_csv(...)) |> 
	base::identity()

readxl_sheets = function (path, ...) 
{
	sheet_names = readxl::excel_sheets(path)
	base::names(sheet_names) = sheet_names
	
	usethis::ui_info("Loading sheets: {usethis::ui_value(sheet_names)}")
	
	sheet_names |> 
		base::lapply(\ (sheet_name) readxl::read_excel(
			path = path, 
			sheet = sheet_name, 
			...)) |> 
		base::identity()
}

# https://stackoverflow.com/questions/65511615/several-csv-files-into-one-xlsx-with-multiple-tabs-in-r
# https://stackoverflow.com/questions/27524472/list-of-data-frames-to-individual-excel-worksheets-r
# 
# https://stackoverflow.com/questions/27459758/write-a-list-in-r-to-an-excel-file
# https://stackoverflow.com/questions/48029854/export-data-frames-in-list-to-xlsx-with-named-sheets
