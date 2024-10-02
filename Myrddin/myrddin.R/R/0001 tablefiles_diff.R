
readxl_diffsheets = function (
		path_base, 
		path_comp, 
		pathes = base::list(
			base = path_base,
			comp = path_comp), 
		...) 
{
	usethis::ui_info("Loading workbooks: {usethis::ui_value(pathes)}")
	
	sheets_names = pathes |> base::lapply(readxl::excel_sheets)
	sheets_intersect = sheets_names |> base::Reduce(x = _, f = base::intersect)
	base::names(sheets_intersect) = sheets_intersect
	
	sheets_names |> 
		purrr::map(~ base::setdiff(x = .x, y = sheets_intersect)) |> 
		purrr::imap(\ (x, n) usethis::ui_info(
			"Sheets only in {usethis::ui_value(n)}: {usethis::ui_value(x)}"))
	
	usethis::ui_info("Diffing sheets: {usethis::ui_value(sheets_intersect)}")
	
	sheets_intersect |> 
		base::lapply(
			\ (sheet_name) pathes |> 
				base::lapply(\ (path) readxl::read_excel(
					path = path, 
					sheet = sheet_name, 
					...)) |> 
				base::Reduce(
					f = \ (a,b) diffdf::diffdf(
						base = a, 
						compare = b, 
						...), 
					x = _)) |> 
		base::identity()
}
