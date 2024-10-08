

#' Check diff between two groups of RDS file
#'  with no KEY-FIELDS specifiable.
#' 
rdses_diff = function (
		path_same, 
		diff_path_base, 
		diff_path_comp, 
		.pathes = base::list(
			base = path_same |> base::file.path(diff_path_base),
			comp = path_same |> base::file.path(diff_path_comp)), 
		.files = .pathes |> base::lapply(base::list.files), 
		.future_plan = future::multisession, 
		.func_sort = dplyr::arrange_all) 
{
	.files_base_lack = .files$comp |> base::setdiff(.files$base)
	.files_comp_lack = .files$base |> base::setdiff(.files$comp)
	
	if (length(.files_base_lack) > 0) usethis::ui_warn("Lack in BASE: {usethis::ui_value(.files_base_lack)}")
	if (length(.files_comp_lack) > 0) usethis::ui_warn("Lack in COMP: {usethis::ui_value(.files_comp_lack)}")
	
	files_intersect = .files |> base::Reduce(x = _, f = base::intersect)
	base::names(files_intersect) = files_intersect
	
	usethis::ui_info("Comparing files: {usethis::ui_value(files_intersect)}")
	
	files_fullpath = .pathes |> base::lapply(\ (dir_path) name_as(
		.x = base::file.path(dir_path, files_intersect), 
		as_name = files_intersect))
	
	pathpairs = files_intersect |> 
		base::lapply(\ (file) base::list(
			base = files_fullpath$base[file], 
			compare = files_fullpath$comp[file]))
	
	future::plan(.future_plan)
	pathpairs |> 
		future.apply::future_lapply(
			\ (pathes) pathes |> 
				base::lapply(rds_loader) |> 
				base::lapply(data.table::as.data.table) |> 
				base::lapply(.func_sort) |> 
				base::lapply(data.table::as.data.table) |> 
				base::Reduce(x = _, f = diffdf::diffdf)) -> .reports
	
	.issues = .reports |> Filter(x = _, f = diffdf::diffdf_has_issues)
	
	(
		if (!base::length(.issues) > 0) "No issues found !!" else 
			"Have issue in: {usethis::ui_value(names(.issues))}"
		) |> 
		usethis::ui_info()
	
	base::return(base::list(
		dir_pathes = .pathes, 
		file_lack = base::list(
			base = .files_base_lack, 
			comp = .files_comp_lack), 
		issues = .issues,
		diff_reports = .reports))
}

report_loadissues = function (report, filenames = names(report$issues), vars = NULL) name_asself(filenames) |> 
	base::lapply(\ (filename) report$dir_pathes |> future.apply::future_lapply (\ (dirpath) diffdf::diffdf_issuerows(
		df = rds_loader(base::file.path(dirpath, filename)), 
		diff = report$issues[[filename]], 
		vars = vars)))



###############################################



# https://github.com/cran-task-views/HighPerformanceComputing/blob/master/HighPerformanceComputing.md
# 
# https://github.com/tarakc02/lazylist
# https://stackoverflow.com/questions/45015916/lazy-evaluation-of-a-list-element-in-r
# https://mailund.github.io/r-programmer-blog/2018/10/03/lazy-lists/

# 下面这个可能会卡死因为需要做的事情太多了

rdses_diff_l2 = function (
		path_same, 
		diff_path_base, 
		diff_path_comp, 
		.file_range = files.intersect |> length() |> seq(), 
		.pair_path = base::list(
			base = path_same |> base::file.path(diff_path_base),
			comp = path_same |> base::file.path(diff_path_comp)), 
		.pair_files = .pair_path |> base::lapply(base::list.files), 
		.keyfields, 
		files.intersect = .pair_files |> Reduce(x = _, f = base::intersect),
		.future_plan.files = \ (...) future::plan(future::cluster, ...), 
		.future_plan.pieces = \ (...) future::plan(future::multisession, ...), 
		.func_sort = dplyr::arrange_all) 
{
	.files_base_lack = .pair_files$comp |> setdiff(.pair_files$base)
	.files_comp_lack = .pair_files$base |> setdiff(.pair_files$comp)
	
	if (length(.files_base_lack) > 0) usethis::ui_warn("Lack in BASE: {usethis::ui_value(.files_base_lack)}")
	if (length(.files_comp_lack) > 0) usethis::ui_warn("Lack in COMP: {usethis::ui_value(.files_comp_lack)}")
	
	names(files.intersect) = files.intersect
	
	usethis::ui_info("Comparable files: {usethis::ui_value(files.intersect)}")
	
	pair_filespath = .pair_path |> lapply(\ (dir_path) name_as(
		.x = file.path(dir_path, files.intersect), 
		as_name = files.intersect))
	
	filespath_pairs = files.intersect |> 
		lapply(\ (file) list(
			base = pair_filespath$base[file], 
			compare = pair_filespath$comp[file]))
	
	.file_range = if (! is.null(.file_range)) .file_range else seq(pmin(6, length(filespath_pairs)))
	
	usethis::ui_info("Files Comparing: {usethis::ui_value(base::names(filespath_pairs[.file_range]))}")
	
	.future_plan.files(workers = pmin(future::availableCores(), length(.file_range)))
	filespath_pairs[.file_range] |> 
		furrr::future_imap(
			\ (pair_path, filename) 
			{
				pair_df = pair_path |> 
					lapply(readRDS) |> 
					lapply(data.table::as.data.table)
				
				pair_dfpieces = pair_df |> 
					lapply(
						\ (.d) .d |> 
							splidt_at(.keyfields) |> 
							lapply(data.table::as.data.table))
				
				pair_dfpiecekeys = pair_dfpieces |> lapply(names)
				
				keys.intersect = intersect(
					pair_dfpiecekeys$base, 
					pair_dfpiecekeys$compare)
				
				.keys_lack_base = pair_dfpiecekeys$base |> setdiff(keys.intersect)
				.keys_lack_compare = pair_dfpiecekeys$compare |> setdiff(keys.intersect)
				
				if (length(.keys_lack_base) > 0) usethis::ui_warn("Key of {usethis::ui_value(filename)}: Lack in BASE: {usethis::ui_value(.keys_lack_base)}")
				if (length(.keys_lack_compare) > 0) usethis::ui_warn("Key of {usethis::ui_value(filename)}: Lack in COMP: {usethis::ui_value(.keys_lack_compare)}")
				
				names(keys.intersect) = keys.intersect
				
				dfpiece_pairs = keys.intersect |> lapply(\ (k) list(
					BASE = pair_dfpieces$base[[k]], 
					COMP = pair_dfpieces$compare[[k]]))
				
				#' 这里在使用 *nix 系统时最优方案是用 `parallel::mclapply` （足够快但有可能 OOM 崩溃）
				#' 其它情况要用 future 的 `future::plan(future::multisession)` （或类似的）
				#' 
				#' .future_plan.pieces()
				#' .dfpiece_reports = dfpiece_pairs |> 
				#'   furrr::future_map(
				#'     \ (pair_dfpiece) diffdf::diffdf(
				#'       base = .func_sort(pair_dfpiece$BASE), 
				#'       compare = .func_sort(pair_dfpiece$COMP)))
				#' 
				#' 如果 `future::plan(future::multicore)` 不会仅仅因为在 R Studio 中使用就被禁止，就不用这么麻烦了。
				#' 它和 `parallel::mclapply` 效果一样，并且在语言层面统一为 future 风格（因而便于抽象）。
				#' 
				
				.dfpiece_reports = dfpiece_pairs |> 
					parallel::mclapply(
						mc.cores = getOption("mc.cores", parallel::detectCores() / 8), 
						FUN = \ (pair_dfpiece) diffdf::diffdf(
							base = .func_sort(pair_dfpiece$BASE), 
							compare = .func_sort(pair_dfpiece$COMP)), 
						X = _)
				
				.dfpiece_issues = .dfpiece_reports |> Filter(x = _, f = diffdf::diffdf_has_issues)
				
				.issue_keys = names(.dfpiece_issues) |> base::lapply(\ (x) base::parse(text = x)) |> base::lapply(eval) |> data.table::rbindlist()
				
				usethis::ui_info("Issue keys in file {usethis::ui_value(filename)}")
				print(.issue_keys); readr::write_csv(.issue_keys, glue::glue(".issue_keys__{filename}.csv"))
				
				return(list(
					key_lack = list(
						BASE = .keys_lack_base, 
						COMP = .keys_lack_compare), 
					key_issue = .issue_keys, 
					splidf_issues = .dfpiece_issues, 
					splidf_reports = .dfpiece_reports))
			}) -> .reports
	
	# .issues = .reports |> Filter(x = _, f = diffdf::diffdf_has_issues)
	
	.issues = .reports |> lapply(\ (report) report$splidf_issues) |> Filter(x = _, f = \ (splidf_issues) length(splidf_issues) > 0)
	
	if (length(.issues) > 0) usethis::ui_info("Have issue in: {usethis::ui_value(names(.issues))}") 
	else usethis::ui_info("No issues found !!")
	
	return(list(
		pair_dirpath = .pair_path, 
		file_lack = list(
			base = .files_base_lack, 
			comp = .files_comp_lack), 
		issues = .issues,
		diff_reports = .reports))
}

