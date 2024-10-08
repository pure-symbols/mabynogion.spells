.splidiff_demos = \ () 
{
	splidiff_rds(
		dir_base = '~/a', 
		dir_compare = '~/b', 
		file_name = "x_1.rds", 
		key_fields = 'homeworld') -> splidiff_report
	
	rdses_diff(
		path_same = '~/data',
		diff_path_base = 'a/dw',
		diff_path_comp = 'b/dw', 
		.future_plan = future::multisession) -> diffs_a7sc_expected
}


# https://nbisweden.github.io/raukr-2023/slides/vectorization/
# https://nbisweden.github.io/raukr/
# https://github.com/NBISweden/raukr

splidiff_rds = function (
		dir_base, 
		dir_compare, 
		file_name, 
		key_fields, 
		.diff_sorter = dplyr::arrange_all, 
		.rds_loader = memoise::memoise(binder(base::readRDS, data.table::as.data.table)), 
		...) 
{
	# filepath_base = dir_base |> base::file.path(file_name)
	# filepath_compare = dir_compare |> base::file.path(file_name)
	# 
	# df_base.loading = parallel::mcparallel(.rds_loader(filepath_base))
	# df_compare.loading = parallel::mcparallel(.rds_loader(filepath_compare))
	# 
	# df_base = parallel::mccollect(df_base.loading)[[base::as.character(df_base.loading$pid)]]
	# df_compare = parallel::mccollect(df_compare.loading)[[base::as.character(df_compare.loading$pid)]]
	
	base::list(BASE = dir_base, COMPARE = dir_compare) |> 
		base::lapply(\ (dir) dir |> base::file.path(file_name)) |> 
		base::lapply(\ (filepath) parallel::mcparallel(.rds_loader(filepath))) |> 
		base::lapply(\ (df_loading) parallel::mccollect(df_loading)[[base::as.character(df_loading$pid)]]) |> 
		base::identity() -> pair_df
	
	splidiff_df(
		base = pair_df$BASE,
		compare = pair_df$COMPARE,
		.key_fields = key_fields,
		.work_name = file_name,
		.diff_sorter = .diff_sorter,
		...)
}

splidiff_df = function (
		base, 
		compare, 
		.key_fields, 
		.work_name = NULL, 
		.diff_sorter = dplyr::arrange_all, 
		...) 
{
	pair_df = base::list(BASE = base, COMP = compare)
	
	usethis::ui_info("Splidiff Comparing DF: {usethis::ui_value(.work_name)}")
	
	tictoc::tic("split dataset")
	pair_splidfs = pair_df |> 
		base::lapply(data.table::as.data.table) |> 
		base::lapply(\ (.d) .d |> splidt_at(.key_fields))
	tictoc::toc()
	
	pair_splidfkeys = pair_splidfs |> base::lapply(base::names)
	
	keys.intersect = base::intersect(
		x = pair_splidfkeys$BASE, 
		y = pair_splidfkeys$COMP)
	
	base::names(keys.intersect) = keys.intersect
	
	..pair_lack_keyfields = pair_splidfkeys |> 
		base::lapply(\ (keyfields) keyfields |> base::setdiff(keys.intersect)) |> 
		base::lapply(summary_keyfields)
	
	..pair_lack_keyfields |> liapply(\ (lack_keyfields, partname) if (nrow(lack_keyfields) > 0) 
		usethis::ui_warn("Key Fields of {usethis::ui_value(.work_name)}: Lack in {partname}: \n{outformat(lack_keyfields)}"))
	
	# .keys_lack_base = pair_splidfkeys$BASE |> setdiff(keys.intersect) |> summary_keyfields()
	# .keys_lack_compare = pair_splidfkeys$COMP |> setdiff(keys.intersect) |> summary_keyfields()
	# 
	# if (nrow(.keys_lack_base) > 0) usethis::ui_warn("Key Fields of {usethis::ui_value(.work_name)}: Lack in BASE: \n{outformat(.keys_lack_base)}")
	# if (nrow(.keys_lack_compare) > 0) usethis::ui_warn("Key Fields of {usethis::ui_value(.work_name)}: Lack in COMP: \n{outformat(.keys_lack_compare)}")
	
	usethis::ui_info("Comparable Keys in {usethis::ui_value(.work_name)}: \n{outformat(summary_keyfields(keys.intersect))}")
	
	
	splidf_pairs = keys.intersect |> base::lapply(\ (k) base::list(
		BASE = pair_splidfs$BASE[[k]], 
		COMP = pair_splidfs$COMP[[k]]))
	
	
	#' 这里在使用 *nix 系统时最优方案是用 `parallel::mclapply` （足够快但有可能 OOM 崩溃）
	#' 其它情况要用 future 的 `future::plan(future::multisession)` （或类似的）
	#' 
	#' .future_plan.pieces()
	#' .splidf_reports = splidf_pairs |> 
	#'   furrr::future_map(
	#'     \ (pair_splidf) diffdf::diffdf(
	#'       base = .func_sort(pair_splidf$BASE), 
	#'       compare = .func_sort(pair_splidf$COMP), 
	#'       ...))
	#' 
	#' 如果 `future::plan(future::multicore)` 不会仅仅因为在 R Studio 中使用就被禁止，就不用这么麻烦了。
	#' 它和 `parallel::mclapply` 效果一样，并且在语言层面统一为 future 风格（因而便于抽象）。
	#' 
	
	tictoc::tic("diff splidf")
	..splidf_reports = splidf_pairs |> 
		parallel::mclapply(
			mc.cores = base::getOption("mc.cores", parallel::detectCores() / 2^2), 
			FUN = \ (pair_splidf) diffdf::diffdf(
				base = .diff_sorter(pair_splidf$BASE), 
				compare = .diff_sorter(pair_splidf$COMP), 
				...), 
			X = _)
	tictoc::toc()
	
	#' 关于 Lazy DT
	#' 
	#' 首先，如果求取 `head(x)` 的话，它并不会只进行头 6 次计算，而是计算所有，然后给你头 6 行。
	#' 其次，当再次求取时，计算要重新进行。就是说，进行过一次因而没必要再来一次的工作要再来一次。
	#' 
	#' 这样就完全不如 Scala 的 LazyList 了 ……
	#' 
	#' 验证用代码：
	#' 
	#'   # 更长耗时：
	#'   data.table::data.table(key_fields = base::names(splidf_pairs)) |>
	#'     dtplyr::lazy_dt(immutable = F) |>
	#'     dplyr::mutate(diff = base::Vectorize(diffdf::diffdf)(
	#'       base = pair_splidfs$BASE,
	#'       compare = pair_splidfs$COMP)) -> x
	#'   head(x)
	#'   
	#'   # 更短耗时：
	#'   base::Vectorize(diffdf::diffdf)(head(pair_splidfs$BASE), head(pair_splidfs$COMP)) -> x2
	#' 
	#' 如此一来这里也没有用 `data.table` 的必要了，它并没有魔法。
	#' 
	
	
	..splidf_issues = ..splidf_reports |> base::Filter(x = _, f = diffdf::diffdf_has_issues)
	..issue_keys = summarynames_splidf(..splidf_issues)
	
	usethis::ui_info("Issue keys in file {usethis::ui_value(.work_name)}: \n{outformat(..issue_keys)}")
	
	..issued_pairs = splidf_pairs[base::names(..splidf_issues)]
	
	base::list(
		.issued_pairs = ..issued_pairs, 
		lack_keyfields = ..pair_lack_keyfields, 
		issue_keyfields = ..issue_keys, 
		issues = ..splidf_issues, 
		reports = ..splidf_reports)
}


splidiff_checkvar = function (diff_report, vars, .anyall = base::any, ...) diff_report |> 
	base::lapply(\ (x) x$NumDiff) |> 
	base::Filter(x = _, f = \ (D) .anyall(vars %in% D$Variable)) |> 
	base::identity()

splidiff_varchoose = function (diff_report, vars, .anyall = base::any, ...) diff_report |> 
	splidiff_checkvar(vars, .anyall, ...) |> 
	summarynames_splidf()

\ () 
{
	# show keyfields that have choosed var diff
	splidiff_rep$issues |> splidiff_varchoose('sex')
	splidiff_rep$issues |> splidiff_checkvar('sex')
}
