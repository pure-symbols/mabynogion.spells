
# https://stackoverflow.com/questions/42704919/what-is-the-tidyverse-method-for-splitting-a-df-by-multiple-columns/

#' @name splitidy
#' 
#' @param .data a tidy supported data
#' @param ... write fields (which is exists in `.data`) here to split by
#' @param .at_fields another style to write fields (which is exists in `.data`) here to split by
#' @return a list of splited dataset pieces with their names clear
#' 
#' @examples
#' 
#' data.table::as.data.table(dplyr::storms)[base::sample(16)] |> splitidy_by(status, year, wind)
#' data.table::as.data.table(dplyr::storms)[base::sample(16)] |> splitidy(status, year, wind)
#' 
#' data.table::as.data.table(dplyr::storms)[base::sample(16)] |> splitidy_at(base::c('status', 'year', 'wind'))
#' data.table::as.data.table(dplyr::storms)[base::sample(16)] |> splitidy(.at_fields = base::c('status', 'year', 'wind'))
#' 

splitidy_by = function (.data, ...) 
{
	.sp = .data |> 
		dplyr::group_split(...)
	
	names(.sp) = .sp |> lapply(
		\ (x) x |> 
			dplyr::select(...) |> 
			base::unique())
	.sp
}

splitidy_by = function (.data, ...) .data |> 
	dplyr::group_split(...) |> 
	magrittr::'%>%'(name_as(
		as_name = . |> base::lapply(
			\ (.sp) .sp |> 
				dplyr::select(...) |> 
				base::unique()), 
		.x = .)) |> 
	base::identity()


# splitidy_in(status, year == 1975, xx = wind == 60): 
# 
# data.table::as.data.table(dplyr::storms) |> 
#   dplyr::mutate(`status` = status, `year == 1975` = year == 1975, `xx` = wind == 60) |> 
#   splitidy_by(status, `year == 1975`, `xx`)


# splidt_in(status, year == 1975, xx = wind == 60): 
# 
# data.table::as.data.table(dplyr::storms) |> 
#   dplyr::mutate('status' = status, 'year == 1975' = year == 1975, 'xx' = wind == 60) |> 
#   splidt_at(by = c('status', 'year == 1975', 'xx'))


splitidy_at = function (.data, fields) 
{
	.sp = .data |> 
		dplyr::group_by(dplyr::pick(tidyselect::all_of(fields))) |> 
		dplyr::group_split()
	
	names(.sp) = .sp |> lapply(
		\ (x) x |> 
			dplyr::select(tidyselect::all_of(fields)) |> 
			base::unique())
	
	.sp
}

splitidy_at = function (.data, fields) .data |> 
	dplyr::group_by(dplyr::pick(tidyselect::all_of(fields))) |> 
	dplyr::group_split() |> 
	magrittr::'%>%'(name_as(
		as_name = . |> base::lapply(
			\ (.sp) .sp |> 
				dplyr::select(tidyselect::all_of(fields)) |> 
				base::unique()), 
		.x = .)) |> 
	base::identity()


splitidy = function (
		.data, 
		..., 
		.at_fields = NULL) 
	if (base::is.null(.at_fields)) 
		.data |> splitidy_by(...) else 
		.data |> splitidy_at(.at_fields)



# https://rdatatable.gitlab.io/data.table/reference/split.html

#' @name splidt
#' 
#' @param .dt a `data.table` instance
#' @param by specify columns here to split by
#' @return a list of splited dataset pieces with their names clear
#' 
#' @examples
#' 
#' data.table::as.data.table(dplyr::storms)[base::sample(16)] -> ST16
#' ST16 |> splidt_at(by = base::c('pressure', 'wind', 'name'))
#' ST16 |> splidt_at(by = base::c('pressure', 'wind', 'name', 'status'))
#' ST16 |> splidt_ex(formulas = base::c(~ pressure, ~ wind, ~ name), drop = T)
#' ST16 |> splidt_in(~ pressure, ~ wind, ~ name, .drop = F)
#' ST16 |> splidt_in(~ pressure, ~ wind, ~ name)
#' ST16 |> splidt_in(~ pressure, ~ wind, ~ name, .keep_order = T)
#' 
#' data.table::as.data.table(dplyr::storms) -> ST
#' ST |> splidt_in(~ year == 2022, ~ wind == 60, ~ pressure == 1000, ~ name)
#' ST |> splidt_in(~ name == 'AL012000', ~ wind == 25)
#' ST |> splidt_in(~ name == 'AL012000', ~ wind == 25, .keep_order = T)
#' ST |> splidt_in(~ name == 'AL012000', ~ wind == 25, .keep_order = T, .drop = F)
#' 

splidt_at = function (.dt, by, ...) .dt |> 
	data.table:::split.data.table(by = by, ...) |> 
	magrittr::'%>%'(name_as(
		as_name = base::lapply(
			FUN = \ (.sp) base::unique(.sp[, base::mget(by)]), 
			X = .), 
		.x = .)) |> 
	base::identity()

splidt_ex = function (
		.dt, 
		formulas, 
		.sep = ' # <==> # ', 
		...) .dt |> 
	data.table:::split.data.table(
		f = evaldt_formulas(
			..formulas = base::c(formulas),
			.dt = .dt), 
		...) |> 
	magrittr::'%>%'(name_as(
		as_name = . |> 
			base::lapply(
				\ (.sp) .sp |> 
					evaldt_formulas(..formulas = base::c(formulas)) |> 
					base::unique()) |> 
			base::as.character() |> 
			base::paste(
				sep = .sep, 
				base::names(.)), 
		.x = .)) |> 
	base::identity()


splidt_lines = function (.dt) .dt |> 
	data.table:::split.data.table(
		base::seq(
			base::nrow(
				.dt)))


splidt_exn = function (
		.dt, 
		formulas, 
		.sep = ' # <--> # ', 
		drop = option: effects ~ suppressed, 
		sorted = option: effects ~ suppressed, 
		..., 
		._fct = evaldt_formulas(
			..formulas = base::c(formulas),
			.dt = .dt)) .dt |> 
	data.table:::split.data.table(
		f = ._fct, 
		drop = T, 
		sorted = T, 
		...) |> 
	magrittr::'%>%'(.[base::order(base::names(.))]) |>
	magrittr::'%>%'(name_as(
		as_name = base::unique(._fct) |> 
			magrittr::'%T>%'((data.table:::setorder)) |>
			splidt_lines() |> 
			base::as.character() |> 
			base::sort() |> 
			base::paste(
				sep = .sep, 
				base::names(.)), 
		.x = .)) |> 
	base::identity()


splidt_in = function (
		.dt, 
		..., 
		.keep_order = F, 
		.drop = T, 
		._fn_splidtex = 
			base::list(
				splidt_exn, 
				splidt_ex)[[1 + .keep_order]]) .dt |> 
	._fn_splidtex(
		formulas = base::c(...), 
		drop = .drop)


# https://www.brodieg.com/2014/04/18/datatable-vs-dplyr-in-split-apply-comgine/ "speed"
# https://stackoverflow.com/questions/12740353/split-a-datatable-into-2-or-more-datatables-based-on-column-value "C#"



summary_keyfields = function (keyfields) keyfields |> 
	purrr::map(~ base::parse(text = .x)) |> 
	base::lapply(base::eval) |> 
	data.table::rbindlist()

summarynames_splidf = function (splidf) summary_keyfields(base::names(splidf))

