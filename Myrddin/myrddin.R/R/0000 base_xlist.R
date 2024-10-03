
#' {{a1, b1, c1},{a2, b2, c2}} -- ↓
#' ↑ -- {{a1,a2}, {b1,b2}, {c1,c2}}
#' 
#' Combine Records
#' 
xrosslist = function (
		.xs = base::list(), 
		..., 
		..xs = .xs |> base::append(base::list(...)), 
		.names = ..xs |> 
			base::lapply(name_i) |> 
			base::unique() |> 
			magrittr::'%T>%'({if (base::length(.) == 1) "" else 
				usethis::ui_oops |> useout_lines(
					head = "Names of elements Mismatched !!", 
					tail = .,
					.valuer = usethis::ui_field)}) |> 
			base::Reduce(x = _, f = base::intersect), 
		.zone_names = base::names(..xs) |> 
			magrittr::'%>%'({if (base::is.null(.)) 
				base::seq(base::length(..xs)) else .}), 
		.hint_template_names = "Will using names: {usethis::ui_field(.)}",
		.hint_template_serials = "Didn't find any name in terms ... Will using serial numbers as the name.", 
		.silent = F) name_asself(.names) |> 
	magrittr::'%T>%'({if (.silent) {} else 
		usethis::ui_info(
			if (base::is.integer(.)) 
				.hint_template_serials else 
					.hint_template_names)}) |> 
	base::lapply(
		
		#' like: 
		#' 
		#' \ (name) name_asself(.zone_names) |> 
		#'   base::lapply(\ (zone_name) ..xs[[zone_name]][[name]])
		#' 
		\ (name) ..xs |> base::lapply(\ (.x) .x[[name]])) |> 
	base::identity()


#| > list(list(a = 5,6), list(a = 7,c = 8)) |> xrosslist(a = list(a = 1,2), b = list(c = 3, a = 4))
#| ✖ Names of elements Mismatched !!
#| • ✖ a, 
#| • ✖ a, c
#| • ✖ c, a
#| ℹ Will using names: a
#| $a
#| $a[[1]]
#| [1] 5
#| 
#| $a[[2]]
#| [1] 7
#| 
#| $a$a
#| [1] 1
#| 
#| $a$b
#| [1] 4
#| 
#| 
#| > list(list(5,6), list(7,8)) |> xrosslist(a = list(1,2), b = list(3,4))
#| ℹ Didn't find any name in terms ... Will using serial numbers as the name.
#| $`1`
#| $`1`[[1]]
#| [1] 5
#| 
#| $`1`[[2]]
#| [1] 7
#| 
#| $`1`$a
#| [1] 1
#| 
#| $`1`$b
#| [1] 3
#| 
#| 
#| $`2`
#| $`2`[[1]]
#| [1] 6
#| 
#| $`2`[[2]]
#| [1] 8
#| 
#| $`2`$a
#| [1] 2
#| 
#| $`2`$b
#| [1] 4
#| 
#| 



tidy_transpose = function (
		df, 
		.rownames_maker = 
			\ (.data) tibble::rownames_to_column(
				var = ".rowname",
				.data = .data),
		.keep_colname = F) tibble::as_tibble(df) |> 
	.rownames_maker() |> 
	tidyr::pivot_longer(
		cols = - .rowname,
		names_to = ".colname",
		values_to = ".value") |> 
	tidyr::pivot_wider(
		names_from = .rowname,
		values_from = .value) |> 
	dplyr::select(- tidyselect::all_of(
		if (.keep_colname) NULL else '.colname')) |> 
	base::identity()

# tibble::tibble(1:4,2:5) |> tidy_transpose()
# # # A tibble: 2 × 4
# #     `1`   `2`   `3`   `4`
# #   <int> <int> <int> <int>
# # 1     1     2     3     4
# # 2     2     3     4     5

# tibble::tibble (1:4,4:1) |> 
#   tidy_transpose(.keep_colname = T) |> 
#   tidy_transpose(\ (.d) dplyr::rename(.d, .rowname = .colname))
# # # A tibble: 4 × 2
# #   `1:4` `4:1`
# #   <int> <int>
# # 1     1     4
# # 2     2     3
# # 3     3     2
# # 4     4     1


