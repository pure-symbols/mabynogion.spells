
#' @examples
#' 
#' base::c(-1,74999:225001) |> range_cat(c(0, 75000, 125000, 175000, 225000))
#' 
range_cat = function (
		.seq, 
		.cats, 
		.fn_as = base::identity, 
		..cats = base::c(-Inf, base::sort(.cats), Inf) |> .fn_as(), 
		..cats_chr = clear_chr(..cats), 
		..catts = base::paste(
			..cats_chr, 
			..cats_chr[
				base::seq(base::length(..cats_chr)) |> 
					utils::tail(-1)] |> base::c(Inf), 
			sep = ' >-> ')) .seq |> 
	tibble::tibble(src_term = _) |> 
	dplyr::mutate(
		cat_index = src_term |> 
			base::findInterval(..cats)) |> 
	dplyr::mutate(
		cat_range = ..catts[cat_index], 
		cat_until = ..cats[cat_index]) |> 
	base::identity()

#| > base::c(-1,-2,74999:225001) |> range_cat(c(0, 75000, 125000, 175000, 225000)) |> data.table::setDT() |> print()
#|         src_term cat_index        cat_range cat_until
#|            <num>     <int>           <char>     <num>
#|      1:       -1         1        -Inf -> 0      -Inf
#|      2:       -2         1        -Inf -> 0      -Inf
#|      3:    74999         2       0 -> 75000         0
#|      4:    75000         3  75000 -> 125000     75000
#|      5:    75001         3  75000 -> 125000     75000
#|     ---                                              
#| 150001:   224997         5 175000 -> 225000    175000
#| 150002:   224998         5 175000 -> 225000    175000
#| 150003:   224999         5 175000 -> 225000    175000
#| 150004:   225000         6    225000 -> Inf    225000
#| 150005:   225001         6    225000 -> Inf    225000


# base::c(-Inf, base::sort(.map), Inf) |> type_as_fn() -> .map
# .x |> type_as_fn() |> base::findInterval(.map) |> magrittr::'%>%'({base::names(.map)[.]})
range_map = function (
		.x, 
		.map, 
		type_as_fn = base::identity, 
		.map_to = NULL, 
		.infinity_to = base::c(
			'.infinity_less', 
			'.infinity_more'), 
		..., 
		..map = .map |> 
			magrittr::'%>%'({base::sort(type_as_fn(
				. |> namelacked_as(.map_to) |> namelacked_self()))}) |> 
			middle_insert(base::c(- Inf, + Inf) |> name_as(.infinity_to)), 
		..map_to = .map_to |> 
			magrittr::'%>%'({if 
				(base::is.null(.)) base::names(..map) else if 
				(T) . |> middle_insert(.infinity_to)}), 
		.show_src = F) .x |> 
	base::findInterval(..map, ...) |> 
	magrittr::'%>%'({..map_to[.]}) |> 
	magrittr::'%>%'({if (.show_src) . |> name_as(clear_chr(.x)) else .}) |> 
	base::identity()
	
#| > X = zoo::as.Date(base::c('2020-02-02','2023-03-03','2025-02-01','2027-07-07'))
#| > Y = zoo::as.Date(base::c(A = '2021-01-31', B = '2025-09-09', C = '2029-03-10'))
#| > X
#| [1] "2020-02-02" "2023-03-03" "2025-02-01" "2027-07-07"
#| > Y
#|            A            B            C 
#| "2021-01-31" "2025-09-09" "2029-03-10" 
#| > X |> range_map(Y, zoo::as.Date, .show_src = T)
#|       2020-02-02       2023-03-03       2025-02-01       2027-07-07 
#| ".infinity_less"              "A"              "A"              "B" 
#| > Z = base::c(1,2,3)
#| > V = base::c(0)
#| > X |> range_map(Y, zoo::as.Date, Z, V, .show_src = T)
#| 2020-02-02 2023-03-03 2025-02-01 2027-07-07 
#|          0          1          1          2 


