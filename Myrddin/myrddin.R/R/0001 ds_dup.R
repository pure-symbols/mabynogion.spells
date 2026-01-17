
df_dupchk = function (.d, ..., .show_all = F, .tail_dup = T) .d |> 
	dplyr::group_by(...) |> 
	dplyr::mutate(
		.dup_count = dplyr::n(), 
		.dup_rownum = dplyr::row_number(), 
		.dup_flagtail = .tail_dup) |> 
	dplyr::ungroup() |> 
	dplyr::mutate(
		.is_duplicated = dplyr::if_else(
			.dup_flagtail, 
			.dup_rownum > 1, 
			.dup_rownum < .dup_count), 
		.has_duplicated = .dup_count > 1) |> 
	dplyr::select(- .dup_flagtail) |> 
	magrittr::'%>%'({
		if (!.show_all) . else . |> 
			dplyr::filter(.has_duplicated) |> 
			dplyr::select(- .has_duplicated)}) |> 
	base::identity()

df_dupdistinct = function (
		.d, 
		..., 
		.rm_fields = base::c(
			'.has_duplicated',
			'.is_duplicated',
			'.dup_rownum',
			'.dup_count'), 
		.tail_dup = T) .d |> 
	df_dupchk(..., .show_all = T, .tail_dup = .tail_dup) |> 
	dplyr::filter(!.is_duplicated) |> 
	dplyr::select(- tidyselect::any_of(.rm_fields)) |> 
	base::identity()



#| > dplyr::storms |> df_dupchk(name)
#| # A tibble: 19,537 × 17
#|    name   year month   day  hour   lat  long status     category  wind pressure tropicalstorm_force_…¹ hurricane_force_diam…² .dup_count .dup_rownum .is_duplicated
#|    <chr> <dbl> <dbl> <int> <dbl> <dbl> <dbl> <fct>         <dbl> <int>    <int>                  <int>                  <int>      <int>       <int> <lgl>         
#|  1 Amy    1975     6    27     0  27.5 -79   tropical …       NA    25     1013                     NA                     NA         31           1 FALSE         
#|  2 Amy    1975     6    27     6  28.5 -79   tropical …       NA    25     1013                     NA                     NA         31           2 TRUE          
#|  3 Amy    1975     6    27    12  29.5 -79   tropical …       NA    25     1013                     NA                     NA         31           3 TRUE          
#|  4 Amy    1975     6    27    18  30.5 -79   tropical …       NA    25     1013                     NA                     NA         31           4 TRUE          
#|  5 Amy    1975     6    28     0  31.5 -78.8 tropical …       NA    25     1012                     NA                     NA         31           5 TRUE          
#|  6 Amy    1975     6    28     6  32.4 -78.7 tropical …       NA    25     1012                     NA                     NA         31           6 TRUE          
#|  7 Amy    1975     6    28    12  33.3 -78   tropical …       NA    25     1011                     NA                     NA         31           7 TRUE          
#|  8 Amy    1975     6    28    18  34   -77   tropical …       NA    30     1006                     NA                     NA         31           8 TRUE          
#|  9 Amy    1975     6    29     0  34.4 -75.8 tropical …       NA    35     1004                     NA                     NA         31           9 TRUE          
#| 10 Amy    1975     6    29     6  34   -74.8 tropical …       NA    40     1002                     NA                     NA         31          10 TRUE          
#| # ℹ 19,527 more rows
#| # ℹ abbreviated names: ¹​tropicalstorm_force_diameter, ²​hurricane_force_diameter
#| # ℹ 1 more variable: .has_duplicated <lgl>
#| # ℹ Use `print(n = ...)` to see more rows
#| > dplyr::storms |> df_dupchk(name, .tail_dup = F)
#| # A tibble: 19,537 × 17
#|    name   year month   day  hour   lat  long status     category  wind pressure tropicalstorm_force_…¹ hurricane_force_diam…² .dup_count .dup_rownum .is_duplicated
#|    <chr> <dbl> <dbl> <int> <dbl> <dbl> <dbl> <fct>         <dbl> <int>    <int>                  <int>                  <int>      <int>       <int> <lgl>         
#|  1 Amy    1975     6    27     0  27.5 -79   tropical …       NA    25     1013                     NA                     NA         31           1 TRUE          
#|  2 Amy    1975     6    27     6  28.5 -79   tropical …       NA    25     1013                     NA                     NA         31           2 TRUE          
#|  3 Amy    1975     6    27    12  29.5 -79   tropical …       NA    25     1013                     NA                     NA         31           3 TRUE          
#|  4 Amy    1975     6    27    18  30.5 -79   tropical …       NA    25     1013                     NA                     NA         31           4 TRUE          
#|  5 Amy    1975     6    28     0  31.5 -78.8 tropical …       NA    25     1012                     NA                     NA         31           5 TRUE          
#|  6 Amy    1975     6    28     6  32.4 -78.7 tropical …       NA    25     1012                     NA                     NA         31           6 TRUE          
#|  7 Amy    1975     6    28    12  33.3 -78   tropical …       NA    25     1011                     NA                     NA         31           7 TRUE          
#|  8 Amy    1975     6    28    18  34   -77   tropical …       NA    30     1006                     NA                     NA         31           8 TRUE          
#|  9 Amy    1975     6    29     0  34.4 -75.8 tropical …       NA    35     1004                     NA                     NA         31           9 TRUE          
#| 10 Amy    1975     6    29     6  34   -74.8 tropical …       NA    40     1002                     NA                     NA         31          10 TRUE          
#| # ℹ 19,527 more rows
#| # ℹ abbreviated names: ¹​tropicalstorm_force_diameter, ²​hurricane_force_diameter
#| # ℹ 1 more variable: .has_duplicated <lgl>
#| # ℹ Use `print(n = ...)` to see more rows

#| > dplyr::storms |> df_dupdistinct(name)
#| # A tibble: 260 × 13
#|    name      year month   day  hour   lat  long status                 category  wind pressure tropicalstorm_force_diameter hurricane_force_diameter
#|    <chr>    <dbl> <dbl> <int> <dbl> <dbl> <dbl> <fct>                     <dbl> <int>    <int>                        <int>                    <int>
#|  1 Amy       1975     6    27     0  27.5 -79   tropical depression          NA    25     1013                           NA                       NA
#|  2 Blanche   1975     7    24     0  26   -68.4 tropical depression          NA    20     1014                           NA                       NA
#|  3 Caroline  1975     8    24    12  22.4 -69.8 tropical depression          NA    25     1011                           NA                       NA
#|  4 Doris     1975     8    28    12  33.3 -46.3 subtropical storm            NA    35     1005                           NA                       NA
#|  5 Eloise    1975     9    13    12  17.6 -55.2 tropical depression          NA    25     1009                           NA                       NA
#|  6 Faye      1975     9    24    18  23   -56.9 tropical depression          NA    25     1005                           NA                       NA
#|  7 Gladys    1975     9    22    18  10.3 -34.8 tropical depression          NA    25     1012                           NA                       NA
#|  8 Hallie    1975    10    24    18  29.1 -79.3 subtropical depression       NA    30     1006                           NA                       NA
#|  9 Belle     1976     8     6     6  26   -72.8 tropical depression          NA    25     1012                           NA                       NA
#| 10 Dottie    1976     8    18     0  26   -84   tropical depression          NA    20     1010                           NA                       NA
#| # ℹ 250 more rows
#| # ℹ Use `print(n = ...)` to see more rows
#| > dplyr::storms |> df_dupdistinct(name, .tail_dup = F)
#| # A tibble: 260 × 13
#|    name      year month   day  hour   lat  long status              category  wind pressure tropicalstorm_force_diameter hurricane_force_diameter
#|    <chr>    <dbl> <dbl> <int> <dbl> <dbl> <dbl> <fct>                  <dbl> <int>    <int>                        <int>                    <int>
#|  1 Amy       1975     7     4    12  47   -48   extratropical             NA    45      995                           NA                       NA
#|  2 Blanche   1975     7    28    18  47.2 -62.4 extratropical             NA    60      992                           NA                       NA
#|  3 Caroline  1975     9     1    12  25.3 -99   tropical depression       NA    20     1002                           NA                       NA
#|  4 Doris     1975     9     4    12  43.5 -40.7 extratropical             NA    45     1005                           NA                       NA
#|  5 Eloise    1975     9    24    18  37.5 -81.5 extratropical             NA    20     1004                           NA                       NA
#|  6 Faye      1975     9    29     6  42.9 -40   hurricane                  1    65      977                           NA                       NA
#|  7 Gladys    1975    10     4     0  55   -40   extratropical             NA    65      980                           NA                       NA
#|  8 Hallie    1975    10    28     0  36.9 -71.4 extratropical             NA    35     1008                           NA                       NA
#|  9 Belle     1976     8    10    12  42.6 -72.4 tropical storm            NA    35      992                           NA                       NA
#| 10 Dottie    1976     8    21    12  33.5 -80   tropical depression       NA    20     1015                           NA                       NA
#| # ℹ 250 more rows
#| # ℹ Use `print(n = ...)` to see more rows




#| > dplyr::storms |> df_dupchk(name) |> dplyr::arrange(name)
#| # A tibble: 19,537 × 17
#|    name      year month   day  hour   lat  long status  category  wind pressure tropicalstorm_force_…¹ hurricane_force_diam…² .dup_count .dup_rownum .is_duplicated
#|    <chr>    <dbl> <dbl> <int> <dbl> <dbl> <dbl> <fct>      <dbl> <int>    <int>                  <int>                  <int>      <int>       <int> <lgl>         
#|  1 AL011993  1993     5    31    12  21.5 -84   tropic…       NA    25     1003                     NA                     NA         11           1 FALSE         
#|  2 AL011993  1993     5    31    18  22.3 -82   tropic…       NA    25     1002                     NA                     NA         11           2 TRUE          
#|  3 AL011993  1993     6     1     0  23.2 -80.3 tropic…       NA    25     1000                     NA                     NA         11           3 TRUE          
#|  4 AL011993  1993     6     1     6  24.5 -79   tropic…       NA    25     1000                     NA                     NA         11           4 TRUE          
#|  5 AL011993  1993     6     1    12  25.4 -77.5 tropic…       NA    30      999                     NA                     NA         11           5 TRUE          
#|  6 AL011993  1993     6     1    18  26.1 -75.8 tropic…       NA    30      999                     NA                     NA         11           6 TRUE          
#|  7 AL011993  1993     6     2     0  26.7 -74   tropic…       NA    30      999                     NA                     NA         11           7 TRUE          
#|  8 AL011993  1993     6     2     6  27.8 -71.8 tropic…       NA    30      999                     NA                     NA         11           8 TRUE          
#|  9 AL011993  1993     6     2    12  28.4 -69.9 extrat…       NA    35      999                     NA                     NA         11           9 TRUE          
#| 10 AL011993  1993     6     2    18  28.5 -67.8 extrat…       NA    35     1000                     NA                     NA         11          10 TRUE          
#| # ℹ 19,527 more rows
#| # ℹ abbreviated names: ¹​tropicalstorm_force_diameter, ²​hurricane_force_diameter
#| # ℹ 1 more variable: .has_duplicated <lgl>
#| # ℹ Use `print(n = ...)` to see more rows
#| > dplyr::storms |> df_dupchk(name, .tail_dup = F) |> dplyr::arrange(name)
#| # A tibble: 19,537 × 17
#|    name      year month   day  hour   lat  long status  category  wind pressure tropicalstorm_force_…¹ hurricane_force_diam…² .dup_count .dup_rownum .is_duplicated
#|    <chr>    <dbl> <dbl> <int> <dbl> <dbl> <dbl> <fct>      <dbl> <int>    <int>                  <int>                  <int>      <int>       <int> <lgl>         
#|  1 AL011993  1993     5    31    12  21.5 -84   tropic…       NA    25     1003                     NA                     NA         11           1 TRUE          
#|  2 AL011993  1993     5    31    18  22.3 -82   tropic…       NA    25     1002                     NA                     NA         11           2 TRUE          
#|  3 AL011993  1993     6     1     0  23.2 -80.3 tropic…       NA    25     1000                     NA                     NA         11           3 TRUE          
#|  4 AL011993  1993     6     1     6  24.5 -79   tropic…       NA    25     1000                     NA                     NA         11           4 TRUE          
#|  5 AL011993  1993     6     1    12  25.4 -77.5 tropic…       NA    30      999                     NA                     NA         11           5 TRUE          
#|  6 AL011993  1993     6     1    18  26.1 -75.8 tropic…       NA    30      999                     NA                     NA         11           6 TRUE          
#|  7 AL011993  1993     6     2     0  26.7 -74   tropic…       NA    30      999                     NA                     NA         11           7 TRUE          
#|  8 AL011993  1993     6     2     6  27.8 -71.8 tropic…       NA    30      999                     NA                     NA         11           8 TRUE          
#|  9 AL011993  1993     6     2    12  28.4 -69.9 extrat…       NA    35      999                     NA                     NA         11           9 TRUE          
#| 10 AL011993  1993     6     2    18  28.5 -67.8 extrat…       NA    35     1000                     NA                     NA         11          10 TRUE          
#| # ℹ 19,527 more rows
#| # ℹ abbreviated names: ¹​tropicalstorm_force_diameter, ²​hurricane_force_diameter
#| # ℹ 1 more variable: .has_duplicated <lgl>
#| # ℹ Use `print(n = ...)` to see more rows

#| > dplyr::storms |> df_dupdistinct(name) |> dplyr::arrange(name)
#| # A tibble: 260 × 13
#|    name      year month   day  hour   lat  long status              category  wind pressure tropicalstorm_force_diameter hurricane_force_diameter
#|    <chr>    <dbl> <dbl> <int> <dbl> <dbl> <dbl> <fct>                  <dbl> <int>    <int>                        <int>                    <int>
#|  1 AL011993  1993     5    31    12  21.5 -84   tropical depression       NA    25     1003                           NA                       NA
#|  2 AL012000  2000     6     7    18  21   -93   tropical depression       NA    25     1008                           NA                       NA
#|  3 AL021992  1992     6    25    12  24.5 -85.5 tropical depression       NA    25     1009                           NA                       NA
#|  4 AL021994  1994     7    20     6  32.2 -78.9 tropical depression       NA    25     1017                           NA                       NA
#|  5 AL021999  1999     7     2    18  20.2 -95   tropical depression       NA    30     1006                           NA                       NA
#|  6 AL022000  2000     6    23     0   9.5 -19.8 tropical depression       NA    25     1010                           NA                       NA
#|  7 AL022001  2001     7    11    18  10.9 -42.1 tropical depression       NA    25     1011                           NA                       NA
#|  8 AL022003  2003     6    11     0   9.5 -40.8 tropical depression       NA    30     1009                           NA                       NA
#|  9 AL022006  2006     7    16    12  37.2 -68.7 extratropical             NA    30     1009                            0                        0
#| 10 AL031987  1987     8     9    12  26.3 -93.6 tropical depression       NA    30     1010                           NA                       NA
#| # ℹ 250 more rows
#| # ℹ Use `print(n = ...)` to see more rows
#| > dplyr::storms |> df_dupdistinct(name, .tail_dup = F) |> dplyr::arrange(name)
#| # A tibble: 260 × 13
#|    name      year month   day  hour   lat  long status              category  wind pressure tropicalstorm_force_diameter hurricane_force_diameter
#|    <chr>    <dbl> <dbl> <int> <dbl> <dbl> <dbl> <fct>                  <dbl> <int>    <int>                        <int>                    <int>
#|  1 AL011993  1993     6     3     0  28.8 -65.3 extratropical             NA    35     1000                           NA                       NA
#|  2 AL012000  2000     6     8    12  20.8 -93.5 tropical depression       NA    25     1010                           NA                       NA
#|  3 AL021992  1992     6    26    12  28.5 -82.9 tropical depression       NA    30     1007                           NA                       NA
#|  4 AL021994  1994     7    21     6  35.2 -81   tropical depression       NA    15     1016                           NA                       NA
#|  5 AL021999  1999     7     3     6  20.4 -97.3 tropical depression       NA    25     1005                           NA                       NA
#|  6 AL022000  2000     6    25    18   9.7 -38.2 tropical depression       NA    25     1010                           NA                       NA
#|  7 AL022001  2001     7    12    18  13.1 -48.5 tropical depression       NA    25     1012                           NA                       NA
#|  8 AL022003  2003     6    11    18   9.7 -45.9 tropical depression       NA    30     1010                           NA                       NA
#|  9 AL022006  2006     7    19    12  49.8 -46.1 other low                 NA    25     1014                            0                        0
#| 10 AL031987  1987     8    17     6  31.8 -82.3 tropical depression       NA    10     1015                           NA                       NA
#| # ℹ 250 more rows
#| # ℹ Use `print(n = ...)` to see more rows


