
chr_vapply = function (
		.x, 
		.f, 
		..., 
		.names_switch = F) .x |> 
	base::vapply(
		FUN.VALUE = 'character',
		...,
		USE.NAMES = .names_switch,
		FUN = .f,
		X = .x)

#| > base::unique(dplyr::storms$name) |> utils::head()
#| [1] "Amy"      "Blanche"  "Caroline" "Doris"    "Eloise"   "Faye"    
#| > base::unique(dplyr::storms$name) |> chr_vapply(\ (x) x |> base::paste(0), .names_switch = T) |> utils::head()
#|          Amy      Blanche     Caroline        Doris       Eloise         Faye 
#|      "Amy 0"  "Blanche 0" "Caroline 0"    "Doris 0"   "Eloise 0"     "Faye 0" 
#| > base::unique(dplyr::storms$name) |> chr_vapply(\ (x) x |> base::paste(0)) |> utils::head()
#| [1] "Amy 0"      "Blanche 0"  "Caroline 0" "Doris 0"    "Eloise 0"   "Faye 0"    


split_str = function (strs, .spliter = '') strs |> base::split(.spliter)
