

grave_chr = '`'
symbol_gravewarp = function (s) base::as.symbol(base::paste0(grave_chr, s, grave_chr))
symbol_isgraved = function (s) base::toString(s) |> 
	split_str() |> 
	base::lapply(
		\ (.x) base::c(grave_chr, grave_chr) |> 
			base::identical(
				base::c(
					utils::head(.x, 1), 
					utils::tail(.x, 1)))) |> 
	base::unlist() |> 
	base::identity()

