
.xrosslist_demo = \ () 
{
	a = base::list(
		x = dplyr::starwars[1, base::c('homeworld', 'eye_color')],
		y = dplyr::starwars[2, base::c('hair_color', 'mass')],
		z = dplyr::starwars[3, base::c('species', 'birth_year')])
	
	b = base::list(
		x = dplyr::starwars[4, base::c('homeworld', 'eye_color')],
		y = dplyr::starwars[5, base::c('hair_color', 'mass')],
		z = dplyr::starwars[6, base::c('species', 'birth_year')])
	
	xrosslist(base::list(a = a, b = b))
	
	xrosslist(base::list(a, b)) |> base::lapply(\ (x) base::Reduce(x = x, f = base::rbind))
	xrosslist(base::list(a, b)) |> base::lapply(\ (x) base::Reduce(x = x, f = base::rbind)) |> lapply(data.table::as.data.table)
	xrosslist(base::list(a, b)) |> base::lapply(data.table::rbindlist)
	xreducelist(a, b, .f = base::rbind)
	
	#| ℹ Will use names: x, y, z
	#| $x
	#| # A tibble: 2 × 2
	#|   homeworld eye_color
	#|   <chr>     <chr>    
	#| 1 Tatooine  blue     
	#| 2 Tatooine  yellow   
	#| 
	#| $y
	#| # A tibble: 2 × 2
	#|   hair_color  mass
	#|   <chr>      <dbl>
	#| 1 NA            75
	#| 2 brown         49
	#| 
	#| $z
	#| # A tibble: 2 × 2
	#|   species birth_year
	#|   <chr>        <dbl>
	#| 1 Droid           33
	#| 2 Human           52
	#| 
	
	xrbindtbls(a,b)
	xrbindtbls(.tables_list = list(a,b))
	
	#| ℹ Will using names: x, y, z
	#| $x
	#|    homeworld eye_color
	#|       <char>    <char>
	#| 1:  Tatooine      blue
	#| 2:  Tatooine    yellow
	#| 
	#| $y
	#|    hair_color  mass
	#|        <char> <num>
	#| 1:       <NA>    75
	#| 2:      brown    49
	#| 
	#| $z
	#|    species birth_year
	#|     <char>      <num>
	#| 1:   Droid         33
	#| 2:   Human         52
	#| 
	
	rbind_tablelist(a,b)
	rbind_tablelist(.tables_list = list(a,b))
}

xreducelist = function (..., .f = base::rbind) base::list(...) |> 
	xrosslist() |> 
	base::lapply(\ (x) base::Reduce(x = x, f = .f)) |> 
	base::identity()

xrbindtbls = function (..., .tables_list = NULL) (if (base::is.null(.tables_list)) 
	base::list(...) else .tables_list) |> 
	xrosslist() |> 
	base::lapply(data.table::rbindlist) |> 
	base::identity()


rbind_tablelist = function (..., .tables_list = NULL) 
{
	tables_list = if (is.null(.tables_list)) list(...) else .tables_list
	
	namex = tables_list |> base::lapply(base::names) |> base::unique()
	
	if (base::length(namex) == 1) "" else 
	{
		.a = "Names of elements Mismatched !!"
		.x = namex |> base::lapply(usethis::ui_field)
		usethis::ui_oops("{.a |> base::append(.x)}")
	}
	
	namex = Reduce(f = base::intersect, x = namex)
	base::names(namex) = namex
	
	usethis::ui_info("Will using names: {usethis::ui_field(namex)}")
	
	namex |> 
		base::lapply(\ (name) tables_list |> base::lapply(\ (tbls) tbls[[name]])) |> 
		base::lapply(data.table::rbindlist) |> 
		base::identity()
}
