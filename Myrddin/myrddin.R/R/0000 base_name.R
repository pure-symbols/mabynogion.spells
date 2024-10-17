
# name_as = function (.x, as_name) base::'names<-'(x = .x, value = as_name)
name_as = function (.x, as_name) .x |> attr_assign(names = as_name)
name_asself = function (.x) .x |> name_as(as_name = .x)
name_drop = function (.x) .x |> name_as(as_name = NULL)

name_i = function (.x) base::names(.x) |> 
	magrittr::'%>%'({if (base::is.null(.)) 
		base::seq(base::length(.x)) else .}) |> 
	base::identity()

name_asi = function (.x) .x |> 
	name_as(name_i(.x)) |> 
	base::identity()

name_will = function (as_name) name_i(.x = as_name) |> 
	name_as(as_name = as_name) |> 
	base::names()

name_reverse = function (x) name_i(x) |> name_as(x)



namelacked_as = function (.x, as_name) base::names(.x) |> 
	magrittr::'%>%'({if (base::is.null(.)) base::rep(NA, base::length(.x)) else .}) |> 
	magrittr::'%>%'(base::replace(. == "", NA)) |> 
	pcollase(base::unname(
		force = T,
		obj = as_name)) |> 
	name_as(.x, as_name = _) |> 
	base::identity()

namelacked_self = function (.x) namelacked_as(.x, .x)


