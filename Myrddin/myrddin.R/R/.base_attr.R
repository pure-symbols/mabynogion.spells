
#' Attr func
#' 
attr_as = function (.x, attr = NULL, as = NULL, .attrs_init = base::attributes(.x)) .attrs_init |> 
	magrittr::'%>%'({if (base::is.null(attr) || base::is.null(as)) . else list_upsert(attr, as)}) |> 
	magrittr::'%>%'(base::'attributes<-'(.x, .)) |> 
	base::identity()

attr_assign = function (
		.x, 
		..., 
		.attrs_init = base::attributes(.x), 
		.assigns = base::list(...)) .attrs_init |> 
	list_update(.news = .assigns) |> 
	# magrittr::'%>%'(base::'attributes<-'(.x, .)) |> 
	attr_as(.attrs_init = _, .x = .x) |> 
	base::identity()
