#' ref: 
#' - https://stackoverflow.com/questions/34598139/left-join-using-data-table/34600831#34600831
#' - https://rdatatable.gitlab.io/data.table/library/data.table/doc/datatable-joins.html
#' - https://rdatatable.gitlab.io/data.table/reference/merge.html
#' 
left_join.data.table = function (
		.dt, 
		.dt_i, 
		by = base::'names<-'(
			data.table::key(.dt_i),
			data.table::key(.dt)), 
		.i_select = base::names(.dt_i) |> 
			base::setdiff(by)) 
{
	data.table::setDT(.dt)[, (.i_select) := NULL, allow.cartesian = TRUE]
	data.table::setDT(.dt)[.dt_i, on = by, (.i_select) := base::mget(base::paste0('i.', .i_select))]
	data.table::setDT(.dt)
}



