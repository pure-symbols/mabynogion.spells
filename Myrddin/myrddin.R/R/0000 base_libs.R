
libs_loading = function (
		expr = {}, 
		libnames = 'base', 
		.expr = base::as.expression(
			base::substitute(
				expr))) base::paste0('library(',libnames,')') |> 
	base::parse(text = _) |> 
	base::c(.expr) |> 
	base::identity()

# {
#   library(callr)
#   library(lobstr)
# } |> 
#   libs_loading('data.table') |> 
#   bse::eval()
