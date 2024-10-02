
#' @examples
#' ns_info('purrr', 'dplyr')
#' ns_baseinfo('purrr', 'dplyr')
#' ns_info('maybe', 'memoise')
#' ns_baseinfo('maybe', 'freqtables, 'memoise', 'wrapr')
#' 
#' ns_info('diffdf', 'freqtables', 'emayili', 'tidyverse') |> purrr::map(\ (info) info$exports) |> purrr::map(base::ls)
#' 

ns_take = function (ns_name) base::asNamespace(ns_name)
ns_name = function (ns) base::getNamespaceName(ns = ns)

ns_infoterms = function(
		ns, 
		...) ns |> 
	
	#' ref: `?base::asNamespace`
	#' 
	base::asNamespace(
		base.OK = F,
		ns = _) |> 
	base::get(
		x = '.__NAMESPACE__.',
		inherits = F,
		envir = _) |> 
	base::ls(
		...,
		envir = _) |> 
	base::identity()

ns_infoget = function (ns, which) base::getNamespaceInfo(
	which = which,
	ns = ns)

ns_info = function (
		..., 
		.ns = base::c(...), 
		.ns_names = .ns |> 
			purrr::imap(
				\ (ns, i) if 
				(base::is.character(ns)) ns else if 
				(ns |> base::inherits('environment')) ns_name(ns) else if 
				(base::is.integer(i)) "" else i) |> 
			base::unlist()) .ns |> 
	base::lapply(ns_take) |> 
	name_as(.ns_names) |> 
	base::lapply(
		\ (ns) name_asself(ns_infoterms(ns)) |> 
			base::lapply(\ (which) ns |> ns_infoget(which)) |> 
			base::identity()) |> 
	base::identity()
	
ns_baseinfo = function (
		..., 
		.ns = base::c(...), 
		.fns = base::list(
			Namespace = base::getNamespace, 
			Name = base::getNamespaceName, 
			Exports = base::getNamespaceExports, 
			Imports = base::getNamespaceImports, 
			Users = base::getNamespaceUsers, 
			Version = base::getNamespaceVersion)) name_asself(.ns) |> 
	base::lapply(
		\ (.ns) .fns |> 
			base::lapply(\ (f) f(.ns)) |> 
			base::identity()) |> 
	base::identity()
