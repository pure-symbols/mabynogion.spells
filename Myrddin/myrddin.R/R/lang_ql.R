
paster_sqlin = function (terms) terms |> 
	str_transer ("\\", "\\\\") (strs = _) |> 
	str_transer ("'", "\\\'") (strs = _) |> 
	# purrr::map_chr(\ (x) if (base::is.na(x)) "NULL" else base::paste0("'",x,"'")) |> 
	magrittr::'%>%'({base::ifelse(base::is.na(.), "NULL", base::paste0("'",.,"'"))}) |> 
	base::paste(collapse = ", ") |> 
	base::paste0('in (',termz = _,')') |> 
	base::identity()

#| > egnames = base::c("Tom", "Jerry", NA, "Jonny", NA, "\\Harr'ier")
#| > glue::glue("select * from tbl where name {paster_sqlin(egnames)}")
#| select * from tbl where name in ('Tom', 'Jerry', 'NA', 'Jonny', 'NA', '\\Harr\'ier')


r2ql_typemap = base::c(
	
	CHAR = "character" ,
	VARCHAR = "character" ,
	STRING = "character" ,
	
	TINYINT = "integer" ,
	SMALLINT = "integer" ,
	INT = "integer" ,
	INTEGER = "integer" ,
	BIGINT = "numeric" ,
	
	DECIMAL = "numeric" ,
	NUMERIC = "numeric" ,
	FLOAT = "numeric" ,
	DOUBLE = "numeric" ,
	
	DATE = "Date" ,
	TIMESTAMP = "POSIXct" ,
	DATETIME = "POSIXct" ,
	
	ARRAY = "array" ,
	STRUCT = "list" ,
	
	BOOLEAN = "logical")

r2ql_typetranser = function 
(mode = base::c('r2sql','sql2r')[1]) function 
(
	.modefn_choosed = .modefns[[base::names(.modefns) |> magrittr::'%>%'(.[. == mode])]], 
	.modefn_unchoosed = .modefns[[base::names(.modefns) |> magrittr::'%>%'(.[. != mode])]], 
	.modefns = base::list(
		r2sql = base::'(', 
		sql2r = base::names), 
	.typemap = r2ql_typemap) function 
(cls) .typemap |> 
	magrittr::'%>%'({.[base::toupper(.modefn_choosed(.)) == base::toupper(cls)]}) |> 
	.modefn_unchoosed() |> 
	magrittr::'%>%'({.[base::length(.)]})

r2ql_typestranser = function (
		classes, 
		mode = base::c('r2sql','sql2r')[1], 
		...) classes |> 
	chr_vapply(
		.f = r2ql_typetranser (mode) (...), 
		.names_switch = T)


#| > base::c("logical","character") |> r2ql_typestranser('r2sql')
#|   logical character 
#| "BOOLEAN"  "STRING" 
#| > base::c("BOOLEAN","STRING") |> r2ql_typestranser('sql2r')
#|     BOOLEAN      STRING 
#|   "logical" "character" 


