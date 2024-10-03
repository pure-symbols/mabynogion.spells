
collase_0 = function (.na = NA, .default = NA) function (.xs = base::list(...), ...) .xs |> 
	base::Filter(x = _, f = \ (x) !(x %in% base::unlist(.na))) |> 
	base::append(.default) |> 
	magrittr::'%>%'(.[[1]]) |> 
	base::identity()

collase = function (
		..., 
		.xs = base::list(...), 
		.na = NA, 
		.default = NA, 
		.simplify = T, 
		.use_names = F) .xs |> 
	xrosslist(.silent = T) |> 
	base::lapply(collase_0 (.na, .default)) |> 
	magrittr::'%>%'({if (.simplify) base::unlist(.) else .}) |> 
	magrittr::'%>%'({if (.use_names) . else . |> name_as(NULL)}) |> 
	base::identity()

pcollase = function (
		..., 
		.na = NA, 
		.default = NA) ... |> 
	collase(
		.na = .na, 
		.default = .default, 
		.simplify = T, 
		.use_names = F)

# collase(c(T,NA,NA), c(3,NA,2), c(NA,9,5)) #> [1] 1 9 2
# pcollase(c(T,NA,NA), c(3,NA,2), c(NA,9,5)) #> [1] 1 9 2
# pcollase(c(T,NA,NA), c("a",NA,""), c(NA,9,5), .na = list(NA,"")) #> [1] 1 9 5





preducer = 
	function (fun) 
	function (...) 
(\ (l) # lst of ...
(\ (n) # names get
(\ (n) # names if null be ""
(\ (r) # length for a 'row' (or 'col' for the input)
	
	base::list() |> 
		
		#' formats
		#' 
		looper_reduce(
			.f = \ (a, b) base::c (a, b |> len_as(r) |> base::list()), 
			.iter = l, 
			.init = _) |> 
		name_as(n) |>
		
		base::lapply(base::as.list) |> 
		
		#' pivots
		#' 
		#' also: 
		#' 
		#'   base::Reduce (
		#'     x = _,
		#'     f = \ (a,b) base::seq(r) |> 
		#'       base::lapply (\ (rownum) base::c (a[[rownum]], base::list(b[rownum]))) ) |> 
		#' 
		#' but the using codes are clear.
		#' 
		looper_reduce(
			.f = \ (a, b) base::seq(r) |> 
				base::lapply(
					\ (rownum) a |> 
						len_as(r) |> 
						magrittr::'%>%'(.[[rownum]]) |> 
						list_append(b[[rownum]])), 
			.init = base::list()) |> 
		base::lapply (\ (l) base::'names<-'(x = l, value = n)) |> 
		base::'names<-'(x = _, value = base::seq(r)) |> 
		
		#' applies
		#' 
		base::lapply (\ (params) base::do.call (fun, params) ) |>
		
		base::identity()
	
) (r = base::length(l[[1]]))
) (n = if (base::is.null(n)) base::rep("",base::length(l)) else n)
) (n = base::names(l))
) (l = base::list(...))


#| > preducer (base::min) (a = 1:4, b = 3:5, c = 2:5) |> base::unlist()
#| 1  2  3  4 
#| 1  2  3 NA 

#| > preducer (\ (...) base::list(...)) (a = 1:4, b = c("a","b"), c = 2:5)
#| ... (shows the params)

#| > preducer (base::paste) (a = 1:4, b = c("a","b"), c = 2:5) |> base::unlist()
#|        1        2        3        4 
#|  "1 a 2"  "2 b 3" "3 NA 4" "4 NA 5" 

#| > preducer (base::sum) (list(1:4, c(NA,1), c(NA,2)), na.rm = c(F,F,T)) |> base::unlist()
#|  1  2  3 
#| 10 NA  2 



#' simpler ver'
#' 

tidy_papplier = function (func) function (...) tibble::tibble(...) |> 
	tidy_transpose() |> 
	base::lapply(func) |> 
	tidy_transpose() |> 
	base::identity()

# tidy_papplier (identity) (1:4, 4:1)
# # # A tibble: 4 × 2
# #     `1`   `2`
# #   <int> <int>
# # 1     1     4
# # 2     2     3
# # 3     3     2
# # 4     4     1
# 
# tidy_papplier (min) (1:4, 4:1)
# # # A tibble: 4 × 1
# #     `1`
# #   <int>
# # 1     1
# # 2     2
# # 3     2
# # 4     1
# 
# tidy_papplier (sum) (1:4, 4:1)
# # # A tibble: 4 × 1
# #     `1`
# #   <int>
# # 1     5
# # 2     5
# # 3     5
# # 4     5

preducer_simple = function (func) function (...) tidy_papplier (func) (...) |> 
	base::unlist(recursive = F, use.names = F)

# preducer_simple (base::min) (1:4, 4:1)
# # [1] 1 2 2 1

# base::pmin (1:4, 4:1)
# # [1] 1 2 2 1
