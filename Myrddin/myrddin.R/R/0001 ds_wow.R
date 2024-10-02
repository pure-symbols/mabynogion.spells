

#' - https://stackoverflow.com/questions/11134812/how-to-find-the-length-of-a-string-in-r
#' - https://stackoverflow.com/questions/10128617/test-if-characters-are-in-a-string
#' - https://stackoverflow.com/questions/49280212/create-new-columns-based-on-vector-in-dplyr
#' 
#' @examples 
#' 
#' dplyr::storms |> wow('name')
#' dplyr::storms$name |> wow()
#' 
#' dplyr::storms |> wow('name', pattern = 0)
#' dplyr::storms$name |> wow(pattern = 0)
#' 
#' words_in_words("A","AA","AB","B")
#' words_in_words(.words = base::c("A","AA","AB","B"))
#' 

words_in_words = wiw = function (
		..., 
		.words = base::c(...), 
		.f = \ (substr, str) base::grepl(substr, str, fixed = T), 
		.seqi = base::seq(base::length(.words)), 
		.applier = \ (.d, .i) .d |> 
			df_colmutate(
				.val = .f(.words[.i], .words) |> name_as(.words), 
				.i = .i), 
		.simplify = T) .seqi |> 
	base::Reduce(
		f = .applier,
		init = base::list(),
		x = _) |> 
	name_as(.words) |> 
	magrittr::'%>%'({if (.simplify) 
		(base::as.data.frame %bind% base::as.matrix) (.) else 
			base::list(.words = .words) |> base::c(.)}) |> 
	base::identity()


#| > words_in_words("A","AA","AB","B")
#|        A    AA    AB     B
#| A   TRUE FALSE FALSE FALSE
#| AA  TRUE  TRUE FALSE FALSE
#| AB  TRUE FALSE  TRUE  TRUE
#| B  FALSE FALSE FALSE  TRUE
#| > # words_in_words(base::c("A","AA","AB","B"))
#| > # words_in_words(.words = base::c("A","AA","AB","B"))

#| > words_in_words("A","AA","AB","B") |> data.table::as.data.table(keep.rownames = '.word')
#|     .word      A     AA     AB      B
#|    <char> <lgcl> <lgcl> <lgcl> <lgcl>
#| 1:      A   TRUE  FALSE  FALSE  FALSE
#| 2:     AA   TRUE   TRUE  FALSE  FALSE
#| 3:     AB   TRUE  FALSE   TRUE   TRUE
#| 4:      B  FALSE  FALSE  FALSE   TRUE
#| > words_in_words("A","AA","AB","B") |> tibble::as_tibble(rownames = '.word')
#| # A tibble: 4 × 5
#|   .word A     AA    AB    B    
#|   <chr> <lgl> <lgl> <lgl> <lgl>
#| 1 A     TRUE  FALSE FALSE FALSE
#| 2 AA    TRUE  TRUE  FALSE FALSE
#| 3 AB    TRUE  FALSE TRUE  TRUE 
#| 4 B     FALSE FALSE FALSE TRUE 

#| > words_in_words("A","AA","AB","B", .simplify = F)
#| $.words
#| [1] "A"  "AA" "AB" "B" 
#| 
#| $A
#|     A    AA    AB     B 
#|  TRUE  TRUE  TRUE FALSE 
#| 
#| $AA
#|     A    AA    AB     B 
#| FALSE  TRUE FALSE FALSE 
#| 
#| $AB
#|     A    AA    AB     B 
#| FALSE FALSE  TRUE FALSE 
#| 
#| $B
#|     A    AA    AB     B 
#| FALSE FALSE  TRUE  TRUE 
#| 


words_on_words = wow = function (
		words, 
		word_col = NULL, 
		.f = \ (substr, str) base::grepl(substr, str, fixed = T), 
		.words = if (base::is.null(word_col)) words else words[[word_col]], 
		.seqi = base::seq(base::length(.words)), 
		.words_name = 'words' |> 
			base::c(word_col) |> 
			base::paste(collapse = '.'), 
		.val_names = 'VAL_' |> 
			base::paste0(.words) |> 
			name_as(.words), 
		.reducer = \ (.d, .w) .d |> 
			df_colmutate(
				.val = .f(.w, .d[[.words_name]]) |> name_as(.words_name), 
				.i = .val_names[.w]), 
		.dt = data.table::data.table(words = words), 
		pattern = 1) pattern |> 
	magrittr::'%>%'({if (!base::as.logical(.)) {
		
		base::Reduce(
			f = .reducer,
			init = .dt,
			x = .words)
		
	} else {
		
		dplyr::mutate(
			words_in_words = utils::tail(
				n = -1L,
				x = wiw(
					.f = .f,
					.seqi = .seqi,
					.simplify = F,
					.words = base::get(.words_name))),
			wiw = words_in_words |> base::lapply(\ (bools) base::get(.words_name)[bools]),
			wiw_count = wiw |> base::lapply(base::length) |> base::unlist(),
			.data = .dt)
		
	}})
	

#| > c("A","AA","AA","AB","B") |> words_on_words()
#|     words                words_in_words        wiw wiw_count
#|    <char>                        <list>     <list>     <int>
#| 1:      A  TRUE, TRUE, TRUE, TRUE,FALSE A,AA,AA,AB         4
#| 2:     AA FALSE, TRUE, TRUE,FALSE,FALSE      AA,AA         2
#| 3:     AA FALSE, TRUE, TRUE,FALSE,FALSE      AA,AA         2
#| 4:     AB FALSE,FALSE,FALSE, TRUE,FALSE         AB         1
#| 5:      B FALSE,FALSE,FALSE, TRUE, TRUE       AB,B         2

#| > dplyr::storms |> utils::head(32) |> utils::tail(- 29) |> wow('name')
#|    words.name words.year words.month words.day words.hour
#|        <char>      <num>       <num>     <int>      <num>
#| 1:        Amy       1975           7         4          6
#| 2:        Amy       1975           7         4         12
#| 3:    Blanche       1975           7        24          0
#|    words.lat words.long        words.status words.category
#|        <num>      <num>              <fctr>          <num>
#| 1:      44.5      -51.6      tropical storm             NA
#| 2:      47.0      -48.0       extratropical             NA
#| 3:      26.0      -68.4 tropical depression             NA
#|    words.wind words.pressure words.tropicalstorm_force_diameter
#|         <int>          <int>                              <int>
#| 1:         50            986                                 NA
#| 2:         45            995                                 NA
#| 3:         20           1014                                 NA
#|    words.hurricane_force_diameter    words_in_words     wiw
#|                             <int>            <list>  <list>
#| 1:                             NA  TRUE, TRUE,FALSE Amy,Amy
#| 2:                             NA  TRUE, TRUE,FALSE Amy,Amy
#| 3:                             NA FALSE,FALSE, TRUE Blanche
#|    wiw_count
#|        <int>
#| 1:         2
#| 2:         2
#| 3:         1
#| > dplyr::storms[['name']] |> utils::head(32) |> utils::tail(- 29) |> wow()
#|      words    words_in_words     wiw wiw_count
#|     <char>            <list>  <list>     <int>
#| 1:     Amy  TRUE, TRUE,FALSE Amy,Amy         2
#| 2:     Amy  TRUE, TRUE,FALSE Amy,Amy         2
#| 3: Blanche FALSE,FALSE, TRUE Blanche         1

# Simple Direct def: 

# wow = function (
#     words, 
#     word_col = NULL, 
#     .f = \ (substr, str) base::grepl(substr, str, fixed = T)) {
#   
#   DT = data.table::data.table(words = words)
#   words = if (base::is.null(word_col)) words else words[[word_col]]
#   
#   col_name = 'words' |> 
#     base::c(word_col) |> 
#     base::paste(collapse = '.')
#   
#   val_names = 'VAL_' |> 
#     base::paste0(words) |> 
#     magrittr::'%>%'(base::'names<-'(words)) |> 
#     base::as.list()
#   
#   base::Reduce(
#     f = \ (dt, w) `[[<-`(
#       dt, 
#       val_names[[w]], 
#       value = .f(w, dt[[col_name]])),
#     init = DT,
#     x = words)
# }

# wow = function (
#     words, 
#     word_col = NULL, 
#     .f = \ (substr, str) base::grepl(substr, str, fixed = T)) {
#   
#   DT = data.table::data.table(words = words)
#   words = if (base::is.null(word_col)) words else words[[word_col]]
#   
#   col_name = 'words' |> 
#     base::c(word_col) |> 
#     base::paste(collapse = '.')
#   
#   val_names = 'VAL_' |> 
#     base::paste0(words) |> 
#     magrittr::'%>%'(base::'names<-'(words)) |> 
#     base::as.list()
#   
#   ls = seq(length(words)) |> 
#     base::Reduce(
#       f = \ (acc, i) `[[<-`(
#         acc, 
#         i, 
#         value = .f(words[i], words)),
#       init = list(),
#       x = _)
#   names(ls) = val_names
#   
#   DT[T, ws_in := ls]
#   DT[T, ws_in_count := ls |> lapply(sum) |> unlist()]
#   
#   DT
# }
# 
# report$words.name[report[words.name == 'Amy']$ws_in |> unlist()]


