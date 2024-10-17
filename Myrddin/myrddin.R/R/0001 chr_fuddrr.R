



#' @examples
#' 
#' `"ABC123" %>% cut_numletter`
#' #| [[1]]
#' #| [1] "ABC" "123"
#' 
#' `"ABC_a2_a3" %>% cut_numletter`
#' #| [[1]]
#' #| [1] "ABC_a" "2_a"   "3"
#' 
#' `"A1BC_a2_a3" %>% cut_numletter`
#' #| [[1]]
#' #| [1] "A"    "1"    "BC_a" "2_a"  "3"
#' 
#' `"ABC_123" %>% cut_numletter`
#' #| [[1]]
#' #| [1] "ABC_123"
#' 
#' `"ABC_123" %>% ("A-Za-z_" %str.cutter% "0-9")`
#' `"ABC_123" %>% ("0-9" %str.cutter% "A-Za-z_")`
#' 
#' #| [[1]]
#' #| [1] "ABC_" "123"
#' 
#' `"1-1-ABC_12.3" %>% ("0-9\\." %str.cutter% "A-Za-z_\\-")`
#' #| [[1]]
#' #| [1] "1"     "-"     "1"     "-ABC_" "12.3"
#' 
#' `"1-1-ABC_12.3" %>% ("0-9\\.\\-" %str.cutter% "A-Za-z_\\-")`
#' #| [[1]]
#' #| [1] "1"    "-"    "1"    "-"    "ABC_" "12.3"
#' 
`%str.cutter%` = 
#' ref by: 
#' https://stackoverflow.com/a/47670127
#' 
function (pattern_a, pattern_b) 
function (str) str |> 
	strsplit(
		base::paste0(
			"(?=[",pattern_a,
			"])(?<=[",pattern_b,
			"])|(?=[",pattern_b,
			"])(?<=[",pattern_a,
			"])"), 
		perl = TRUE)

cut_numletter = "A-Za-z" %str.cutter% "0-9"


str_surpluschar = function (str) base::seq(base::nchar(str) + 1) |> 
	base::sample(1) |> 
	base::intToUtf8() |> 
	magrittr::'%>%'({if (!(
		. %in% base::unlist(base::strsplit(str,"")))) . else 
			str_surpluschar(str)})

str_transer = `%str.tr%` = 
function (old, new) 
function (strs, .supchr = str_surpluschar(old)) strs |> 
	base::paste0(.supchr) |> 
	base::strsplit(if (old |> identical("\\")) "\\\\" else old) |> 
	base::lapply (\ (s) s |> paste(collapse = new)) |> 
	base::unlist() |> 
	magrittr::'%>%'(base::substr(0, base::nchar(.) - 1)) |> 
	name_as(strs)

#| > '%>%' = magrittr::'%>%'
#| > base::c("aaa bbb CCC ddd bb CC", "bb CC eee 1bb CCC PPP") %>% ("bb CC" %str.tr% "tt TT")
#|   aaa bbb CCC ddd bb CC   bb CC eee 1bb CCC PPP 
#| "aaa btt TTC ddd tt TT" "tt TT eee 1tt TTC PPP" 


strtr_crlf2lf = function (text) ('\r\n' %str.tr% '\n') (text)



# ↑↑↑↑↑↑↓↓↓↑↑↑↓↓↑↑↑↑↑↓↑↑↓↓↓↓↓↓↓↑↑↑↑↑↓

# """
# Hi I'm Fine "Jimmy"
# Thanks!
# """


