
#' @examples
#' 
#' 1:20 %>% (3 %num.capser% 16)
#' #| [1]  3  3  3  4  5  6  7  8  9 10 11 12 13 14 15 16 16 16 16 16
#' 
#' 
capsnum = `%num.capser%` = 
function (from, to) 
function (nums) base::ifelse(
	nums < from, 
	from, base::ifelse(
		nums > to, 
		to, nums))
