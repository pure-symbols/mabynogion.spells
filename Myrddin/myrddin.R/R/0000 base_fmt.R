



# https://www.gastonsanchez.com/r4strings/formatting.html


# print_as = function (.x, class = base::class(.x)) base::print(
# 	base::'class<-'(x = .x, value = class))

print_as = function (.x, class = base::class(.x)) .x |> 
	class_as(class) |> 
	base::print() |> 
	base::identity()


outformat = function (.x, as_class = base::class(.x)) .x |> 
	print_as(.x = _, class = as_class) |> 
	utils::capture.output() |> 
	base::paste(collapse = "\n")

#' base::identical(
#'   x = base::seq(4) |> print_as('Dlist') |> utils::capture.output(), 
#'   y = base::seq(4) |> outformat('Dlist') |> base::cat() |> utils::capture.output())
#' #| [1] TRUE
#' 
#' 在 `utils::capture.output` 后 `base::cat(sep = '\n')` 就和 `utils::capture.output` 之前一样了 ……
#' 


useout_lines = function (
		.liner = usethis::ui_line, 
		head, tail, 
		.valuer = usethis::ui_value, 
		.template = "{head |> base::append(tail |> base::lapply(.valuer))}"
		) .liner(.template)

