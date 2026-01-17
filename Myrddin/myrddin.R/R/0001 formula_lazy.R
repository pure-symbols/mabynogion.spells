
condz_lazy = function (
		conds, 
		yes_formulas, 
		no_formulas, 
		.names = base::names(conds), 
		.seq = base::seq(base::length(conds)), 
		.extractor = ((\ (x) x[[1]]) %bind% evalue_formula %bind% name_drop)) .seq |> 
	name_as(.names) |> 
	base::mapply(FUN = \ (i) if (conds[i]) 
		.extractor(yes_formulas[i]) else 
			.extractor(no_formulas[i])) |> 
	base::identity()

#| > condz_lazy(c(T,F,T,T), c(~1+1, ~{usethis::ui_info("X");0},~1,~1), c(~1+1, ~6,~1,~{message(0);0}))
#| [1] 2 6 1 1
#| > condz_lazy(c(T,T,T,T), c(~1+1, ~{usethis::ui_info("X");0},~1,~1), c(~1+1, ~6,~1,~{message(0);0}))
#| ℹ X
#| [1] 2 0 1 1
#| > condz_lazy(c(T,F,T,F), c(~1+1, ~{usethis::ui_info("X");0},~1,~1), c(~1+1, ~6,~1,~{message(0);0}))
#| 0
#| [1] 2 6 1 0
#| > condz_lazy(c(T,T,T,F), c(~1+1, ~{usethis::ui_info("X");0},~1,~1), c(~1+1, ~6,~1,~{message(0);0}))
#| ℹ X
#| 0
#| [1] 2 0 1 0

# closure_lazy = function (expr) \ () base::eval(base::substitute(expr))
