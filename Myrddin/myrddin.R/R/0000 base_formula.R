



# base::'~'(a = 1+1, 2-2, list(c = 3:6, d = 4:7), data.frame(x = seq(4), y = 6:9)) |> as.list() |> tail(-1) |> magrittr::'%>%'(base::'names<-'(.,.)) |> lapply(eval)

formulaz_new = function (
		..., 
		.tail = base::c(-1, -0.1)[base::eval(.tail_type)(1)], 
		.tail_type = base::as.symbol('-')) `~`(...) |> 
	base::substitute() |> 
	base::eval() |> 
	utils::tail(.tail) |> 
	base::identity()

eval_exprs = function (
		..., 
		.formulaz = formulaz_new(..., .tail = .tail, .tail_type = .tail_type), 
		.tail = base::c(-1, -0.1)[base::eval(.tail_type)(1)], 
		.tail_type = base::as.symbol('+')) .formulaz |> 
	base::lapply(base::eval) |> 
	namelacked_as(base::as.character(base::as.list(.formulaz))) |> 
	base::identity()

#| > eval_exprs(a = 1+1, 2-2, list(c = 3:6, d = 4:7), data.frame(x = seq(4), y = 6:9), .tail_type = `-`)
#| $``~``
#| .Primitive("~")
#| 
#| $a
#| [1] 2
#| 
#| $`2 - 2`
#| [1] 0
#| 
#| $`list(c = 3:6, d = 4:7)`
#| $`list(c = 3:6, d = 4:7)`$c
#| [1] 3 4 5 6
#| 
#| $`list(c = 3:6, d = 4:7)`$d
#| [1] 4 5 6 7
#| 
#| 
#| $`data.frame(x = seq(4), y = 6:9)`
#|   x y
#| 1 1 6
#| 2 2 7
#| 3 3 8
#| 4 4 9
#| 


eval_formula = function (
		.formula, 
		.index = base::length(.formula), 
		.env = base::sys.nframe(), 
		...) .formula |> 
	base::Reduce(
		f = \ (.fml, .i) .fml |> 
			base::'[[<-'(
				.i, 
				base::eval(
					expr = .fml[[.i]], 
					envir = .env, 
					...)), 
		init = _, 
		x = .index)

evalue_formula = function (
		.formula, 
		.index = base::length(.formula), 
		.env = base::parent.frame(), 
		...) .formula |> 
	eval_formula(.index = .index, .env = .env, ...) |> 
	base::as.list() |> 
	magrittr::'%>%'({.[.index]}) |> 
	name_as(.index) |> 
	magrittr::'%>%'({if (base::length(.) > 1) . else base::unlist(.)}) |> 
	base::identity()

evalue_formulas = function (.formulas, .env = base::parent.frame(), ...) base::c(.formulas) |> 
	base::lapply(\ (.formula) .formula |> evalue_formula(.env = .env, ...)) |> 
	namelacked_as(base::as.character(base::c(.formulas)))

evaldt_formulas = function (
		.dt, 
		..., 
		.filter = T, 
		..formulas = base::list(...)) data.table::as.data.table(.dt) |> 
	magrittr::'%>%'(.[
		.filter, 
		evalue_formulas(
			.formulas = ..formulas, 
			.env = .)])

parse_formulas = function (text) base::parse(text = text) |> 
	# base::as.list() |> 
	# base::append(x = base::c, values = _) |> 
	# base::as.call() |> 
	# base::eval()
	base::lapply(base::eval) |> 
	name_as(base::names(text)) |> 
	namelacked_as(text) |> 
	base::identity()

#| > c("~pressure == 987", a = "~wind == 25") |> parse_formulas()
#| $`~pressure == 987`
#| ~pressure == 987
#| <environment: 0x55fa8caa3530>
#| 
#| $a
#| ~wind == 25
#| <environment: 0x55fa8caa3530>
#| 

#| > a = 7
#| > eval_formula(a ~ 1 + a)
#| a ~ 8
#| > eval_formula(~ 1 + a)
#| ~8
#| > eval_formula(a ~ 1 + a, c(2,3))
#| 7 ~ 8
#| > eval_formula(a ~ 1 + a, c(1,2,3))
#| .Primitive("~")(7, 8)
#| > eval_formula(a ~ 1 + a, c(1,2,3)) |> eval_formula(c(1,2,3))
#| .Primitive("~")(7, 8)

#| > evalue_formula(a ~ 1 + a)
#| 3 
#| 8 
#| > evalue_formula(a ~ 1 + a, c(1,2,3))
#| $`1`
#| .Primitive("~")
#| 
#| $`2`
#| [1] 7
#| 
#| $`3`
#| [1] 8
#| 
#| > evalue_formula(a ~ 1 + a, c(2,1,3))
#| $`2`
#| [1] 7
#| 
#| $`1`
#| .Primitive("~")
#| 
#| $`3`
#| [1] 8
#| 
#| > data.table::as.data.table(dplyr::storms) -> DT
#| > all(DT[, evalue_formula(~ year == 1975, .env = sys.nframe())] == DT[, year == 1975])
#| [1] TRUE
#| > all(DT[, evalue_formula(~ year == 1975)] == DT[, year == 1975])
#| [1] TRUE
#| > DT[, .(evalue_formula(~ year == 1975))] |> diffdf::diffdf(DT[, .(year == 1975)])
#| No issues were found!
#| > DT[, .(evalue_formula(~ wind == 25))] |> diffdf::diffdf(DT[, .(wind == 25)])
#| No issues were found!
#| > DT[, .(evalue_formula(~ year == 1975), evalue_formula(~ wind == 25))] |> diffdf::diffdf(DT[, .(year == 1975, wind == 25)])
#| No issues were found!

#| > evalue_formulas(c(~ 1+1, b = ~ 0))
#| $`~1 + 1`
#| 2 
#| 2 
#| 
#| $b
#| 2 
#| 0 
#| 
#| > DT |> evaldt_formulas(x = ~ year == 2022, ~ wind == 25)
#|             x ~wind == 25
#|        <lgcl>      <lgcl>
#|     1:  FALSE        TRUE
#|     2:  FALSE        TRUE
#|     3:  FALSE        TRUE
#|     4:  FALSE        TRUE
#|     5:  FALSE        TRUE
#|    ---                   
#| 19533:   TRUE       FALSE
#| 19534:   TRUE       FALSE
#| 19535:   TRUE       FALSE
#| 19536:   TRUE        TRUE
#| 19537:   TRUE        TRUE

