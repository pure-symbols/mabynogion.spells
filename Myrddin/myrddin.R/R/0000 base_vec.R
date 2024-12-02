warp_if = function (cond) function (warpper) if (cond) warpper else base::identity
warpper_vec = function (.vec_ref) warp_if(base::length(.vec_ref) > 1)(base::Vectorize)

fullvec_as = function (.vec, .as) .as |> 
	base::rep(base::length(.vec)) |> 
	name_as(base::names(.vec)) |> 
	base::identity()

#| > seq(3) |> names_as('a','b','c')
#| a b c 
#| 1 2 3 
#| > seq(3) |> names_as('a','b','c') |> fullvec_as('X')
#|   a   b   c 
#| "X" "X" "X" 
