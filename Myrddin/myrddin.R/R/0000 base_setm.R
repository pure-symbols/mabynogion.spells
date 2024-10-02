
#' set functions
#' ref: https://en.wikipedia.org/wiki/Set_(mathematics)#Basic_operations
#' 

`%set.diff%` = function (a,b) 
	#' > If A and B are sets, then the relative complement of A in B,
	#' >  also termed the set difference of B and A,
	#' >  is the set of elements in B but not in A.
	#' > 
	#' 
	#' ref: https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement
	#' 
	#' logic: `a [! a %in% b]`
	#' means: get elements of `a` which is not in `b`
	#' 
	base::setdiff (a,b) ;

`%set.intersect%` = function (a,b) 
	#' > In set theory, the intersection of two sets A and B,
	#' >  denoted by A ∩ B, is the set containing all elements of A that also belong to B
	#' >  or equivalently, all elements of B that also belong to A.
	#' > 
	#' 
	#' ref: https://en.wikipedia.org/wiki/Intersection_(set_theory)
	#' 
	#' same as `generics::intersect` or `base::intersect` in R version ≥ 4.3: 
	#' 
	a %set.diff% (a %set.diff% b) ;

`%set.union%` = function (a,b) 
	#' > In set theory, the union (denoted by ∪) of a collection 
	#' >  of sets is the set of all elements in the collection.
	#' > 
	#' 
	#' ref: https://en.wikipedia.org/wiki/Union_(set_theory)
	#' 
	base::append(a,b) %>% (base::unique) ;

`%set.sum%` = function (a,b) 
	#' > In mathematics, the symmetric difference of two sets,
	#' >  also known as the disjunctive union and set sum,
	#' >  is the set of elements which are in either of the sets,
	#' >  but not in their intersection.
	#' > 
	#' 
	#' ref: https://en.wikipedia.org/wiki/Symmetric_difference
	#' 
	(a %set.diff% b) %set.union% (b %set.diff% a) ;


#' @name set.surplus
#' @aliases `%set.surplus%`
#' 
#' @param lhs a vector
#' @param rhs a set
#' 
#' @examples 
#' 
#' c(a="x",b="y",c="z",d="z",e="v") %set.surplus% c("x","y")
#' #|   c   d   e 
#' #| "z" "z" "v" 
#' 
#' 
set.surplus = `%set.surplus%` = function (a, b) a [! a %in% base::unique(b)]
