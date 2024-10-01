
class_as = function (.x, as_class) .x |> attr_assign(class = as_class)

#| > c(A = 1, b = 2) |> attr_assign(class = 'Dlist')
#| A                                         1
#| b                                         2
#| > c(A = 1, b = 2) |> attr_assign(class = 'Dlist', names = c('D','C'))
#| D                                         1
#| C                                         2
