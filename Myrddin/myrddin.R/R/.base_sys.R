

env_load = function (
		env_name = '', 
		.default = '', 
		.keep_name = T, 
		.class = c('Dlist', 'character')) env_name |> 
	magrittr::'%T>%'({tictoc::tic("★ Load envs")}) |> 
	base::Sys.getenv(unset = .default) |> 
	magrittr::'%T>%'({tictoc::toc()}) |> 
	name_as(if (.keep_name) env_name else {}) |> 
	class_as(if (.keep_name) .class else {}) |> 
	magrittr::'%T>%'({if (.keep_name) base::print(.) else {}}) |> 
	base::identity()

#| > env_load(c("AAA","BBB"),'xx',.keep_name = F)
#| ★ Load envs: 0 sec elapsed
#| [1] "xx" "xx"
#| > env_load(c("AAA","BBB"),'xx') -> a
#| ★ Load envs: 0 sec elapsed
#| AAA                                       xx
#| BBB                                       xx
#| > a
#| AAA                                       xx
#| BBB                                       xx
#| > env_load(c('AAA','LD_LIBRARY_PATH','KUBERNETES_PORT_443_TCP_ADDR','RSTUDIO_STANDALONE_PORT'),"aaabc") -> b
#| ★ Load envs: 0 sec elapsed
#| AAA                                       aaabc
#| LD_LIBRARY_PATH                           /usr/local/lib/R/lib:/usr/lib/jvm/java-11-openjdk-amd64/lib/server
#| KUBERNETES_PORT_443_TCP_ADDR              192.168.0.1
#| RSTUDIO_STANDALONE_PORT                   8788

