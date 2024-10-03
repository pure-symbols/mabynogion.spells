

sys_envload = function (
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

#| > sys_envload(c("AAA","BBB"),'xx',.keep_name = F)
#| ★ Load envs: 0 sec elapsed
#| [1] "xx" "xx"
#| > sys_envload(c("AAA","BBB"),'xx') -> a
#| ★ Load envs: 0 sec elapsed
#| AAA                                       xx
#| BBB                                       xx
#| > a
#| AAA                                       xx
#| BBB                                       xx
#| > sys_envload(c('AAA','LD_LIBRARY_PATH','KUBERNETES_PORT_443_TCP_ADDR','RSTUDIO_STANDALONE_PORT'),"aaabc") -> b
#| ★ Load envs: 0 sec elapsed
#| AAA                                       aaabc
#| LD_LIBRARY_PATH                           /usr/local/lib/R/lib:/usr/lib/jvm/java-11-openjdk-amd64/lib/server
#| KUBERNETES_PORT_443_TCP_ADDR              192.168.0.1
#| RSTUDIO_STANDALONE_PORT                   8788


memenv_ls = function (.ls_pos = T) base::search() |> 
	name_reverse() |> 
	(if (!.ls_pos) name_reverse else base::identity)() |> 
	base::identity()

memobj_ls = function (
		.pos = NULL, 
		.ns = NULL, 
		..., 
		..env = if 
		(!base::is.null(.pos)) base::as.environment(.pos) else if 
		(!base::is.null(.ns)) ns_take(.ns) else 
			base::parent.frame()) ..env |> 
	base::ls(
		envir = _, 
		all.names = T, 
		...)

#| > memenv_ls() |> base::lapply(memobj_ls) |> liapply(data.table::data.table) |> data.table::rbindlist()
#|                          V1              V2
#|                      <char>          <char>
#|    1:                     x      .GlobalEnv
#|    2:       .splidiff_demos package:myrddin
#|    3:       .xrosslist_demo package:myrddin
#|    4:                 %///% package:myrddin
#|    5:                %bind% package:myrddin
#|   ---                                      
#| 4277: xtfrm.numeric_version    package:base
#| 4278:         xtfrm.POSIXct    package:base
#| 4279:         xtfrm.POSIXlt    package:base
#| 4280:                xzfile    package:base
#| 4281:              zapsmall    package:base








