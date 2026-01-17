
# todo: if object name same between many env, the size will only the first...
memobj_list = function (
		env_name = base::names(.envpos), 
		.envpos = memenv_ls(), 
		.sizer = utils::object.size) .envpos[env_name] |> 
	base::lapply(memobj_ls) |> 
	liapply(\ (obj, env) data.table::data.table(env = env, obj = obj)) |> 
	data.table::rbindlist() |> 
	dplyr::mutate(size = base::Vectorize(.sizer)(obj)) |> 
	base::identity()

# memobj_list = function () data.table::data.table(name = memobj_ls()) |> 
# 	dtplyr::lazy_dt(immutable = F) |> 
# 	dplyr::mutate(size.expr = base::parse(text = glue::glue("utils::object.size({name})"))) |> 
# 	dplyr::mutate(size = Vectorize(eval)(size.expr)) |> 
# 	dplyr::mutate(size.expr = as.character(size.expr)) |> 
# 	dplyr::arrange(-size) |> 
# 	data.table::as.data.table()


#| > memobj_list()
#|                   env                   obj  size
#|                <char>                <char> <num>
#|    1:      .GlobalEnv                     a   112
#|    2:      .GlobalEnv                     b   112
#|    3: package:myrddin       .splidiff_demos   120
#|    4: package:myrddin       .xrosslist_demo   120
#|    5: package:myrddin                 %///%   112
#|   ---                                            
#| 4280:    package:base xtfrm.numeric_version   136
#| 4281:    package:base         xtfrm.POSIXct   120
#| 4282:    package:base         xtfrm.POSIXlt   120
#| 4283:    package:base                xzfile   112
#| 4284:    package:base              zapsmall   120
#| > memobj_list('.GlobalEnv')
#|           env    obj  size
#|        <char> <char> <num>
#| 1: .GlobalEnv      a   112
#| 2: .GlobalEnv      b   112


memobjls_accsize = function (memls) memls |> 
	dplyr::mutate(.rnk = dplyr::row_number()) |> 
	dplyr::group_by(env) |> 
	dplyr::mutate(.rnk = base::min(.rnk)) |> 
	dplyr::arrange(+ size) |> 
	dplyr::mutate(size_acc = scanover(base::sum)(size, dplyr::row_number())) |> 
	dplyr::arrange(
		+ .rnk, 
		- size) |> 
	dplyr::select(- .rnk) |> 
	base::identity()

#| > memobj_list() |> dtplyr::lazy_dt() |> memobjls_accsize() |> data.table::as.data.table()
#|                   env                 obj  size size_acc
#|                <char>              <char> <num>    <num>
#|    1:      .GlobalEnv                   a   112      112
#|    2:      .GlobalEnv                   b   112      224
#|    3: package:myrddin combinecsv_workbook   136    18320
#|    4: package:myrddin    default_of_basic   136    18456
#|    5: package:myrddin   dividebytes_lines   136    18592
#|   ---                                                   
#| 4283:    package:base              within   112    48496
#| 4284:    package:base               write   112    48608
#| 4285:    package:base                 xor   112    48720
#| 4286:    package:base               xtfrm   112    48832
#| 4287:    package:base              xzfile   112    48944
#| > memobj_list('package:base') |> dtplyr::lazy_dt() |> memobjls_accsize() |> data.table::as.data.table()
#|                env                                obj  size size_acc
#|             <char>                             <char> <num>    <num>
#|    1: package:base getDLLRegisteredRoutines.character   152   167440
#|    2: package:base   getDLLRegisteredRoutines.DLLInfo   152   167592
#|    3: package:base               .__S3MethodsTable__.   136   133560
#|    4: package:base                  .BaseNamespaceEnv   136   133696
#|    5: package:base               .C_R_addTaskCallback   136   133832
#|   ---                                                               
#| 1388: package:base                             within   112    48496
#| 1389: package:base                              write   112    48608
#| 1390: package:base                                xor   112    48720
#| 1391: package:base                              xtfrm   112    48832
#| 1392: package:base                             xzfile   112    48944


