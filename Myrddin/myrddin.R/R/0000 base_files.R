
list_files = function (path) name_asself(base::list.files(path)) |> 
	base::lapply(\ (file) path |> base::file.path(file)) |> 
	base::identity()


load_file = readr::read_file
file_loader = memoise::memoise(load_file)

#' @param dir_path: The path you want to found something.
#' @param file_glob: To match file by glob expression. i.e., '*.R'
#' @param file_regexp: To match file by reg expression. i.e., '.*\\.R$'
#' @param reader: To specify the function for file read, default is memoised `readr::read_file`.
#' 
#' @examples
#' 
#' # find code in your package
#' myrddin::load_files('R')
#' myrddin::load_files('.', file_glob = '*.R')
#' myrddin::load_files('R') |> magrittr::'%>%'(.[. |> base::grepl('fs::', x = _, fixed = T)])
#' 
load_files = function (
		dir_path = '.', 
		reader = file_loader, 
		file_glob = NULL, 
		file_regexp = NULL, 
		type = base::c('file','FIFO','character_device'), 
		all = T, 
		recurse = F) dir_path |> 
	fs::dir_ls(
		all = all,
		recurse = recurse,
		type = type,
		glob = file_glob,
		regexp = file_regexp,
		path = _) |> 
	base::lapply(reader) |> 
	base::unlist() |> 
	base::identity()

#| > './files' |> load_files(file_glob = '*.R') -> r_files
#| > r_files[1:2]
#|   files/a.R   files/b.R 
#|       "(c)" "\nbase::c" 



writelines_list = function (
		textses, 
		pathes = name_i(textses)) textses |> 
	magrittr::'%>%'(name_as(., pathes)) |> 
	magrittr::'%T>%'({usethis::ui_info("Writing into files: {usethis::ui_value(base::names(.))}")}) |> 
	liapply(readr::write_lines)


findin_files = function (
		.pattern, 
		.dirpath = ".", 
		..., 
		.name_only = T) load_files(.dirpath, ...) |> 
	magrittr::'%>%'(.[base::grepl(.pattern, x = ., fixed = T)]) |> 
	(if (!.name_only) base::identity else base::names)() |> 
	base::identity()

#| > findin_files('sys_envload', 'R', reader = load_file)
#| [1] "R/0000 base_files.R" "R/0000 base_sys.R"  
#| > findin_files('sys_envload', recurse = T, reader = load_file)
#| [1] "R/0000 base_files.R" "R/0000 base_sys.R"  

