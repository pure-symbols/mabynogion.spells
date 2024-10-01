
list_files = function (path) name_asself(base::list.files(path)) |> 
	base::lapply(\ (file) path |> base::file.path(file)) |> 
	base::identity()


load_file = readr::read_file
file_loader = memoise::memoise(load_file)

#' @param file_glob i.e., '*.R'
#' @param file_regexp i.e., '.*\\.R$'
#' 
load_files = function (
		dir_path = '.', 
		reader = readr::read_file, 
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
	purrr::imap(readr::write_lines)
