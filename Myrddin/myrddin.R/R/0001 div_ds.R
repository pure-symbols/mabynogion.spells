
# div: 
# https://stackoverflow.com/questions/37145863/splitting-a-data-frame-into-equal-parts
# https://stackoverflow.com/questions/14164525/splitting-a-large-data-frame-into-smaller-segments
# https://stackoverflow.com/questions/58278200/how-to-split-a-file-in-several-files-by-a-condition-in-r
# https://lobstr.r-lib.org/reference/obj_size.html
# 
# byte: 
# https://stackoverflow.com/questions/10910688/converting-kilobytes-megabytes-etc-to-bytes-in-r

#' 此处应使用 `utils::object.size` 而非 `lobstr::obj_size` .
#' 
#'   x <- stats::runif(1e4) #< [1] 0.5347077383194 0.6884295991622 ...
#'   z <- list(a = x, b = x, c = x)
#'   lobstr::obj_size(x) #> 80.05 kB
#'   utils::object.size(x) #> 80048 bytes
#'   lobstr::obj_size(z) #> 80.49 kB
#'   utils::object.size(z) #> 240584 bytes
#' 
#' 此处需要的是内容大小，而不是其在内存中的实际占用量大小。
#' 
#' 另外根据对除法的模拟， `div_bytes` 表示一片最多这么多。
#' 
#' stats::runif(64) |> divide_size(fs::as_fs_bytes("0.03 KiB")) # 最多 0.03 KiB 一份的话切分拓扑是什么
#' 
divide_size = function (
		.x, 
		div_bytes, 
		len_by = base::length, 
		.len = len_by(.x)) 
{
	avgsize_bylen = utils::object.size(.x) / .len
	div_len = base::as.integer(div_bytes / base::as.numeric(avgsize_bylen))
	# div_topology = base::ceiling(base::seq(.len) / div_len)
	div_topology = sequence_div(.len, div_len)
	len_div = div_topology[base::length(div_topology)]
	base::list(
		len_input = .len, 
		per_avgsize = avgsize_bylen, 
		per_len = div_len, 
		len_divided = len_div, 
		divide_topology = div_topology)
}



dividesize_df = function (
		.df, 
		div_bytes, 
		.len_by = base::nrow) .df |> 
	divide_size (
		div_bytes = div_bytes, 
		len_by = .len_by)

dividesize_seq = function (
		.seq, 
		div_bytes, 
		.len_by = base::length) .seq |> 
	divide_size (
		div_bytes = div_bytes, 
		len_by = .len_by)



dividebytes_df = function (
		.df, 
		div_bytes, 
		.len_by = base::nrow) .df |> 
	base::split(.df |> dividesize_df(div_bytes)) |> 
	base::identity()


dividebytes_lines = function (
		.conn, 
		div_bytes) 
{
	.contents = base::readLines(.conn)
	
	.divider = .contents |> 
		dividesize_seq(
			.len_by = base::length, 
			div_bytes = div_bytes) |> 
		base::identity()
	
	base::list(
		.divider = .divider, 
		contents = .contents |> 
			base::split(.divider$divide_topology) |> 
			base::identity())
}





# divide_file_content = function (path, bytes) 
# {
#   lines = readLines(path)
#   len = length(lines)
#   
#   avgsize = utils::object.size(lines) / len
#   part_len = base::as.integer(bytes / base::as.numeric(avgsize))
#   
#   div_topo = base::ceiling(base::seq(len) / part_len)
#   
#   div_lines = lines |> base::split(div_topo)
#   return(div_lines)
# }
# 
# 
# divide_file = function (
#     path, 
#     bytes_size, 
#     .prefix = paste0(tools::file_path_sans_ext(path), .sp_prefix), 
#     .endfix = paste0(.sp_endfix, tools::file_ext(path)), 
#     .sp_prefix = "_", 
#     .sp_endfix = ".") 
# {
#   dv = divide_file_content(path, bytes_size)
#   path = .prefix |> 
#     paste0(names(dv)) |> 
#     paste0(.endfix)
#   usethis::ui_info("Writing into files: {usethis::ui_value(path)}")
#   names(dv) = path
#   dv |> purrr::imap(readr::write_lines)
#   return(path)
# }



# https://stackoverflow.com/questions/29214932/split-a-file-path-into-folder-names-vector

dividefile_lines = function (
		.path, 
		bytes_per, 
		.names_prefix = base::paste0(tools::file_path_sans_ext(.path), ..prefix_sp), 
		.names_endfix = base::paste0(..endfix_sp, tools::file_ext(.path)), 
		..prefix_sp = "_", 
		..endfix_sp = ".") 
{
	divided = .path |> dividebytes_lines(bytes_per)
	
	pathes = .names_prefix |> 
		base::paste0(base::names(divided$contents)) |> 
		base::paste0("in", base::length(base::names(divided$contents))) |> 
		base::paste0(.names_endfix)
	
	base::names(divided$contents) = pathes
	
	writelines_list(divided$contents)
	
	base::list(
		.divider = divided$.divider, 
		path = pathes)
}