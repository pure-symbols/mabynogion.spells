load_rds = readr::read_rds
rds_loader = memoise::memoise(load_rds)
