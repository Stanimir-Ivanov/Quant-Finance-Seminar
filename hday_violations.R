sp_ten_day <- rollapply(sp_data, 10, sum)
violations <- sp_ten_day[1001:length(sp_ten_day)] > res_sp_garch$`10 Day 0.99`