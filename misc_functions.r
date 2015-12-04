# This file contains some miscellaneous functions

# Computes sharpe ratio
compute_Sharpe_Ratio = function(x){
	sh = mean(x, na.rm=TRUE) / sd(x, na.rm=TRUE) * sqrt(252);  
	return(sh);
}

# Calculate performance statistics
calculate_performance = function(daily_pnl,stats) {
	# Performance of each currency
	cum_daily_pnl = apply(daily_pnl,2,cumsum);

	stats[,'average_pnl'] = apply(daily_pnl,2,mean);
	stats[,'yearly_pnl'] = apply(daily_pnl,2,mean)*252;
	stats[,'total_pnl'] = cum_daily_pnl[dim(cum_daily_pnl)[1],,drop=FALSE];
	stats[,'sharpe'] = apply(daily_pnl,2,compute_Sharpe_Ratio);
	stats= round(stats,5);
	print(stats);

	# Portfolio sharpe
	temp = rowSums(daily_pnl);
	porto_sharpe = compute_Sharpe_Ratio(temp);
	print('Portfolio Sharpe Ratio:')
	print(porto_sharpe);
}