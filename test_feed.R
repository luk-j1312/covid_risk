library(quantmod)
library(QRM)
library(xts)
library(zoo)
library(tseries)
set.seed(7)
tickers <- c("WMT", "UAL", "NFLX", "ETSY", "XOM", "PFE", "REGN",
             "JPM", "MSFT", "BA", "DUK", "DIS", "TSLA", "NVDA", "SPG")
sector <- c(WMT="Consumer Staples", UAL="Airlines/Travel", NFLX="Media",
            ETSY="Consumer Discretionary",XOM="Energy",PFE="Healthcare",
            REGN="Healthcare",JPM="Banking",MSFT="Technology", 
            BA="Aerospace & Defense", DUK="Utilities",DIS="EntertDiscretionary",
            TSLA="Autos",NVDA="Semis",SPG="Real Estate")
sector_map <- data.frame(ticker = tickers,sector = sector[tickers],row.names=NULL) 
start_date <- "2019-01-01";end_date <- "2022-12-31"
getSymbols(tickers, src="yahoo", from = start_date, to= end_date) 
adj_prices <- do.call(merge, lapply(tickers, function(tk) Ad(get(tk))))
colnames(adj_prices) <- colnames(close_prices) <- colnames(volume) <- tickers
saveRDS(list(adjusted=adj_prices, close=close_prices, volume=volume, sector_map=sector_map, pulled_on=Sys.Date()), 
        file = file.path("data_raw", "combined_snapshot.rds"))
first_last <- data.frame(first_obs = as.Date(sapply(tickers, function(tk) index(na.omit(adj_prices[, tk]))[1])),
                         last_obs=as.Date(sapply(tickers, function(tk) tail(index(na.omit(adj_prices[, tk])), 1))),
                         n_obs =sapply(tickers, function(tk) sum(!is.na(adj_prices[, tk]))))
log_returns <- diff(log_prices)[-1,]
zero_mask <- (log_returns == 0)
log_returns[zero_mask] <- NA
cat("confirm it took effect (should match count above):",
    sum(is.na(log_returns)), "\n")
affected_dates <- index(log_returns)[rowSums(zero_mask,na.rm=TRUE) >0]
extrem_threshold <- 0.15
returns_matr <- as.matrix(log_returns)
idx <- which(abs(returns_matr) > extrem_threshold, arr.ind=TRUE)
extrem_table <- data.frame(
  date       = index(log_returns)[idx[,1]],
  ticker     = colnames(returns_matr)[idx[,2]],
  log_return = round(returns_matr[idx],4)
)
extrem_table <- extrem_table[order(extrem_table$date),]

snap <- readRDS("data_raw/combined_snapshot.rds")
adj_prices <- snap$adjusted
sector_map <- snap$sector_map
tickers <- sector_map$ticker
log_returns <- readRDS("data_raw/log_returns_diagnostic.rds")
returns_matr <- as.matrix(log_returns)
dates <-index(log_returns)

skewness <- function(x) {
  x <- x[!is.na(x)]
  mean((x-mean(x))^3)/sd(x)^3
}
excess_kurtosis <- function(x) {
  x <- x[!is.na(x)]
  mean((x-mean(x))^4)/sd(x)^4 - 3
}
normality_summary <- data.frame(
  ticker = tickers,mean = colMeans(returns_matr, na.rm=TRUE), sd=apply(returns_matr,2,sd, na.rm=TRUE),
  skewness = apply(returns_matr,2, skewness), exc_kurtosis=apply(returns_matr,2,excess_kurtosis),
  jb_stat = NA_real_, jb_pvalue = NA_real_)
for (i in seq_along(tickers)) {
  jb <- jarque.bera.test(na.omit(returns_matr[,tickers[i]]))
  normality_summary$jb_stat[i] <- unname(jb$statistic)
  normality_summary$jb_pvalue[i] <- jb$p.value
}
mardia_test <- function(X) {
  n <- nrow(X); d<- ncol(X)
  Xbar <- colMeans(X)
  S <- ((n-1)/n)*cov(X)
  Sinv <- solve(S)
  Xc <- sweep(X,2,Xbar,"-")
  G <- Xc %*% Sinv %*% t(Xc)  # skewness needs the full matrix (cross terms), kurtosis only its diagonal
  b1 <- sum(G^3)/n^2  # multivariate skewness stat
  b2 <- sum(diag(G)^2)/n  # multivariate kurtosis stat
  skew_stat <- (n/6)*b1
  skew_df <- d*(d+1) *(d+2)/6
  skew_pval <- pchisq(skew_stat,df=skew_df, lower.tail=FALSE)
  kurt_mean <- d*(d+2)
  kurt_sd <- sqrt(8 * d * (d + 2) / n)
  kurt_stat <- (b2-kurt_mean)/kurt_sd
  kurt_pval <- 2*pnorm(-abs(kurt_stat))
  list(n=n, d=d, b1=b1, b2=b2,
       skew_stat=skew_stat,skew_df=skew_df, skew_pval=skew_pval,
       kurt_stat=kurt_stat,kurt_pval=kurt_pval,maha_sq=diag(G))
}  # Gemini basically coded all the below due to my poor formatting and syntax knowledge
mardia_res <- mardia_test(na.omit(returns_matr))
cat("=== Mardia test for joint multiv. normality, 15 stocks ===\n")
cat(sprintf("n = %d, d = %d\n", mardia_res$n, mardia_res$d))
cat(sprintf("Skewness stat = %.2f  (df = %.0f)   p-value = %.4g\n",
            mardia_res$skew_stat, mardia_res$skew_df, mardia_res$skew_pval))
cat(sprintf("Kurtosis stat (z) = %.2f              p-value = %.4g\n",
            mardia_res$kurt_stat, mardia_res$kurt_pval))










