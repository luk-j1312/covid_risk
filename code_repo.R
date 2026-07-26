install.packages("quantmod")
install.packages("QRM")
install.packages("tseries")
install.packages("xts")
install.packages("zoo")
library(quantmod)
library(QRM)
library(xts)
library(zoo)
library(tseries)
set.seed(7)

required_pkgs <- c("quantmod", "xts", "zoo")
new_pkgs <- required_pkgs[!(required_pkgs %in% 
                              installed.packages()[,"Package"])]
if (length(new_pkgs)) install.packages(new_pkgs)
invisible(lapply(required_pkgs, library, character.only=TRUE))

tickers <- c("WMT", "UAL", "NFLX", "ETSY", "XOM", "PFE", "REGN",
             "JPM", "MSFT", "BA", "DUK", "DIS", "TSLA", "NVDA", "SPG")
sector <- c(WMT="Consumer Staples", UAL="Airlines/Travel", NFLX="Media",
            ETSY="Consumer Discretionary",XOM="Energy",PFE="Healthcare",
            REGN="Healthcare",JPM="Banking",MSFT="Technology", 
            BA="Aerospace & Defense", DUK="Utilities",DIS="EntertDiscretionary",
            TSLA="Autos",NVDA="Semis",SPG="Real Estate")
sector_map <- data.frame(ticker = tickers,sector = sector[tickers],row.names=NULL) 
start_date <- "2019-01-01";end_date <- "2022-12-31"  # in-sample 19--21, backtest 22
getSymbols(tickers, src="yahoo", from = start_date, to= end_date) 
adj_prices <- do.call(merge, lapply(tickers, function(tk) Ad(get(tk))))
close_prices <-do.call(merge, lapply(tickers, function(tk) Cl(get(tk))))
volume <- do.call(merge, lapply(tickers, function(tk) Vo(get(tk))))
colnames(adj_prices) <- colnames(close_prices) <- colnames(volume) <- tickers
dir.create("data_raw", showWarnings =FALSE)  # ensures reproducibility
for (tk in tickers) {
  write.zoo(get(tk), file=file.path("data_raw", paste0(tk,"_raw.csv")), sep=",", index.name="Date")
}  # all subsequent scripts should not recall getSymbols bc if Yahoo's adjusted-close history changes
   # (e.g., due to corporate actions) the report's info won't be reproducible
   # just load in combined_snapshot.rds
saveRDS(list(adjusted=adj_prices, close=close_prices, volume=volume, sector_map=sector_map, pulled_on=Sys.Date()), 
        file = file.path("data_raw", "combined_snapshot.rds"))
# 1008 rows (252 trading days * 4 years)
cat("=== dimensions (rows=trading days, cols=stocks) ===\n"); print(dim(adj_prices))
cat("\n=== class / periodicity ===\n"); print(class(adj_prices)); print(periodicity(adj_prices))
cat("\n=== time window ===\n"); print(range(index(adj_prices)))
first_last <- data.frame(first_obs = as.Date(sapply(tickers, function(tk) index(na.omit(adj_prices[, tk]))[1])),
  last_obs=as.Date(sapply(tickers, function(tk) tail(index(na.omit(adj_prices[, tk])), 1))),
  n_obs =sapply(tickers, function(tk) sum(!is.na(adj_prices[, tk]))))
cat("\n=== first/last observation and count ===\n"); print(first_last)  # dates and obs should match
cat("\n=== NA counts ===\n")  # this is empty (obs above matches dimension of original)
print(sort(colSums(is.na(adj_prices)), decreasing = TRUE))  # actually shows it

cat("\n=== TSLA around AUG 2020 split; raw vs. adjusted close ===\n")
print(cbind(RawClose = close_prices["2020-08-20/2020-09-10", "TSLA"], AdjClose = adj_prices["2020-08-20/2020-09-10", "TSLA"]))
cat("\n=== NVDA around JUL 2021 split; raw vs. adjusted close ===\n")
print(cbind(RawClose= close_prices["2021-07-12/2021-07-28", "NVDA"], AdjClose = adj_prices["2021-07-12/2021-07-28", "NVDA"]))
cat("\n=== TSLA splits on record (2019-2022) ===\n")
print(getSplits("TSLA", from = start_date,to = end_date))
cat("\n=== NVDA splits on record (2019-2022) ===\n")
print(getSplits("NVDA", from =start_date, to = end_date))
cat("\n=== SPG dividends per share (2019-2022) ===\n")  # look for 2020 cut
print(getDividends("SPG", from = start_date, to = end_date))

log_prices <- log(adj_prices)
log_returns <- diff(log_prices)[-1,]
zero_return_count <- colSums(log_returns ==0, na.rm=TRUE)
cat("\n=== zero-log-return day frequency ===\n")
print(sort(zero_return_count, decreasing=TRUE))
worst_zero <-names(which.max(zero_return_count))
cat("\n=== sample zero-return dates for", worst_zero, "===\n")  # for illustration purposes
print(head(index(log_returns)[log_returns[, worst_zero]==0],20))
zero_mask <- (log_returns == 0)
cat("\n=== zero-return cells found:", sum(zero_mask, na.rm=TRUE), "out of",
    length(log_returns), sprintf("(%.2f%%)", 100*sum(zero_mask,na.rm=TRUE)/length(log_returns)),"===\n")
log_returns[zero_mask] <- NA
cat("confirm it took effect (should match count above):",
    sum(is.na(log_returns)), "\n")
affected_dates <- index(log_returns)[rowSums(zero_mask,na.rm=TRUE) >0]
cat("\n=== dates with >=1 stock affected (cross-check against extreme-moves table printed further below for any overlap) ===\n")
print(affected_dates)

png("data_raw/fig_indexed_prices.png", width=1600, height=1000, res=150)
price_matr <- as.matrix(adj_prices)
indexed_matr <- sweep(price_matr,2, price_matr[1,], "/")*100
indexed_prices <- xts(indexed_matr, order.by=index(adj_prices))
plot.zoo(indexed_prices, plot.type="single", col=rainbow(15), lwd=1,
         main="15 stocks, Adjusted Close indexed to 100 at 02JAN2019", xlab="",ylab="indexed price")
legend("topleft", legend=tickers, col =rainbow(15), lty = 1, cex=0.6,ncol=3)
dev.off()
cat("Saved: data_raw/fig_indexed_prices.png\n")
# file.show("data_raw/fig_indexed_prices.png")

# log returns, small multiples; FEB--MAR 2020 crash should look like vertical
# band of spikes at same x-position in every single panel.
dates <- index(log_returns)
returns_matr <- as.matrix(log_returns)   # you already build this later for extreme_table
png("data_raw/fig_return_small_multiples.png", width = 1400, height = 1800, res = 150)
par(mfrow = c(5,3), mar = c(2,2,2,1))
for (tk in tickers) {
  plot(dates, returns_matr[,tk], type = "l", main =tk, cex.main = 0.9, ylab = "",
       xlab="", col = "steelblue")
  abline(h= 0, col = "grey70", lty = 3)
}
par(mfrow=c(1,1))
dev.off()
cat("Saved: data_raw/fig_return_small_multiples.png\n")
# file.show("data_raw/fig_return_small_multiples.png")

# for extreme moves, flag anything beyond +-15% for manual cross-referencing against dated-news list
# note that this is not a removal per se
# also note that Gemini was used lines 90--102 (to which I made some minor personal formatting edits for consistency with already-used stylistic preferences)
extrem_threshold <- 0.15
returns_matr <- as.matrix(log_returns)
idx <- which(abs(returns_matr) > extrem_threshold, arr.ind=TRUE)
extrem_table <- data.frame(
  date       = index(log_returns)[idx[,1]],
  ticker     = colnames(returns_matr)[idx[,2]],
  log_return = round(returns_matr[idx],4)
)
extrem_table <- extrem_table[order(extrem_table$date),]
cat("\n=== daily log returns beyond +-15% ===\n")
print(extrem_table)
cat("\n=== any non-positive adjusted prices? ===\n")  # sanity checks suggested by Gemini
print(any(adj_prices <= 0,na.rm=TRUE))
cat("\n=== date index strictly increasing (no duplicates/out-of-order)? ===\n")
print(!is.unsorted(index(adj_prices), strictly= TRUE))

write.csv(first_last, "data_raw/inspection_first_last.csv", row.names=FALSE)
write.csv(extrem_table, "data_raw/inspection_extreme_moves.csv", row.names=FALSE)
saveRDS(log_returns,"data_raw/log_returns_diagnostic.rds")
cat("\nDone. Raw snapshot and inspection artifacts written to ./data_raw/\n")


#///////////////////////////////////////////////////////////////////////////////


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
normality_summary <- normality_summary[order(-normality_summary$exc_kurtosis), ]
print_table_summary <- normality_summary
print_table_summary[,-1]<- round(print_table_summary[,-1], 4)
cat("=== skewness / excess kurtosis / JB test, across all 15 stocks ===\n")
print(print_table_summary)

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
cat(sprintf("(Under H0, kurtosis b2 should average d(d+2) = %d; result is %.2f)\n",
            mardia_res$d * (mardia_res$d + 2), mardia_res$b2))
png("data_raw/fig_mardia_qq.png", width = 1000, height = 1000, res = 150)
qqplot(qchisq(ppoints(mardia_res$n), df = mardia_res$d), mardia_res$maha_sq,
       xlab = bquote(chi[.(mardia_res$d)]^2 ~ "quantile"),
       ylab = "Squared Mahalanobis distance (daily obs.)",
       main = "Mardia QQ-plot: joint normality (linear = normal)",
       pch = 20, cex = 0.5)
abline(0, 1, col = "red")
dev.off()
cat("Saved: data_raw/fig_mardia_qq.png\n")
# file.show("data_raw/fig_mardia_qq.png")

png("data_raw/fig_qqplots.png", width=1400, height=1800, res=150)
par(mfrow = c(5,3),mar= c(2,2,2,1))
for (tk in tickers) {  # QQ plots
  qqnorm(returns_matr[,tk], main=tk,cex.main=0.9,pch=20,cex=0.5)
  qqline(returns_matr[,tk], col="red")
}
par(mfrow = c(1,1))
dev.off()
cat("Saved: data_raw/fig_qqplots.png\n")
# file.show("data_raw/fig_qqplots.png")

# pairwise return scatters
png("data_raw/fig_pairs_overview.png", width=2400, height=2400, res=150)
pairs(returns_matr,col=rgb(0,0,0,0.15), pch=20, cex=0.4,
      main="Pairwise log-return scatter, all 15 stocks")
dev.off()
cat("Saved: data_raw/fig_pairs_overview.png\n")
# file.show("data_raw/fig_pairs_overview.png")  # will need to zoom in for this
# within-sector as well as across-sector compare/contrast
pair_list <- list(
  c("MSFT","NVDA"),  # within tech (e.g., stable vs. high-beta)
  c("PFE","REGN"),  # within healthcare (e.g., diversified vs. focused biotech)
  c("UAL","WMT"),  # across sectors (e.g., crasher vs. defensive)
  c("TSLA","DUK")  # across sectors (e.g., surger vs. defensive)
)
png("data_raw/fig_pairs_targeted.png", width=1400, height=1400, res=150)
par(mfrow = c(2,2), mar = c(4,4,2,1))
for (p in pair_list) {
  plot(returns_matr[, p[1]], returns_matr[,p[2]],
       xlab=p[1], ylab = p[2], main= paste(p[1], "vs.",p[2]),
       pch =20, col=rgb(0,0,0.6,0.3),cex=0.6)
  abline(h=0, v=0, col = "grey80",lty=3)
}
par(mfrow = c(1,1))
dev.off()
cat("Saved: data_raw/fig_pairs_targeted.png\n")
# file.show("data_raw/fig_pairs_targeted.png")

tail_probs <- c(0.01,0.05,0.95,0.99)
ratio_matr <- t(sapply(tickers, function(tk) {
  x <- returns_matr[,tk]
  mu <- mean(x, na.rm=TRUE); sigma <- sd(x, na.rm=TRUE)
  emp_q <- quantile(x, probs=tail_probs, na.rm=TRUE)
  norm_q <- qnorm(tail_probs,mean=mu, sd=sigma)
  emp_q/norm_q
}))
colnames(ratio_matr) <- paste0("q", tail_probs*100)
tail_ratio_table <- data.frame(ticker = tickers, round(ratio_matr,2))
tail_ratio_table <- tail_ratio_table[order(-tail_ratio_table$q1),]
cat("=== empirical/normal-implied quantile ratio (>1 means fatter tail than normal predicts) ===\n")
print(tail_ratio_table, row.names=FALSE)
# full correlation matrix (alt to scatter overview; Gemini helped with implementing this)
corr_matr <- round(cor(returns_matr, method="pearson", use="pairwise.complete.obs"),2)
cat("\n=== correlation matrix ===\n"); print(corr_matr)
corr_idx <- which(upper.tri(corr_matr), arr.ind=TRUE)
pair_table <- data.frame(stock_1 = rownames(corr_matr)[corr_idx[,1]],
  stock_2 = colnames(corr_matr)[corr_idx[,2]], pearson_r= corr_matr[corr_idx])
pair_table <- pair_table[order(-pair_table$pearson_r), ]
cat("\n=== 8 most correlated pairs ===\n")
print(head(pair_table,8), row.names=FALSE)
cat("\n=== 8 least/most-negatively correlated pairs ===\n")
print(tail(pair_table,8), row.names=FALSE)
tail_comovement <- function(x, y, q=0.10) {
  x_bad <- x <= quantile(x, q, na.rm=TRUE)
  y_bad <- y <= quantile(y, q, na.rm=TRUE)
  round(mean(y_bad[x_bad], na.rm=TRUE)/q,2)
}
cat("\n== Targeted pairs: correlation and lower-tail comovement ==\n")
for (p in pair_list) {
  x <- returns_matr[, p[1]]; y <- returns_matr[, p[2]]
  cat(sprintf("\n%s vs. %s\n", p[1], p[2]))
  cat(sprintf("  Pearson r    = %.2f\n", cor(x,y, method="pearson", use="complete.obs")))
  cat(sprintf("  Tail comovement (worst 10%% days): %s bad -> %s also bad: %.1fx baseline\n",
              p[1], p[2], tail_comovement(x, y, 0.10)))
  cat(sprintf("  Tail comovement (worst 10%% days): %s bad -> %s also bad: %.1fx baseline\n",
              p[2], p[1], tail_comovement(y, x, 0.10)))
}
write.csv(tail_ratio_table, "data_raw/summary_tail_ratios.csv", row.names=FALSE)
write.csv(pair_table, "data_raw/summary_correlation_pairs.csv", row.names=FALSE)


#///////////////////////////////////////////////////////////////////////////////


snap <- readRDS("data_raw/combined_snapshot.rds")  # good to add again in case top code not run
adj_prices <- snap$adjusted
tickers <- snap$sector_map$ticker
in_sample_end <- "2021-12-31"
backtest_start <- "2022-01-01"
backtest_end <- "2022-12-31"
alphas <- c(0.95,0.99)
p0 <- as.numeric(adj_prices[1,])
shares <- 1000/p0
names(shares) <- tickers
port_value <- xts(as.numeric(as.matrix(adj_prices) %*% shares),order.by=index(adj_prices))
port_loss <- -diff(port_value)[-1,]  # conventional L(t) = -(V_{t} - V_{t-1})
loss_insample  <- as.numeric(port_loss[paste0("/", in_sample_end)])
loss_backtest  <- as.numeric(port_loss[paste0(backtest_start, "/", backtest_end)])
cat(sprintf("beginning notional: $%d across %d stocks, $1000 each\n", 1000*length(tickers),length(tickers)))
cat(sprintf("in-sample days: %d | backtest (2022) days: %d\n", length(loss_insample),length(loss_backtest)))
cat("NOTE: using adjusted close throughout means this implicitly assumes dividends are reinvested, consistent with how adjusted close has been used in report.\n")
# (i) from historic distr of past losses
empirical_VaR <- sapply(alphas, function(a) quantile(loss_insample,a,names=FALSE))
empirical_ES <- sapply(seq_along(alphas), function(i)
  mean(loss_insample[loss_insample>empirical_VaR[i]]))
# (ii) Normal
mu_n <- mean(loss_insample)
sd_n <- sd(loss_insample)
normal_VaR <- mu_n+sd_n*qnorm(alphas)
normal_ES <- mu_n+sd_n*dnorm(qnorm(alphas))/(1-alphas)
# (iii) Student-t
tfit <- fit.st(loss_insample)
tpar <- tfit$par.ests
nu <- unname(tpar["nu"]); mu_t <- unname(tpar["mu"]); sigma_t <-unname(tpar["sigma"])
t_VaR <- mu_t+sigma_t*qt(alphas, df=nu)
t_ES  <- sapply(seq_along(alphas), function(i) {
  dens <- function(x) dt((x-mu_t)/sigma_t, df=nu)/sigma_t
  integrate(function(x) x*dens(x), lower=t_VaR[i], upper = Inf)$value/(1-alphas[i])
})
cat(sprintf("\nStudent-t fit: nu = %.2f, mu = %.2f, sigma = %.2f\n",nu,mu_t,sigma_t))
# compare (i), (ii), and (iii)
risk_table <- data.frame(
  model = rep(c("Empirical", "Normal", "Student-t"), each=2),
  alpha = rep(c("95%","99%"),times =3), VaR = round(c(empirical_VaR,normal_VaR,t_VaR),2),
  ES = round(c(empirical_ES,normal_ES,t_ES),2))
cat("\n=== in-sample VaR/ES, three models, portfolio of $15000 notional ===\n")
print(risk_table,row.names=FALSE)
png("data_raw/fig_loss_distribution.png", width=1400, height=1000,res=150)
hist(loss_insample, breaks=60, prob=TRUE,col="grey85", border="white",
     xlab = "Daily portfolio Loss ($)", main="In-sample Loss distr across empirical data, Normal, and t")
curve(dnorm(x,mu_n,sd_n),add=TRUE, col="blue", lwd=2)
curve(dt((x-mu_t)/ sigma_t, df=nu)/sigma_t, add=TRUE, col="red",lwd=2)
abline(v=empirical_VaR, col = "black", lty = 2)
abline(v=normal_VaR, col="blue",lty=2)
abline(v=t_VaR, col= "red", lty=2)
legend("topright",cex=0.7, lty=c(1,1,2,2,2),lwd=c(2,2,1,1,1),col=c("blue","red","black","blue","red"),
       legend = c("Normal density", "Student-t density", "Empirical VaR", "Normal VaR", "t VaR"))
dev.off()
cat("Saved: data_raw/fig_loss_distribution.png\n")
# file.show("data_raw/fig_loss_distribution.png")

# backtest
kupiec_test <- function(x, n, p) {  # tests failure proportion, see University of Manchester paper for formulae and descriptions
  phat <- x/n  # ^^ https://pure.manchester.ac.uk/ws/portalfiles/portal/60673220/back4.pdf
  ll_null <- ifelse(x==0,0,x*log(p)) + ifelse(x==n,0,(n-x)*log(1-p))
  ll_alt <- ifelse(x==0,0,x*log(phat)) + ifelse(x==n,0,(n-x)*log(1-phat))
  LR <- -2*(ll_null-ll_alt)
  list(LR = LR, pval=pchisq(LR, df = 1, lower.tail = FALSE))
}
VaR_values <- c(empirical_VaR,normal_VaR,t_VaR)
ES_values <- c(empirical_ES, normal_ES,t_ES)
model_names <- rep(c("Empirical","Normal","Student-t"), each=2)
alpha_values <- rep(alphas, times=3)
n_bt <- length(loss_backtest)
# Gemini used to write this chunk, slightly edited for compactness
backtest_results <- do.call(rbind, lapply(seq_along(VaR_values), function(i) {
  exceed <- loss_backtest > VaR_values[i]
  x <- sum(exceed)
  kt <- kupiec_test(x, n_bt, 1 - alpha_values[i])
  data.frame(
    model = model_names[i], alpha = paste0(alpha_values[i] * 100, "%"),
    VaR = round(VaR_values[i], 2), exceedances = x, expected = round(n_bt * (1 - alpha_values[i]), 1),
    kupiec_LR = round(kt$LR, 3), kupiec_pval = round(kt$pval, 4),
    realized_avg_exceedance = if (x > 0) round(mean(loss_backtest[exceed]), 2) else NA,
    model_ES = round(ES_values[i], 2))
}))
cat("\n=== 2022 backtest: exceedances, Kupiec test, and ES check ===\n")
print(backtest_results, row.names=FALSE)
write.csv(risk_table, "data_raw/task3_risk_table.csv", row.names=FALSE)
write.csv(backtest_results, "data_raw/task3_backtest_results.csv", row.names=FALSE)

# checks whether 95% failures are clustered or spread evenly throughout 2022
bt_dates <- index(port_loss[paste0(backtest_start, "/", backtest_end)])
png("data_raw/fig_2022_backtest_timeline.png", width=1600,height=900, res=150)
plot(bt_dates,loss_backtest,type="l",col="grey40",xlab = "",ylab="Daily portfolio loss ($)",
     main = "2022 daily losses vs. in-sample VaR thresholds")
abline(h = empirical_VaR[1],col = "black",lty=2)
abline(h = normal_VaR[1], col = "blue",lty=2)
abline(h = t_VaR[1],col = "red", lty=2)
abline(h = empirical_VaR[2], col = "black", lty=3)
abline(h = normal_VaR[2], col = "blue", lty=3)
abline(h = t_VaR[2], col = "red",lty=3)
legend("topleft", cex=0.65, lty=c(2,2,2,3,3,3), col=rep(c("black","blue","red"),2),
       legend = c("Empirical 95% VaR", "Normal 95% VaR", "t 95% VaR", "Empirical 99% VaR", "Normal 99% VaR","t 99% VaR"))
dev.off()
cat("Saved: data_raw/fig_2022_backtest_timeline.png\n")
# file.show("data_raw/fig_2022_backtest_timeline.png")


#//////////////////////////////////////////////////////


snap <- readRDS("data_raw/combined_snapshot.rds")
tickers <- snap$sector_map$ticker
log_returns <- readRDS("data_raw/log_returns_diagnostic.rds")
returns_matr <- as.matrix(log_returns)
returns_complete <- na.omit(returns_matr)
cat("rows dropped for the full 15-dim copula fit (needs simultaneous data):",
    nrow(returns_matr) - nrow(returns_complete), "check these also against extreme-moves table.\n")
Udata_full <- apply(returns_complete,2, edf,adjust=1)
fit_gauss_full <- fit.gausscopula(Udata_full)
fit_t_full <- fit.tcopula(Udata_full,method="Kendall")
cat("\n=== structural check (READ before accepting numbers below) ===\n")
str(fit_gauss_full, max.level=1)
cat("\n")
str(fit_t_full, max.level=1)
# verify against output above
ll_gauss <- fit_gauss_full$ll.max
ll_t <- fit_t_full$ll.max
nu_full <- fit_t_full$nu
k_gauss <- 15*14/2  # one correlation parameter per stock pair
k_t <- k_gauss+1
aic_gauss <- (-1)*2*ll_gauss + 2*k_gauss
aic_t <- (-1)*2*ll_t + 2*k_t
LR <- 2*(ll_t-ll_gauss)
LR_pval <- pchisq(LR,df=1,lower.tail=FALSE)
full_comparison <- data.frame(copula = c("Gauss","t"), n_par = c(k_gauss,k_t),
                              logLik = round(c(ll_gauss,ll_t),2), AIC = round(c(aic_gauss,aic_t),2))
cat("\n=== full 15-dim copula comparison ===\n")
print(full_comparison, row.names=FALSE)
cat(sprintf("\nt-copula fitted dof: nu = %.2f\n",nu_full))
cat(sprintf("LR test (t vs Gaussian, one extra parameter): LR = %.2f, p-value = %.4g\n",LR,LR_pval))
cor_spearman <- round(cor(returns_matr,method="spearman", use="pairwise.complete.obs"),2)
cor_kendall <- round(cor(returns_matr,method="kendall", use="pairwise.complete.obs"),2)
cat("=== Pearson vs Spearman vs Kendall, four illustrative pairs ===\n")
for (p in list(c("MSFT","NVDA"), c("PFE","REGN"), c("UAL","WMT"), c("TSLA","DUK"))) {
  cat(sprintf("%s vs %s: Pearson=%.2f  Spearman=%.2f  Kendall=%.2f\n", p[1], p[2],
              cor(returns_matr[,p[1]], returns_matr[,p[2]], method="pearson", use="complete.obs"),
              cor(returns_matr[,p[1]], returns_matr[,p[2]], method="spearman", use="complete.obs"),
              cor(returns_matr[,p[1]], returns_matr[,p[2]], method="kendall", use="complete.obs")))
}

pair_list <- list(c("MSFT","NVDA"), c("PFE","REGN"), c("UAL","WMT"), c("TSLA","DUK"))
tail_comovement <- function(x, y, q = 0.10) {
  x_bad <- x <= quantile(x,q, na.rm=TRUE)
  y_bad <- y <= quantile(y,q, na.rm=TRUE)
  round(mean(y_bad[x_bad], na.rm=TRUE)/q,2)
}
pairwise_results <- list()
set.seed(7)
for (p in pair_list) {
  pair_name <- paste(p,collapse="-")
  cat(sprintf("\n=== pair: %s ===\n",pair_name))
  x <- returns_matr[, p[1]]; y <- returns_matr[,p[2]]
  ok <- !is.na(x) & !is.na(y)
  x <- x[ok]; y <- y[ok]
  U <- cbind(edf(x, adjust=1), edf(y, adjust=1))
  fg <- fit.gausscopula(U)
  ft <- fit.tcopula(U, method="Kendall")
  fgu <- fit.AC(U,"gumbel")
  fcl <- fit.AC(U,"clayton")
  if (identical(p, pair_list[[1]])) {
    cat("=== structural check on fit.AC (Gumbel/Clayton), first pair only ===\n")
    str(fgu, max.level=1); str(fcl, max.level=1)
  }
  ll <- c(gauss=fg$ll.max,t=ft$ll.max,gumbel=fgu$ll.max, clayton=fcl$ll.max)
  npar <- c(gauss=1, t=2, gumbel=1,clayton=1)
  aic <- (-1)*2*ll + 2*npar
  comp <- data.frame(copula=names(ll), logLik= round(ll,2), AIC=round(aic,2))
  comp <- comp[order(comp$AIC),]
  print(comp, row.names=FALSE)
  winner <- comp$copula[1]
  cat(sprintf("Best fit: %s\n", winner))
  theta_gu <- fgu$par.ests
  theta_cl <- fcl$par.ests
  cat(sprintf("Gumbel theta = %.2f | Clayton theta = %.2f\n", theta_gu, theta_cl))
  lambda_gumbel <- 2 - 2^(1/theta_gu)
  lambda_clayton <- 2^(-1/theta_cl)
  nsim <- 50000
  set.seed(7)
  sim_U <- switch(winner, gauss=rcopula.gauss(nsim,Sigma=fg$P), t=rcopula.t(nsim,df=ft$nu,Sigma=ft$P),
                  gumbel=rcopula.gumbel(nsim,theta=theta_gu,d=2), clayton=rcopula.clayton(nsim,theta= theta_cl,d=2))
  sim_tailcomove <- tail_comovement(sim_U[,1], sim_U[,2],0.10)
  real_tailcomove <- tail_comovement(x, y, 0.10)
  cat(sprintf("empirical tail comovement (real data): %.1fx baseline\n", real_tailcomove))
  cat(sprintf("simulated tail comovement from fitted %s copula: %.1fx baseline\n", winner, sim_tailcomove))
  cat(sprintf("Gumbel upper-tail lambda=%.3f | Clayton lower-tail lambda=%.3f\n",
              lambda_gumbel, lambda_clayton))
  pairwise_results[[pair_name]] <- comp
}








