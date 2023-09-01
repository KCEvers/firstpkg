# ks = 2:30
#
#
# find_regimes <- function(ks = 2:20){
#   period_per_var = peaks_df %>% group_by(variable, bifpar_idx) %>%
#     dplyr::arrange(time_idx, .by_group=TRUE) %>%
#     dplyr::group_modify(~ find_best_k(ks = ks, coord = .x$X, peak_idx = .x$peak_idx)) %>%
#     ungroup() %>%tidyr::unnest(names(.))
#
#   head(period_per_var)
#   head(peaks_df)
#   period_per_var %>% as.data.frame %>% arrange(bifpar_idx) %>% tail(n=100)
#   merge(peaks_df, period_per_var) %>% as.data.frame %>% arrange(bifpar_idx) %>% tail(n=10)
#
#   peaks_df_ = peaks_df %>% filter(bifpar_idx == 20, variable == "X1") %>% arrange(time_idx)
#   plot(peaks_df_$time, peaks_df_$X, type='l')
#   points(peaks_df_$time, peaks_df_$X, type='p', pch=16)
#   t = period_per_var %>% filter(bifpar_idx == 20, variable == "X1")
#   plot(t$k, t$coord_WCSS, type='l')
# points(t$k, t$coord_WCSS, type='p', pch=16)
#
# plot(t$k, t$peak_idx_WCSS, type='l')
# points(t$k, t$peak_idx_WCSS, type='p', pch=16)
# round(t$coord_WCSS, 5)
# pacf(t$coord_WCSS)
# # which.min(apply(WCSS_df[,2:3], 1, mean))
#
#   # Finding the best period is tricky: The global minimum might be a subharmonic, but the first local minimum might not be optimal for a more complex oscillation. If the timeseries is truly periodic, local minima in WCSS will occur for every subharmonic (e.g. period = 4, minima at period = c(4,8,12,...)).
#   coord_first_max_acf = which.max(acf(WCSS_df[,2], plot = FALSE)$acf[-1]) + 1 # Remove lag = 1)
#   peak_idx_first_max_acf = which.max(acf(WCSS_df[,3], plot = FALSE)$acf[-1]) + 1 # Remove lag = 1)
#   return(as.data.frame(cbind(WCSS_df[c(coord_first_max_acf, peak_idx_first_max_acf),], min_from = c("coord", "peak_idx"))))
#   # unlist(lapply(ks, function(k){var(WCSS_df[seq(from = k-1, to = nrow(WCSS_df), by = k),2] )}))
#
#
#   # silhouette_coeff <- function(vec, k){
#   #   cluster_idx = rep(1:k, ceiling(length(vec)/k))[1:length(vec)]
#   #   ss <- cluster::silhouette(cluster_idx, dist(as.matrix(vec)))
#   #   return(mean(ss[,3]))
#   # }
#   #   coord_silh = unlist(lapply(ks[1:(length(ks)-2)], function(k){silhouette_coeff(vec = WCSS_df[,2], k = k)}))
#   #   peak_idx_silh = unlist(lapply(ks, function(k){silhouette_coeff(vec = peak_idx, k = k)}))
#
#   # Find chaotic data: no good subharmonic
#   period_per_var %>% dplyr::filter(coord_WCSS > .2)
#   period_per_var %>% dplyr::mutate(period_corr = ifelse(coord_WCSS > .2 & peak_idx_WCSS > .2, "Chaotic", paste0("Period-", k) ))
#
#
#   ggplot(peaks_df %>%
#            merge(
#              # period_per_var
#              period_per_var %>% dplyr::mutate(period_corr = ifelse(coord_WCSS > .2 & peak_idx_WCSS > .2, "Chaotic or Transitioning", paste0("Period-", k) ))
#              ) %>%
#            dplyr::filter(variable == "X1")
#            # dplyr::filter(min_from == c("coord", "peak_idx")[2])
#          ) +# %>% tibble::add_column(cluster_idx=cluster_idx)) +
#     # geom_line(aes(x = bifpar_idx, y = X), color='grey50', linewidth=.5) +
#     geom_point(aes(x = bifpar_idx, y = X, col = factor(period_corr),
#                    # col = coord_WCSS
#                    ),
#                size = 1.05) +
#     # geom_line(aes(x = bifpar_idx, y = X),
#     #            linewidth = .5, color = 'gray50') +
#      ggh4x::facet_grid2(variable ~ .) +
#     theme_bw() + viridis::scale_color_viridis(
#       discrete=TRUE,
#       begin = 0, end = .8)
#
#
#
#
#
#
#
#
#   }
# ggplot(test) +# %>% tibble::add_column(cluster_idx=cluster_idx)) +
#   geom_line(aes(x = bifpar_idx, y = ks), color='grey50', linewidth=.5) +
#   geom_point(aes(x = bifpar_idx, y = ks),
#              size = 2.5) +
#   theme_bw() + viridis::scale_color_viridis(discrete=TRUE)
#
# X = peaks_df %>% as.data.frame() %>% dplyr::filter(bifpar_idx == 175, variable == "X1") %>%
#   arrange(time_idx) #%>% slice(-c(1:30))
# plot(X$X)
# lines(X$X, cex=.5)
# coord=X$X
# peak_idx=X$peak_idx
#
# points(X$X, col = 'red')
# lines(X$X, cex=.5, col = 'red')
#
# print(period_per_var_corr %>% as.data.frame %>% dplyr::filter(bifpar_idx > 50, bifpar_idx < 70) %>% arrange(bifpar_idx) %>% head(n=30))
#
# #
# print(merge(peaks_df, period_per_var_corr) %>% as.data.frame %>% arrange(bifpar_idx) %>% tail(n=10))
# print(period_per_var_corr %>% as.data.frame %>% dplyr::filter(bifpar_idx > 175, bifpar_idx < 200) %>% arrange(bifpar_idx) %>% head(n=30))
#
# mean_min_dist_centroids <- function(vec,cluster_idx) {
#   # From the package sarafrr/basicClEval
#   vec = as.matrix(vec)
#   # sizeCl <- summary(as.factor(cluster_idx))
#   clCenters <- tapply(vec, list(rep(cluster_idx,ncol(vec)),col(vec)),
#                       function(x) mean(x, na.rm=TRUE))
#
#   mean_min_distance = mean(unlist(lapply(1:length(clCenters), function(cl){
#     cl_ = clCenters[-cl]
#     min(abs(cl_ - clCenters[cl]))
#   })))
#   return(mean_min_distance)
#
#   # clCenters <- as.data.frame(clCenters)
#   # clCenters <- setNames(clCenters, names(df))
#   # }
#   # nClusters = nlevels(as.factor(cluster_idx))
# }
#
#
# order_k = order(apply(WCSS_df[,2:3], 1, mean)) + 1
# # global_min_k = which.min() + 1
#
# plot(WCSS_df[,2],type='l'); points(WCSS_df[,2], pch=16, cex = .75)
# plot(WCSS_df[,3],type='l'); points(WCSS_df[,3], pch=16, cex = .75)
# plot(coord,type='l'); points(coord, pch=16, cex = .75)
# plot(peak_idx,type='l'); points(peak_idx, pch=16, cex = .75)
#
#
# sort(WCSS_df[,2])
#
# # Subharmonics
# subh = lapply(ks, function(k){seq(from = k, to = max(ks), by = k)})
# unlist(lapply(subh, function(h){
#   # if (order_k[1] %in% h){
#   # order_k[2] %in% h
#   if (length(h) > 1){
#     sum(h %in% order_k[1:length(h)]) == length(h)
#   } else {
#     return(FALSE)
#   }
#   # }
# }))
#
# # Compute minimal between cluster distance
# unlist(lapply(ks, function(k){mean_min_dist_centroids(vec = coord,
#                                                       cluster_idx = rep(1:k, length(coord))[1:length(coord)])})) %>% plot(pch = 16, type = 'l')
#
# # ts = peaks_df %>%
# #   dplyr::filter(variable == "X1", bifpar_idx == 2) %>%
# #   arrange(time_idx) %>%
# #   pull(X)
# # ts = peaks_df %>%
# #   dplyr::filter(variable == "X1", bifpar_idx == 5) %>%
# #   arrange(time_idx) %>%
# #   pull(X)
# # ts = peaks_df %>%
# #   dplyr::filter(variable == "X1", bifpar_idx == 6) %>%
# #   arrange(time_idx) %>%
# #   pull(X)
# # plot(ts,cex=.5,type="l")
# #
# #
# # hist(ts, prob=TRUE)            # prob=TRUE for probabilities not counts
# # lines(density(ts))             # add a density estimate with defaults
# # lines(density(ts, adjust=2), lty="dotted")   # add another "smoother" density
# #
# # h = hist(ts, probability = TRUE, breaks = seq(0, 1, length.out = 10000))
# # h$density
# # which(as.logical(h$density))
# # d = density(ts, , breaks = seq(0, 1, length.out = 100))
# # d$y
# # h$density
# # pracma::findpeaks(hist(ts,prob=TRUE))
# #
# # dist_mat <- dist(as.matrix(ts), method = 'euclidean')
# # hclust_avg <- hclust(dist_mat, method = 'average')
# # plot(hclust_avg)
# # hclust_avg$labels
# #
# # mydata.hclust <- hclust(dist_mat, method = 'average')
# # member = cutree(mydata.hclust,3)
# # table(member)
# # # member
# # # 1  2  3
# # # 18  1  3
# # # Characterizing clusters
# # aggregate(ts,list(member),mean)
# # # Group.1 Fixed_charge        RoR       Cost       Load   D.Demand      Sales    Nuclear   Fuel_Cost
# # # 1       1  -0.01313873  0.1868016 -0.2552757  0.1520422 -0.1253617 -0.2215631  0.1071944  0.06692555
# # # 2       2   2.03732429 -0.8628882  0.5782326 -1.2950193 -0.7186431 -1.5814284  0.2143888  1.69263800
# # # 3       3  -0.60027572 -0.8331800  1.3389101 -0.4805802  0.9917178  1.8565214 -0.7146294 -0.96576599
# # # aggregate(mydata[,-c(1,1)],list(member),mean)
# # # Group.1 Fixed_charge       RoR     Cost     Load D.Demand    Sales Nuclear Fuel_Cost
# # # 1       1     1.111667 11.155556 157.6667 57.65556 2.850000  8127.50    13.8 1.1399444
# # # 2       2     1.490000  8.800000 192.0000 51.20000 1.000000  3300.00    15.6 2.0440000
# # # 3       3     1.003333  8.866667 223.3333 54.83333 6.333333 15504.67     0.0 0.5656667
# # # Silhouette Plot
# # library(cluster)
# # plot(silhouette(cutree(mydata.hclust,3), dist_mat))
# #
# # ##
# # ### function to compute average silhouette for k clusters
# # library(tidyverse)  # data manipulation
# # library(cluster)    # clustering algorithms
# # library(factoextra) # clustering algorithms & visualization
# #
# # avg_sil <- function(ts, k) {
# #   ts = log(as.matrix(ts))
# #   km.res <- kmeans(ts, centers = k, nstart = 25)
# #   ss <- cluster::silhouette(km.res$cluster, dist(ts))
# #   mean(ss[, 3])
# # }
# #
# # # Compute and plot wss for k = 2 to k = 15
# # k.values <- 2:10
# #
# # # extract avg silhouette for 2-15 clusters
# # avg_sil_values <- lapply(k.values, function(k){avg_sil(ts = ts, k = k)}) %>% c()
# #
# # plot(k.values, avg_sil_values,
# #      type = "b", pch = 19, frame = FALSE,
# #      xlab = "Number of clusters K",
# #      ylab = "Average Silhouette Score")
# #
# #
# # ##
# # k = 6
# # cluster_idx = rep(1:k, length(X$X))[1:length(X$X)]
# # ss2 <- cluster::silhouette(cluster_idx, dist(as.matrix(X$X)))
# # mean(ss2[,3])
# #
# # k = 2
# # peak_idx = X$peak_idx %>% diff()
# # cluster_idx = rep(1:k, length(peak_idx))[1:length(peak_idx)]
# # ss2 <- cluster::silhouette(cluster_idx, dist(as.matrix(peak_idx)))
# # mean(ss2[,3])
# #
# # ks = 2:20
# #
# # best_period <- function(ks, X){
# #   coord = X$X
# #   peak_idx = X$peak_idx %>% diff()
# #
# #   coord_silh = unlist(lapply(ks, function(k){silhouette_coeff(vec = coord, k = k)}))
# #   peak_idx_silh = unlist(lapply(ks, function(k){silhouette_coeff(vec = peak_idx, k = k)}))
# #   cbind(ks, coord_silh, peak_idx_silh)
# # }
#
# # ##
# # # Can use acf to determine if it's periodic, not to find period itself
# # t_acf = acf(ts, lag.max = length(ts) - 10)
# # t_acf
# # pracma::findpeaks( as.vector(t_acf$acf[-1]))
# # pracma::findpeaks( as.vector(t_acf$acf[-1])) %>% diff()
# #
# # t_pacf = pacf(ts, lag.max = length(ts) - 10)
# # t_pacf
# # pracma::findpeaks( as.vector(t_pacf$acf[-1]))
# # pracma::findpeaks( as.vector(t_pacf$acf[-1])) %>% diff()
# #
# # ##
# # t = df[,c("X1")] %>% stats::ts(start = df[1,c("time")],
# #                                end = df[nrow(df),c("time")],
# #                                deltat = diff(df[,c("time")])[1],
# #                                # frequency = df[,c("time")]
# # )
# # str(t)
# # decomp = stats::decompose(t)
# # plot(decomp)
# # # t = seq(0,1,len=512)
# # # w = 2 * sin(2*pi*16*t)*exp(-(t-.25)^2/.001)
# # # w= w + sin(2*pi*64*t)*exp(-(t-.75)^2/.001)
# # # w = ts(w,deltat=1/512)
# # # plot(t,w,'l')
# # # require(Rwave)
# # # require(lattice)
# # # source("mk.cwt.R")
# # # tmp<-mk.cwt(w,noctave = floor(log2(length(w)))-1,nvoice=10)
# # # print(plot.cwt(tmp,xlab="time (units of sampling interval)"))
# #
# #
# #
# # install.packages("dplR")
# # library(dplR)
# # t1 = df[60000:nrow(df),] %>% as.data.frame()
# # wave.out <- morlet(y1 = t1$X1,
# #                    x1 = t1$time_idx,
# #                    # p2 = 8, dj = 0.1,
# #                    siglvl = 0.95)
# # # p2=6 <=> estimate out to 2^8 = 256 months dj <=> controls the frequency
# # # resolution hack the period estimate to be in years, not months
# # # wave.out$period <- wave.out$period/12
# # levs <- quantile(wave.out$Power, c(0, 0.25, 0.5, 0.75, 0.95, 1))
# # wavelet.plot(wave.out,
# #              # wavelet.levels = levs,
# #              # crn.ylim = c(22.5, 30)
# #              )
# #
# # wave.out$period
# # wave.out$Signif
# # wave.out$Signif < wave.out$siglvl
# # wave.out$Scale
# # wave.out$Power
# # wave.out$wave
# #
# # # We can also calculate the “averaged” wavelet. If we calculate the average across all times, we should get another estimate of the spectral density function.
# # wave.avg <- data.frame(power = apply(wave.out$Power, 2, mean), period = (wave.out$period))
# # plot(wave.avg$period, wave.avg$power, type = "l")
# #
# #
# # acf(df[,c("X1", "X2", "X3", "X4")], lag.max = nrow(df) - 10)
# #
# # t = df[60000:nrow(df),c("X1")]
# # plot(t, type ='p', cex = .1)
# # abline(v = seq(emdim, emdim*10, by = emdim), col = 'grey50')
# # emdim = nonlinearTseries::timeLag(
# #   t,
# #   technique = c("acf", "ami")[1],
# #   selection.method = c("first.e.decay", "first.zero", "first.minimum", "first.value")[1],
# #   # value = 1/exp(1),
# #   # lag.max = NULL,
# #   # do.plot = TRUE,
# #   # main = NULL,
# #   # ...
# # )
# #
# #
# # library(EnvCpt)
# # fit_envcpt = envcpt(peaks_df %>%
# #                       dplyr::filter(variable == "X1") %>%
# #                       arrange(time_idx) %>%
# #                       pull(X))  # Fit all models at once
# # fit_envcpt$summary  # Show log-likelihoods
# # plot(fit_envcpt)
# #
# # fit_envcpt2 = envcpt(dist_df$max_dist)
# # plot(fit_envcpt2)
# #
# #
# # ## entropy
# #
# #
# # # pracma::sample_entropy(ts, edim = 2, r = 0.00005*sd(ts), tau = 1)
# # # pracma::sample_entropy(ts, edim = 3, r = 0.00005*sd(ts), tau = 1)
# # # pracma::sample_entropy(ts, edim = 4, r = 0.00005*sd(ts), tau = 1)
# #
# # sample_entropy <- function(ts, edim = 2, r = 0.2*sd(ts), tau = 1) {
# #   stopifnot(is.numeric(ts), is.numeric(edim))
# #
# #   if (tau > 1) {
# #     s <- seq(1, length(ts), by = tau)
# #     ts <- ts[s]
# #   }
# #
# #   N <- length(ts)
# #   correl  <- numeric(2)
# #   datamat <- pracma::zeros(edim + 1, N - edim)
# #   for (i in 1:(edim+1))
# #     datamat[i, ] <- ts[i:(N - edim + i - 1)]
# #
# #   for (m in edim:(edim+1)) {
# #     count <- pracma::zeros(1, N-edim)
# #     tempmat <- datamat[1:m, ]
# #
# #     for (i in 1:(N-m-1)) {
# #       # calculate Chebyshev distance
# #       X <- abs(tempmat[, (i+1):(N-edim)] -
# #                  pracma::repmat(tempmat[, i, drop=FALSE], 1, N-edim-i))
# #       dst <- apply(X, 2, max)
# #
# #       # calculate Heaviside function
# #       d <- (dst < r)
# #
# #       count[i] <- sum(d) / (N - edim)
# #     }
# #
# #     correl[m - edim + 1] <- sum(count) / (N - edim)
# #   }
# #
# #   return(log(correl[1]/correl[2]))
# # }
# #
# #
# # # install.packages("TSEntropies")
# # TSEntropies::SampEn_C(ts, dim = 2, lag = 1, r = 0.005 * sd(ts))
# # TSEntropies::SampEn_C(ts, dim = 4, lag = 1, r = 0.05 * sd(ts))
# # TSEntropies::SampEn_C(ts, dim = 6, lag = 1, r = 0.05 * sd(ts))
# #
# # TSEntropies::SampEn_R(ts, dim = 3, lag = 1, r = 0.00005 * sd(ts))
# # TSEntropies::SampEn_R(ts, dim = 2, lag = 1, r = 0.00005 * sd(ts))
# #
# # TS = ts
# # dim = 3
# # lag = 1
# # r = 0.2*sd(ts)
# #
# # ts_acf = acf(ts, lag.max = length(ts) - 1)
# # ts_acf
# # ts_acf$acf %>% as.vector %>% pracma::findpeaks()
# # ts_acf$acf %>% as.vector %>% pracma::findpeaks() %>% diff()
# #
# #
# # tak = nonlinearTseries::buildTakens(df %>% as.data.frame() %>% dplyr::filter(bifpar_idx==2) %>% arrange(time_idx) %>% pull(X1), embedding.dim = 2, time.lag = 2)
# # plot(tak[,1],tak[,2])
# # ?pracma::findintervals()
# # pracma::findmins(ts)
# #
# # ts1=sin(seq(0, 10*pi, len=100))
# # plot(ts1,cex=.7)
# # xs <- zapsmall(ts1)
# # pracma::findintervals(0, xs)
# # #   1  10  20  30  40  50  60  70  80  90 100
# #
# # SampEn_R <- function(TS, dim = 2, lag = 1, r = 0.2*sd(TS))
# # {
# #   # TS     : time series
# #   # dim    : embedded dimension
# #   # lag    : delay time for downsampling of data
# #   # r      : tolerance (typically 0.2 * std)
# #
# #   # missing downsampling
# #
# #   N <- length(TS)
# #   result <- rep(NA, 2)
# #
# #   for(x in 1:2){
# #     m <- dim + x - 1
# #     dm <- N - m * lag + 1
# #     phi <- rep(NA,dm)
# #     mtx.data <- NULL
# #
# #     for(j in 1:m){
# #       mtx.data <- rbind(mtx.data, TS[(1 + lag * (j - 1)) : (dm + lag * (j - 1))])
# #     }
# #
# #     for(i in 1:dm){
# #       mtx.temp <- abs(mtx.data - mtx.data[,i])
# #       mtx.bool <- mtx.temp < r
# #       mtx.temp <- mtx.bool[1,]
# #       for(j in 2:m){
# #         mtx.temp <- mtx.temp + mtx.bool[j,]
# #       }
# #
# #       mtx.bool <- mtx.temp == m
# #       phi[i] <- (sum(mtx.bool) - 1)
# #     }
# #
# #     result[x] <- sum(phi)
# #   }
# #
# #   if (result[2] != 0) return(log(result[1]/result[2]))
# #   else return(NA)
# # }
# # ##
# # se = nonlinearTseries::sampleEntropy(ts, do.plot = F)
# # se.est = nonlinearTseries::estimate(se, do.plot = F,
# #                   regression.range = c(8,15))
# # cat("Sample entropy estimate: ", mean(se.est), "\n")
# #
# # ##
# # install.packages("factoextra")
# # library(factoextra)
# # kmeans(ts,3)
# # fviz_nbclust(ts, kmeans, method = "silhouette")
# # ##
# # sun_ar = spec.ar(sunspot, plot = FALSE)  # parametric estimation based on AR model
# # # The latter's order is estimated using the Yule-Walker equations, with
# # # order selected by AIC
# # sun_np = stats::spectrum(ts,
# #                          # spans = c(5, 5),
# #                          plot = TRUE)  # nonparametric
# #
# # plot(sun_np$freq, sun_np$spec, type = "l", log = "y", ylab = "spectrum", xlab = "frequency",
# #      bty = "l")
# # # lines(sun_np$freq, sun_np$spec, lty = 2)
# # # legend("topright", c("parametric", "nonparametric"), lty = 1:2, bty = "n")
# #
# #
# # ##
# # hc <- hclust(dist(USArrests))
# #
# # cutree(hc, k = 1:5) #k = 1 is trivial
# # cutree(hc, h = 250)
# #
# # ## Compare the 2 and 4 grouping:
# # g24 <- cutree(hc, k = c(2,4))
# # table(grp2 = g24[,"2"], grp4 = g24[,"4"])
# #
# # ##
# # model = GLV_model
# # model_pars = list(r = r0, C0 = C0, mu = mu)
# #
# # # bifpar_list = purrr::transpose(expand.grid(s = seq(.91, .95, by = .01))),
# # # bifpar_list = purrr::transpose(expand.grid(s = seq(.8, 1.2, by = .0001)))
# # bifpar_list = purrr::transpose(expand.grid(s = seq(.9,.9, by = .0001)))
# #
# # times = seq(0,10000*5,by=.01)
# # times = seq(0,10000,by=.01)
# #
# # method = "euler"
# # X0 = c()
# # X_names = paste0("X", 1:p)
# # seed_nr = 123
# # stopifregime = function(out){FALSE}
# # silent = FALSE
# #
# # df = bifurcation_ts(model = GLV_model, model_pars = list(r = r0, C0 = C0, mu = mu),
# #                     # bifpar_list = purrr::transpose(expand.grid(s = seq(.91, .95, by = .01))),
# #                     bifpar_list = purrr::transpose(expand.grid(s = seq(.95, .97, by = .0001))),
# #                     times = seq(0,10000,by=.01),
# #                     # method = "euler",
# #                     # bifpar_list = purrr::transpose(expand.grid(s = seq(.5,.55, by = .01))),
# #                     # bifpar_list = purrr::transpose(expand.grid(s = seq(.5, .5, by = .01))),
# #                     X_names = X_names)
# # head(df)
# # nrow(df)
# #
# # peaks_df = peaks_bifdiag(df, X_names)
# # head(peaks_df)
# # peaks_df %>% group_by(bifpar_idx) %>% dplyr::summarise(n=n(), .groups = 'drop')
# #
# # peaks_df_ = peaks_df %>% dplyr::filter(variable == "X1")
# # plot(peaks_df_$bifpar_idx, peaks_df_$X, main = "Transient bifurcation diagram", ylim = c(0, .75), cex = .5, pch = 16, col = 'black')
# #
# # peaks_df_asympt = peaks_df_ %>% arrange(time_idx) %>% slice_tail(n = 100,
# #                          by = c(bifpar_idx, minmax,variable))
# # points(peaks_df_asympt$bifpar_idx, peaks_df_asympt$X, main = "Asymptotic bifurcation diagram", ylim = c(0, .75), cex = .5, pch = 16, col = 'red')
# #
# #
# #
# # ##
# # ### load data
# # install.packages("dtwclust")
# # library(dtwclust)
# # data(uciCT)
# #
# # plot(CharTraj[[100]],  type = 'l', cex = .1, col='red')
# # lines(CharTraj[[1]], type = 'l', cex = .1)
# #
# # # distance between series of different lengths
# # sbd <- SBD(CharTraj[[1]], CharTraj[[100]], znorm = TRUE)$dist
# #
# # # cross-distance matrix for series subset (notice the two-list input)
# # sbD <- proxy::dist(CharTraj[1:10], CharTraj[1:10], method = "SBD", znorm = TRUE)
# # sbd
# # sbD
# #
# #
# #
# # # Sample data
# # data(uciCT)
# #
# # # Normalize desired subset
# # X <- zscore(CharTraj[1:5])
# #
# # # Obtain centroid series
# # C <- shape_extraction(X)
# # C
# # # Result
# # matplot(do.call(cbind, X),
# #         type = "l", col = 1:5)
# # points(C)##
# #
# #
# # hc_sbd <- tsclust(CharTraj, type = "h", k = 20L,
# #                   preproc = zscore, seed = 899,
# #                   distance = "sbd", centroid = shape_extraction,
# #                   control = hierarchical_control(method = "average"))
# # # By default, the dendrogram is plotted in hierarchical clustering
# # plot(hc_sbd)
# # # The series and the obtained prototypes can be plotted too
# # plot(hc_sbd, type = "sc")
# # # Focusing on the first cluster
# # plot(hc_sbd, type = "series", clus = 1L)
# # plot(hc_sbd, type = "centroids", clus = 1L)
# #
# # peaks_df = readRDS("test.RDS") %>% group_by(bifpar_idx, variable) %>% arrange(time_idx, .by_group=TRUE) %>% dplyr::slice_tail(n = 20)
# #
# # peaks_df_ = peaks_df %>% dplyr::filter(variable == "X1", bifpar_idx  > 1, bifpar_idx < 80) %>% group_by(bifpar_idx) %>% arrange(time_idx, .by_group = TRUE)
# #
# # ex = dplyr::group_split(peaks_df_) %>% lapply(function(x){x %>% pull(X)})
# #
# # # peaks_df_$bifpar_idx%>%unique
# # # ex = list(peaks_df %>% dplyr::filter(bifpar_idx == 20, variable == "X1") %>%
# # #             arrange(time_idx) %>% pull(X) ,
# # #           peaks_df %>% dplyr::filter(bifpar_idx == 21, variable == "X1") %>%
# # #             arrange(time_idx) %>% pull(X),
# # #           peaks_df %>% dplyr::filter(bifpar_idx == 22, variable == "X1") %>%
# # #             arrange(time_idx) %>% pull(X),
# # #           peaks_df %>% dplyr::filter(bifpar_idx == 23, variable == "X1") %>%
# # #             arrange(time_idx) %>% pull(X),
# # #           peaks_df %>% dplyr::filter(bifpar_idx == 24, variable == "X1") %>%
# # #             arrange(time_idx) %>% pull(X),
# # #           peaks_df %>% dplyr::filter(bifpar_idx == 25, variable == "X1") %>%
# # #             arrange(time_idx) %>% pull(X) %>% slice(200:2000))
# #
# # plot(ex[[1]])
# #
# # hc_sbd <- dtwclust::tsclust(ex,
# #                   type = "h",
# #                   k = 2,
# #                   # k = 20L,
# #                   preproc = zscore, seed = 899,
# #                   distance = "sbd", centroid = shape_extraction,
# #                   control = hierarchical_control(method = "average"))
# #
# # names(hc_sbd)
# # hc_sbd$merge
# # # By default, the dendrogram is plotted in hierarchical clustering
# # plot(hc_sbd)
# # plot(hc_sbd, type = 'series')
# # # The series and the obtained prototypes can be plotted too
# # plot(hc_sbd, type = "sc")
# #
# # # Focusing on the first cluster
# # plot(hc_sbd, type = "series", clus = 1L)
# # plot(hc_sbd, type = "centroids", clus = 1L)
# #
# #
# # plot(hc_sbd, type = "series", clus = 2L)
# # plot(hc_sbd, type = "centroids", clus = 2L)
# #
# #
# # pc_k <- dtwclust::tsclust(ex,
# #                             type = "h",
# #                             k = 2L:20L,
# #                             # k = 20L,
# #                             preproc = zscore, seed = 899,
# #                             distance = "sbd", centroid = shape_extraction,
# #                             control = hierarchical_control(method = "average"))
# # names(pc_k) <- paste0("k_", 2L:20L)
# # cvi_mat = sapply(pc_k, dtwclust::cvi, type = "internal")
# # cvi_mat
# #
# # cvi_mat[c("Sil"),]
# #
# # plot(pc_k$k_4, type = "series")
# # plot(pc_k$k_4, type = "sc")
# #
# # pc_k$k_4$merge
# # pc_k$k_4@cluster
# # pc_k$k_2@cluster[pc_k$k_2$order]
# # pc_k$k_2@centroids
# #
# # pc_k$k_2@cldist
# # # Crisp partitions
# #
# # # "Sil" (!): Silhouette index (Rousseeuw (1987); to be maximized).
# # #
# # # "D" (!): Dunn index (Arbelaitz et al. (2013); to be maximized).
# # #
# # # "COP" (!): COP index (Arbelaitz et al. (2013); to be minimized).
# # #
# # # "DB" (?): Davies-Bouldin index (Arbelaitz et al. (2013); to be minimized).
# # #
# # # "DBstar" (?): Modified Davies-Bouldin index (DB*) (Kim and Ramakrishna (2005); to be minimized).
# # #
# # # "CH" (~): Calinski-Harabasz index (Arbelaitz et al. (2013); to be maximized).
# # #
# # # "SF" (~): Score Function (Saitta et al. (2007); to be maximized; see notes).
# #
# #
# # require("clue")
# # pc_4 <- tsclust(ex, type = "p", k = 4L,
# #                 distance = "dtw_basic", centroid = "pam",
# #                 control = partitional_control(nrep = 5L),
# #                 seed = 95L)
# #
# # names(pc_4) <- paste0("r_", 1L:5L)
# # pc_4 <- cl_ensemble(list = pc_4)
# # cl_dissimilarity(pc_4)
# # ## Dissimilarities using minimal Euclidean membership distance:
# # ## r_1 r_2 r_3 r_4
# # ## r_2 3.464102
# # ## r_3 0.000000 3.464102
# # ## r_4 0.000000 3.464102 0.000000
# # ## r_5 0.000000 3.464102 0.000000 0.000000
# # # Confusion matrix
# # table(Medoid = cl_class_ids(cl_medoid(pc_4)),
# #       "True Classes" = rep(c(4L, 3L, 1L, 2L), each = 5L))
# #
# #
# # hclust_methods <- c("single", "complete", "average", "mcquitty")
# # hc <- tsclust(data, type = "h", k = 4L,
# #               control = hierarchical_control(method = hclust_methods,
# #                                              distmat = pc_4[[1L]]@distmat))
# # names(hc) <- hclust_methods
# # hc <- cl_ensemble(list = hc)
# # cl_dissimilarity(hc)
# #
# #
# #
# #
# # ##library(ggplot2)
# # library(firstpkg)
# # library(dplyr)
# #
# # hc <- hclust(dist(USArrests),
# #              method=c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")[4])
# # plot(hc)
# # cutree(hc, k = 1:5) #k = 1 is trivial
# # cutree(hc, h = 250)
# # cutree(hc, h = 50)
# #
# # ## Compare the 2 and 4 grouping:
# # g24 <- cutree(hc, k = c(2,4))
# # table(grp2 = g24[,"2"], grp4 = g24[,"4"])
# #
# #
# # ##
# # p <- 4
# # mu <- rep(0, p)
# # r0 = c(1, .72, 1.53, 1.27)
# # C0 = matrix(
# #   c(
# #     1,
# #     1.09,
# #     1.52,
# #     0,
# #     0,
# #     1,
# #     0.44,
# #     1.36,
# #     2.33,
# #     0,
# #     1,
# #     0.47,
# #     1.21,
# #     0.51,
# #     0.35,
# #     1
# #   ),
# #   p,
# #   p,
# #   byrow = TRUE
# # )
# # model_pars <- list(r = r0, s = .96, C0 = C0, mu = mu)
# # state      <- runif(p) %>% setNames(paste0("X", 1:p))
# # times      <- seq(0, 100, by = 0.01)
# # X_names = paste0("X", 1:p)
# #
# # # model = GLV_model
# # # model_pars = list(r = r0, C0 = C0, mu = mu)
# # #
# # # # bifpar_list = purrr::transpose(expand.grid(s = seq(.91, .95, by = .01))),
# # # # bifpar_list = purrr::transpose(expand.grid(s = seq(.8, 1.2, by = .0001)))
# # # bifpar_list = purrr::transpose(expand.grid(s = seq(.9,.9, by = .0001)))
# # #
# # # times = seq(0,10000*5,by=.01)
# # # times = seq(0,10000,by=.01)
# # #
# # # method = "euler"
# # # X0 = c()
# # # X_names = paste0("X", 1:p)
# # # seed_nr = 123
# # # stopifregime = function(out){FALSE}
# # # silent = FALSE
# #
# # df = bifurcation_ts(model = GLV_model, model_pars = list(r = r0, C0 = C0, mu = mu),
# #                     # bifpar_list = purrr::transpose(expand.grid(s = seq(.91, .95, by = .01))),
# #                     bifpar_list = purrr::transpose(expand.grid(s = seq(.9, 1, by = .001))),
# #                     times = seq(0,1000,by=.01),
# #                     # method = "euler",
# #                     # bifpar_list = purrr::transpose(expand.grid(s = seq(.5,.55, by = .01))),
# #                     # bifpar_list = purrr::transpose(expand.grid(s = seq(.5, .5, by = .01))),
# #                     X_names = X_names)
# # head(df)
# # nrow(df)
# #
# # peaks_df = peaks_bifdiag(df, X_names)
# # head(peaks_df)
# #
# # peaks_df_ = peaks_df %>% dplyr::filter(variable == "X1")
# # plot(peaks_df_$bifpar_idx, peaks_df_$X, main = "Transient bifurcation diagram", ylim = c(0, .75), cex = .5, pch = 16, col = 'black')
# #
# #
# #
# # find_period <- function(ks, coord, peak_idx){
# #   # Compute silhouette coefficient for peak coordinates and peak indices
# #   coord_silh = unlist(lapply(ks, function(k){silhouette_coeff(vec = coord, k = k)}))
# #   peak_idx_silh = unlist(lapply(ks, function(k){silhouette_coeff(vec = diff(peak_idx), k = k)}))
# #   silh = cbind(ks, coord_silh, peak_idx_silh)
# #
# #   # Return row with maximum
# #   return(as.data.frame(t(silh[ which.max(apply(silh[,2:3], 1, mean)), ])))
# # }
# #
# # silhouette_coeff <- function(vec, k){
# #   cluster_idx = rep(1:k, ceiling(length(vec)/k))[1:length(vec)]
# #   ss <- cluster::silhouette(cluster_idx, dist(as.matrix(vec)))
# #   return(mean(ss[,3]))
# # }
# # # Finding the periodicity of a limit cycle:
# # # Though we are simulating deterministic dynamics, limit cycles won't repeat exactly the due to machine errors, transients, and the speed of transition. Discretizing with a histogram usually does not result in an accurate number of peaks as one peak may fall outside of the bin by the smallest margin.
# # # Instead, we find the number of distinct peaks through simple clustering. Finding clusters in these peaks allows for the tracking of splitting clusters such as in period doubling by using a maximum distance criterion.
# #
# # X = peaks_df %>% as.data.frame() %>% dplyr::filter(bifpar_idx == 60, variable == "X1") %>% arrange(time_idx) %>% slice(-c(1:30))
# # plot(X$time_idx, X$X)
# # lines(X$time_idx, X$X, cex=.5)
# # k=5
# # cluster_idx = rep(1:k, length(X$X))[1:length(X$X)]
# # points(X$time_idx, X$X,col=cluster_idx)
# # ggplot(X %>% tibble::add_column(cluster_idx=cluster_idx)) +
# #   geom_line(aes(x = time_idx, y = X), color='grey50', linewidth=.5) +
# #   geom_point(aes(x = time_idx, y = X, col = factor(cluster_idx)),
# #              size = 2.5) +
# #   theme_bw() + viridis::scale_color_viridis(discrete=TRUE)
# # coord=X$X
# # peak_idx=X$peak_idx
# #
# # hc <- hclust(dist(as.matrix(X$X)),
# #              method=c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")[2])
# # plot(hc)
# # abline(h = .05, col = 'red')
# # # cutree(hc, h = .01)
# # unique(cutree(hc, h = .005))
# #
# # k=2
# # km.res <- kmeans(as.matrix(X$X), centers = k, nstart = 25)
# # ss <- cluster::silhouette(km.res$cluster, dist(as.matrix(X$X)))
# # ss
# #
# # k = 5
# # cluster_idx = rep(1:k, length(X$X))[1:length(X$X)]
# # ss2 <- cluster::silhouette(cluster_idx, dist(as.matrix(X$X)))
# # mean(ss2[,3])
# #
# # k = 2
# # peak_idx = X$peak_idx %>% diff()
# # cluster_idx = rep(1:k, length(peak_idx))[1:length(peak_idx)]
# # ss2 <- cluster::silhouette(cluster_idx, dist(as.matrix(peak_idx)))
# # mean(ss2[,3])
# #
# # ks = 2:20
# #
# # ##
# # library(ggplot2)
# # library(firstpkg)
# # library(dplyr)
# #
# # hc <- hclust(dist(USArrests),
# #              method=c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")[4])
# # plot(hc)
# # cutree(hc, k = 1:5) #k = 1 is trivial
# # cutree(hc, h = 250)
# # cutree(hc, h = 50)
# #
# # ## Compare the 2 and 4 grouping:
# # g24 <- cutree(hc, k = c(2,4))
# # table(grp2 = g24[,"2"], grp4 = g24[,"4"])
# #
# #
# # ##
# # p <- 4
# # mu <- rep(0, p)
# # r0 = c(1, .72, 1.53, 1.27)
# # C0 = matrix(
# #   c(
# #     1,
# #     1.09,
# #     1.52,
# #     0,
# #     0,
# #     1,
# #     0.44,
# #     1.36,
# #     2.33,
# #     0,
# #     1,
# #     0.47,
# #     1.21,
# #     0.51,
# #     0.35,
# #     1
# #   ),
# #   p,
# #   p,
# #   byrow = TRUE
# # )
# # model_pars <- list(r = r0, s = .96, C0 = C0, mu = mu)
# # state      <- runif(p) %>% setNames(paste0("X", 1:p))
# # times      <- seq(0, 100, by = 0.01)
# # X_names = paste0("X", 1:p)
# #
# # # model = GLV_model
# # # model_pars = list(r = r0, C0 = C0, mu = mu)
# # #
# # # # bifpar_list = purrr::transpose(expand.grid(s = seq(.91, .95, by = .01))),
# # # # bifpar_list = purrr::transpose(expand.grid(s = seq(.8, 1.2, by = .0001)))
# # # bifpar_list = purrr::transpose(expand.grid(s = seq(.9,.9, by = .0001)))
# # #
# # # times = seq(0,10000*5,by=.01)
# # # times = seq(0,10000,by=.01)
# # #
# # # method = "euler"
# # # X0 = c()
# # # X_names = paste0("X", 1:p)
# # # seed_nr = 123
# # # stopifregime = function(out){FALSE}
# # # silent = FALSE
# #
# # df = bifurcation_ts(model = GLV_model, model_pars = list(r = r0, C0 = C0, mu = mu),
# #                     # bifpar_list = purrr::transpose(expand.grid(s = seq(.91, .95, by = .01))),
# #                     bifpar_list = purrr::transpose(expand.grid(s = seq(.9, 1, by = .001))),
# #                     times = seq(0,1000,by=.01),
# #                     # method = "euler",
# #                     # bifpar_list = purrr::transpose(expand.grid(s = seq(.5,.55, by = .01))),
# #                     # bifpar_list = purrr::transpose(expand.grid(s = seq(.5, .5, by = .01))),
# #                     X_names = X_names)
# # head(df)
# # nrow(df)
# #
# # peaks_df = peaks_bifdiag(df, X_names)
# # head(peaks_df)
# #
# # peaks_df_ = peaks_df %>% dplyr::filter(variable == "X1")
# # plot(peaks_df_$bifpar_idx, peaks_df_$X, main = "Transient bifurcation diagram", ylim = c(0, .75), cex = .5, pch = 16, col = 'black')
# #
# # ks = 2:20
# #
# # test = peaks_df %>% group_by(variable, bifpar_idx) %>%
# #   dplyr::arrange(time_idx, .by_group=TRUE) %>%
# #   dplyr::group_modify(~ find_period(ks = ks, coord = .x$X, peak_idx = .x$peak_idx)) %>% ungroup()
# # head(test)
# # test %>% as.data.frame %>% arrange(bifpar_idx) %>% tail(n=100)
# # test %>% as.data.frame %>% filter(peak_idx_silh > .5, variable == "X1")%>% arrange(bifpar_idx) %>% tail(n=100)
# #
# # points(peaks_df_$bifpar_idx, peaks_df_$X, main = "Transient bifurcation diagram", col = test %>% dplyr::filter(variable == "X1") %>% arrange(bifpar_idx) %>% pull(ks),
# #        ylim = c(0, .75), cex = .5, pch = 16)
# #
# # ggplot(peaks_df_ %>% merge(test)) +# %>% tibble::add_column(cluster_idx=cluster_idx)) +
# #   # geom_line(aes(x = bifpar_idx, y = X), color='grey50', linewidth=.5) +
# #   geom_point(aes(x = bifpar_idx, y = X, col = factor(ks)),
# #              size = .75) +
# #   theme_bw() + viridis::scale_color_viridis(begin=.2,end=.8,discrete=TRUE)
# #
# # ggplot(test) +# %>% tibble::add_column(cluster_idx=cluster_idx)) +
# #   geom_line(aes(x = bifpar_idx, y = ks), color='grey50', linewidth=.5) +
# #   geom_point(aes(x = bifpar_idx, y = ks),
# #              size = .5) +
# #   theme_bw() + viridis::scale_color_viridis(discrete=TRUE)
# #
# #
# #
# # ## Return row with maximum
# # # find_period <- function(ks, coord, peak_idx){
# # #   # Compute silhouette coefficient for peak coordinates and peak indices
# # #   coord_silh = unlist(lapply(ks, function(k){silhouette_coeff(vec = coord, k = k)}))
# # #   peak_idx_silh = unlist(lapply(ks, function(k){silhouette_coeff(vec = diff(peak_idx), k = k)}))
# # #   silh = cbind(ks, coord_silh, peak_idx_silh)
# # #
# # #   # Return row with maximum
# # #   return(as.data.frame(silh))
# # # }
# #
# #
# # # find_period <- function(ks, coord, peak_idx){
# # #   # Compute silhouette coefficient for peak coordinates and peak indices
# # #   coord_silh = unlist(lapply(ks, function(k){silhouette_coeff(vec = coord, k = k)}))
# # #   peak_idx_silh = unlist(lapply(ks, function(k){silhouette_coeff(vec = diff(peak_idx), k = k)}))
# # #   silh = cbind(ks, coord_silh, peak_idx_silh)
# # #
# # #   # Return row with maximum
# # #   return(as.data.frame(t(silh[ which.max(apply(silh[,2:3], 1, mean)), ])))
# # # }
# #
# # # silhouette_coeff <- function(vec, k, method = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")[1]){
# # #   cluster_idx = rep(1:k, ceiling(length(vec)/k))[1:length(vec)]
# # #
# # #   # Use intracluster variation, not distance between clusters to evaluate. Neighbouring values are completely fine.
# # #   ss <- cluster::silhouette(cluster_idx, dist(as.matrix(vec),
# # #                                               method=method))
# # #   summary(ss) # Observations with a large s(i) (almost 1) are very well clustered, a small s(i) (around 0) means that the observation lies between two clusters, and observations with a negative s(i) are probably placed in the wrong cluster.
# # #
# # #   return(mean(ss[,3]))
# # # }
# #
# # # Finding the periodicity of a limit cycle:
# # # Though we are simulating deterministic dynamics, limit cycles won't repeat exactly the due to machine errors, transients, and the speed of transition. Discretizing with a histogram usually does not result in an accurate number of peaks as one peak may fall outside of the bin by the smallest margin.
# # # Instead, we find the number of distinct peaks through simple clustering. Finding clusters in these peaks allows for the tracking of splitting clusters such as in period doubling by using a maximum distance criterion.
# #
# # df_ = df %>% as.data.frame() %>% dplyr::filter(bifpar_idx == 45)
# # plot(df_$time_idx, df_$X1, type = "l", cex=.5)
# # X = peaks_df %>% as.data.frame() %>% dplyr::filter(bifpar_idx == 45, variable == "X1") %>% arrange(time_idx) %>% slice(-c(1:30))
# # coord=X$X
# # peak_idx=X$peak_idx
# #
# # points(X$time_idx, X$X, pch = 16)
# #
# # plot(X$time_idx, X$X)
# # lines(X$time_idx, X$X, cex=.5)
# #
# # k=6
# # cluster_idx = rep(1:k, length(X$X))[1:length(X$X)]
# # points(X$time_idx, X$X,col=cluster_idx,pch=16)
# # ggplot(X %>% tibble::add_column(cluster_idx=cluster_idx)) +
# #   geom_line(aes(x = time_idx, y = X), color='grey50', linewidth=.5) +
# #   geom_point(aes(x = time_idx, y = X, col = factor(cluster_idx)),
# #              size = 2.5) +
# #   theme_bw() + viridis::scale_color_viridis(discrete=TRUE)
# #
# # hc <- hclust(dist(as.matrix(X$X)),
# #              method=c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")[2])
# # plot(hc)
# # abline(h = .05, col = 'red')
# # # cutree(hc, h = .01)
# # unique(cutree(hc, h = .005))
# #
# # k=2
# # km.res <- kmeans(as.matrix(X$X), centers = k, nstart = 25)
# # ss <- cluster::silhouette(km.res$cluster, dist(as.matrix(X$X)))
# # ss
# #
# # k = 5
# # cluster_idx = rep(1:k, length(X$X))[1:length(X$X)]
# # ss2 <- cluster::silhouette(cluster_idx, dist(as.matrix(X$X)))
# # mean(ss2[,3])
# #
# # k = 2
# # peak_idx = X$peak_idx %>% diff()
# # cluster_idx = rep(1:k, length(peak_idx))[1:length(peak_idx)]
# # ss2 <- cluster::silhouette(cluster_idx, dist(as.matrix(peak_idx)))
# # mean(ss2[,3])
# #
# # ks = 2:20
# #
