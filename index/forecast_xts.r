## forecast_xts.R ---------------------------------------------------------
##
## xts の単一系列（OECD CLI のような、月次・平滑化された指数など）を渡すと
## 複数の簡易予測手法で h 期先までの予測値を計算する。
##
## 想定用途:
##   OECD CLI のように「なめらかで、直近の増分が減衰/加速するトレンドを持つ」
##   月次指標を、xts 形式のまま突っ込んで先の値を素早く見積もりたいとき。
##
## 依存パッケージ: xts, zoo, forecast (すべて apt の r-cran-* で入る)
##   sudo apt-get install r-cran-xts r-cran-zoo r-cran-forecast
##
## --------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(forecast)
})

#' xts系列を複数手法で予測する
#'
#' @param x xts オブジェクト。1列のみを想定（複数列なら1列目を使う）。
#' @param h 予測したい先の期数（月次なら月数）。既定 2（今回のケース: 7月・8月分）。
#' @param freq 季節周期。月次なら12、四半期なら4。既定12。
#' @param methods 使う手法。c("damped_ratio","ets","arima","drift") から選択。
#' @param level 予測区間の信頼水準（ets/arimaのみに適用）。既定 80。
#' @param plot TRUE なら実績+予測をプロットする。
#'
#' @return list(table = data.frame, models = list(...))
#'         table: 手法ごとの予測値を横に並べた data.frame（行 = 予測期）
forecast_xts <- function(x,
                          h = 2,
                          freq = 12,
                          methods = c("damped_ratio", "ets", "arima", "drift"),
                          level = 80,
                          plot = TRUE) {

  stopifnot(inherits(x, "xts"))
  if (NCOL(x) > 1) {
    message("複数列の xts が渡されたため、1列目 (", colnames(x)[1], ") のみ使用します。")
    x <- x[, 1]
  }

  x <- na.omit(x)
  n <- nrow(x)
  if (n < 3) stop("観測点が少なすぎます（最低3点は必要）。")

  dates      <- zoo::index(x)
  values     <- as.numeric(x)
  last_date  <- dates[n]
  future_dates <- seq_dates(last_date, h, freq)

  results <- list()

  ## ---- 1) damped_ratio: 前期比増分の減衰比から外挿 ------------------------
  ## OECD CLI のような「なめらかに減速/加速していく」系列向け。
  ## 直近の増分 d_t = x_t - x_{t-1} の比 d_t/d_{t-1} の幾何平均を減衰比 r とし、
  ## 最後の増分に r を掛け続けて先を伸ばす。
  if ("damped_ratio" %in% methods) {
    results$damped_ratio <- forecast_damped_ratio(values, h)
  }

  ## ---- 2) ets: 指数平滑法（減衰トレンド許可）------------------------------
  if ("ets" %in% methods) {
    if (n >= 2 * freq) {
      ts_x <- ts(values, frequency = freq)
    } else {
      ts_x <- ts(values, frequency = 1)  # 季節性を推定するには短すぎる場合は無視
    }
    fit_ets <- forecast::ets(ts_x, damped = TRUE)
    fc_ets  <- forecast::forecast(fit_ets, h = h, level = level)
    results$ets <- list(
      point = as.numeric(fc_ets$mean),
      lower = as.numeric(fc_ets$lower),
      upper = as.numeric(fc_ets$upper),
      model = fit_ets
    )
  }

  ## ---- 3) auto.arima ------------------------------------------------------
  if ("arima" %in% methods) {
    if (n >= 2 * freq) {
      ts_x <- ts(values, frequency = freq)
    } else {
      ts_x <- ts(values, frequency = 1)
    }
    fit_arima <- forecast::auto.arima(ts_x)
    fc_arima  <- forecast::forecast(fit_arima, h = h, level = level)
    results$arima <- list(
      point = as.numeric(fc_arima$mean),
      lower = as.numeric(fc_arima$lower),
      upper = as.numeric(fc_arima$upper),
      model = fit_arima
    )
  }

  ## ---- 4) drift: 直近 k 点の平均増分をそのまま伸ばす（ナイーブ・ベースライン）
  if ("drift" %in% methods) {
    k <- min(6, n - 1)
    recent_diffs <- diff(tail(values, k + 1))
    avg_diff <- mean(recent_diffs)
    results$drift <- list(
      point = tail(values, 1) + avg_diff * seq_len(h)
    )
  }

  ## ---- 結果を1つの data.frame にまとめる -----------------------------------
  tbl <- data.frame(date = future_dates)
  for (nm in names(results)) {
    tbl[[nm]] <- round(results[[nm]]$point, 4)
  }

  if (plot) {
    plot_forecast(dates, values, future_dates, results)
  }

  invisible(list(table = tbl, models = results))
}

#' 増分の減衰比から外挿する内部関数
forecast_damped_ratio <- function(values, h) {
  n <- length(values)
  d <- diff(values)                       # 前期比増分
  m <- length(d)
  if (m < 2) {
    # 増分が1つしかない場合は単純にそれを繰り返す
    r <- 1
  } else {
    ratios <- d[-1] / d[-m]
    ratios <- ratios[is.finite(ratios) & ratios > 0]  # 符号反転・ゼロ割りは除外
    if (length(ratios) == 0) {
      r <- 1
    } else {
      r <- exp(mean(log(ratios)))         # 幾何平均（外れ値に頑健）
      r <- min(max(r, 0), 1.5)            # 発散を防ぐため常識的な範囲にクリップ
    }
  }
  last_diff <- d[m]
  last_val  <- values[n]
  point <- numeric(h)
  cur_diff <- last_diff
  cur_val  <- last_val
  for (i in seq_len(h)) {
    cur_diff <- cur_diff * r
    cur_val  <- cur_val + cur_diff
    point[i] <- cur_val
  }
  list(point = point, ratio = r)
}

#' 日付シーケンスを生成（freqに応じて月次/四半期/年次を推定）
seq_dates <- function(last_date, h, freq) {
  by <- switch(as.character(freq),
               "12" = "month",
               "4"  = "quarter",
               "1"  = "year",
               "month")
  seq(as.Date(last_date), by = by, length.out = h + 1)[-1]
}

#' 実績+予測を1枚にプロット（base graphics のみ、追加パッケージ不要）
plot_forecast <- function(dates, values, future_dates, results) {
  all_dates <- c(as.Date(dates), as.Date(future_dates))
  all_vals  <- c(values, rep(NA, length(future_dates)))

  ylim_vals <- c(values)
  for (nm in names(results)) ylim_vals <- c(ylim_vals, results[[nm]]$point)
  ylim <- range(ylim_vals, na.rm = TRUE)
  ylim <- ylim + c(-1, 1) * diff(ylim) * 0.1

  plot(as.Date(dates), values, type = "l", lwd = 2, col = "black",
       xlim = range(all_dates), ylim = ylim,
       xlab = "", ylab = "value", main = "Actual vs. Forecast")

  cols <- c(damped_ratio = "steelblue", ets = "darkorange",
            arima = "forestgreen", drift = "grey50")
  pchs <- c(damped_ratio = 16, ets = 17, arima = 15, drift = 3)

  for (nm in names(results)) {
    fc_dates <- c(tail(as.Date(dates), 1), as.Date(future_dates))
    fc_vals  <- c(tail(values, 1), results[[nm]]$point)
    lines(fc_dates, fc_vals, col = cols[[nm]], lwd = 2, lty = 2)
    points(as.Date(future_dates), results[[nm]]$point,
           col = cols[[nm]], pch = pchs[[nm]])
  }
  legend("topleft", legend = names(results), col = unlist(cols[names(results)]),
         lty = 2, pch = unlist(pchs[names(results)]), bty = "n", cex = 0.8)
}

# source("forecast_xts.R")

## 系列の長さに応じて使う手法を自動選択して実行するラッパー
run_safe <- function(name, x, h = 2, freq = 12) {
  n <- nrow(na.omit(x))
  methods <- c("damped_ratio", "drift")          # 3点あれば動く手法
  if (n >= 2 * freq) methods <- c(methods, "ets", "arima")  # 季節性を推定できる長さなら追加

  cat("\n===", name, "( n =", n, "obs ) ===\n")
  res <- tryCatch(
    forecast_xts(x, h = h, freq = freq, methods = methods, plot = FALSE),
    error = function(e) { message("  -> 予測できませんでした: ", conditionMessage(e)); NULL }
  )
  if (!is.null(res)) print(res$table)
  res
}

res_usa <- run_safe("USA", cli_usa)
res_g7  <- run_safe("G7",  cli_g7)
res_g20 <- run_safe("G20", cli_g20)