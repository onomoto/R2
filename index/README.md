
this directory contains source code to handle macroecomics data and index historical prices.
* Data and Download
  * cli_load.r : load cli data from preprocessed CSV files in ~/Downloads.
  * cli_download.sed : sed script to preprocess csv from https://data.oecd.org/leadind/composite-leading-indicator-cli.htm.
  * [getsymbols_all.r](http://00819.blogspot.com/2018/09/prepare-data-getsymbols-autoarima.html) : download and update all data related to GDP, PAYEMS, UNDCONSA and SPCS10RSA. other data such S&P500, OECD composite leading indicator must be updated separately.
  * getsp5.r : download updated GSPC and combined with CSV from 1950 to 2006 to create daily S&P 500 data from 1950 to the present.
* cli_delta_vs_gspc.r : plot scatter between with cli 6month delta vs. gspc delta.
* cli_delta_vs_vix.r : plot scatter between cli 6month delta vs. vix delta.
* cli_delta_vs_spx_monthreturn.r : CLI delta or VIX histgram vs. SPX price change eventline. able to swith CLI delta or VIX by parameter.
* [cli_5mon.r](https://00819.blogspot.com/2019/05/cli5monr-draw-spiral-graph-of-cli-5.html) : draw spiral graph with given start date and # of years param. the latter is automatically adjusted with the end of data.
  * [cli_from_2011.r](https://00819.blogspot.com/2019/03/vix-cli-6-month-delta-and-s.html) : plot spiral of cli 6 month delta and cli reading itself from 2011.
  * cli_from_2007.r : same as above but from 2007.
* [cli_5mon_dalta_and_SPX_monthlyreturn.r](https://00819.blogspot.com/2019/05/cli-5-month-delta-vs-spx-decline.html) : overlay cli delta with event v-line when SPX declines more than the given parameter.
* [vix_hist_spx.r](https://00819.blogspot.com/2019/03/vix-cli-6-month-delta-and-s.html) : overlay graph. vix as histgram in red when cli 6month delta is positive and in blue otherwise. spx is overlaid. no longer necessary to synchronize the end of CLI and other data.this replaces vix_cli_sp.r.
  * [vix_cli_sp.r](https://00819.blogspot.com/2019/03/vix-cli-6-month-delta-and-s.html) : overlay graph. vix as histgram in red when cli 6month delta is positive and in blue otherwise. spx is overlaid.
* [vix_cli_hist.r](https://00819.blogspot.com/2019/03/vix-vs-cli-6-month-delta.html) : draw the overlaid  histgram of vix when cli delta is positve and negative. this is also the sample to draw translucent and overlaid histgram.
* [nikkei_gspc_jpy.r](https://00819.blogspot.com/2018/02/calculate-nikkei225-vol3.html) : draw the graph to represent lm(nikkei ~ spx + usdjpy).
* [eps_lm.r](http://00819.blogspot.com/2019/03/new-model-cli-6-month-delta-eps-pa-uc.html) : the lattest model for spx. lm(spx ~ eps + PA + UC + CS + cli$oecd 6 month delta).
* [padelta_spx_cliidelta.r](http://00819.blogspot.com/2019/02/plot-abline-eps-gspc.html) : PAYEMS delta, S&P500, Composite Leading Indicator and CLI 6 month delta are drawin in the single graph. The last two's baseline are set at 100 and 0 respectively in the same color.
* Last.r : create time stamped .Rdata back up at the exit. simply type ".Last" also creates same back up without exiting the session
* CSV : the directory to stow CSV and other format data files.
* experiment : the directory for experimental code.
  * [sp5correction_vs_cli_delta.r](https://00819.blogspot.com/2019/05/cli-5-month-delta-and-1-month-delta-vs.html) : count # of months with given percentage of decline for the total 4 cases when cli 5 months delta positive and negative and 1 month delta positive and negative
  * [find_correction.r](https://pbs.twimg.com/media/D7OJEIEVUAYnOzO.jpg) : go through the entire daily price data of S&P 500 from 1950-01-01 and find price correction.
  * find_seq.r : go through the entire daily price data of S&P 500 from 1950-01-01 and find the days when price goes up in consequetive longer than its designated limit of days.
  * [cli_delta_vs_period_return_rate.r](https://00819.blogspot.com/2019/06/s-500-performance-comparison-between.html) :
    1. indentify the period start and ends when cli delta is in either "plus" or "minus".
    1. calculate index updown ratio and daily avarage exponential rate during period.
    1. draw the overlaid [histogram](https://00819.blogspot.com/2019/06/histogram-performance-comparison.html) for the compaison.
