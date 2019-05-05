
this directory contains source code to handle macroecomics data and index historical prices and to draw graphs.

<<<<<<< HEAD
* cli_delta_vs_gspc.r : plot default with cli 6month delta vs. gspc delta.
=======
* cli_delta_vs_gspc.r : plot scatter between with cli 6month delta vs. gspc delta.
>>>>>>> b9c813e3bd79f7dbab6465d01cab715ac23ddf97
* cli_delta_vs_vix.r : plot scatter between cli 6month delta vs. vix delta.
* [cli_from_2011.r](https://00819.blogspot.com/2019/03/vix-cli-6-month-delta-and-s.html) : plot spiral of cli 6 month delta and cli reading itself from 2011.
* cli_from_2007.r : same as above but from 2007.
* cli_5mon.r : draw spiral graph with give start date and # of months param.
* cli_5mon_dalta_and_SPX_monthlyreturn.r : overlay cli delta with event v-line when SPX declines more than 5%.
* [vix_hist_spx.r](https://00819.blogspot.com/2019/03/vix-cli-6-month-delta-and-s.html) : overlay graph. vix as histgram in red when cli 6month delta is positive and in blue otherwise. spx is overlaid. no longer necessary to synchronize the end of CLI and other data.this replaces vix_cli_sp.r.
  * [vix_cli_sp.r](https://00819.blogspot.com/2019/03/vix-cli-6-month-delta-and-s.html) : overlay graph. vix as histgram in red when cli 6month delta is positive and in blue otherwise. spx is overlaid.
* [vix_cli_hist.r](https://00819.blogspot.com/2019/03/vix-vs-cli-6-month-delta.html) : draw the overlaid  histgram of vix when cli delta is positve and negative. this is also the sample to draw translucent and overlaid histgram.
* [getsymbols_all.r](http://00819.blogspot.com/2018/09/prepare-data-getsymbols-autoarima.html) : download and update all data related to GDP, PAYEMS, UNDCONSA and SPCS10RSA. other data such S&P500, OECD composite leading indicator must be updated separately.
* [nikkei_gspc_jpy.r](https://00819.blogspot.com/2018/02/calculate-nikkei225-vol3.html) : draw the graph to represent lm(nikkei ~ spx + usdjpy).
* [eps_lm.r](http://00819.blogspot.com/2019/03/new-model-cli-6-month-delta-eps-pa-uc.html) : the lattest model for spx. lm(spx ~ eps + PA + UC + CS + cli$oecd 6 month delta).
* [padelta_spx_cliidelta.r](http://00819.blogspot.com/2019/02/plot-abline-eps-gspc.html) : PAYEMS delta, S&P500, Composite Leading Indicator and CLI 6 month delta are drawin in the single graph. The last two's baseline are set at 100 and 0 respectively in the same color.
* CSV : the directory to stow CSV and other format data files.
* experiment : the directory for experimental code.
  * sp5correction_vs_cli_delta.r : count # of months with given percentage of decline for the total 4 cases when cli 5 months delta positive and negative and 1 month delta positive and negative
