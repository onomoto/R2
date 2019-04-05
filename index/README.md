
this directory contains source code to handle macroecomics data and index historical prices and to draw graphs.

* cli_delta_vs_gspc.r : plot default with cli 6month delta vs. gspc delta. they show very strong correlation each other.
* cli_from_2011.r : plot spiral of cli 6 month delta and cli reading itself. 
* vix_cli_sp.r : overlay graph. vix as histgram in red when cli 6month delta is positive and in blue otherwise. spx close is overlaid.
* vix_cli_hist.r : draw the overlaid  histgram of vix when cli delta is positve and negative. this is also the sample to draw translucent and overlaid histgram.
* getsymbols_all.r : download and update all data related to GDP, PAYEMS, UNDCONSA ans SPCS10RSA, other data such S&P500, OECD composite leading indicator must be updated separately.
* [nikkei_gspc_jpy.r](https://00819.blogspot.com/2018/02/calculate-nikkei225-vol3.html) : draw the graph to represent lm(nikkei ~ spx + usdjpy).
* [eps_lm.r](http://00819.blogspot.com/2019/03/new-model-cli-6-month-delta-eps-pa-uc.html) : the lattest model for spx. lm(spx ~ eps + PA + UC + CS + cli$oecd 6 month delta).
* CSV : the directory to stow CSV and other format data files.

