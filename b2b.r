# setb2bkikan like "2013-01-01::2016-12-31"
# print("19")
# when the xts has columsn below
#
# > names(b2b_mon_xts)
#  [1] "VSMB.Q"                     "Enterprise.Q"               "B2B.Total.Q"
#  [4] "VSMB.Q.New"                 "VSMB.Q.Renew"               "Enterprise.Q.New"
#  [7] "Enterprise.Q.Renew"         "VSMB.Gross.Bkg"             "Enterprise.Gross.Bkg"
# [10] "B2B.Gross.BkgTotal"         "VSMB.Gross.Bkg.New"         "VSMB.Gross.Bkg.Renew"
# [13] "Enterprise.Gross.Bkg.New"   "Enterprise.Gross.Bkg.Renew" "VSMB.Net.Bkg"
# [16] "Enterprise.Net.Bkg"         "B2B.Net.Bkg.Total"          "New"
# [19] "Actual"
#
# to test every combination from 1x1x1  to 17x17x17 to estimate 18 or 19.

  rsq <- 0  # initialize work are to store result.
  for(k in seq(1,17,1)){
    for(i in seq(1,17,1)){
      for(j in seq(1,17,1)){
        for(h in seq(19,18,-1)){  # try both new and actual as Y.
          w <- c(); # w <- append(w,k); w <- append(w,i); w <- append(w,j); w <- append(w,h);
          w <- append(w,c(k,i,j,h));
          # print(w)
          r <- c();
          r <- summary(lm(apply.quarterly(b2b_mon_xts[b2bkikan][,h],sum) ~ apply.quarterly(b2b_mon_xts[b2bkikan][,i],sum) + apply.quarterly(b2b_mon_xts[b2bkikan][,j],sum) + apply.quarterly(b2b_mon_xts[b2bkikan][,k],sum)))$r.squared
          if(h ==18){
            cat("#")
          }else{
            cat(".")
          }
          if(r > rsq){ # if result is bigger than the current biggest, replace.
            cat("\n")
            print(w)
            print(r)
            rsq <- r # to renew the highest so far.
          }
        }
      }
    }
  }
