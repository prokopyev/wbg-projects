library(alluvial)
tit <- as.data.frame(Titanic)

# only two variables: class and survival status
tit2d <- aggregate( Freq ~ Class + Survived, data=tit, sum)

alluvial( tit2d[,1:2], freq=tit2d$Freq, xw=0.0, alpha=0.8,
          gap.width=0.1, col= "steelblue", border="white",
          layer = tit2d$Survived != "Yes" )

library(alluvial)
alluvial( joined_data_freq[,1:4], freq=joined_data_freq$freq, border=NA,
          hide = joined_data_freq$freq < quantile(joined_data_freq$freq, .50),
          col=ifelse( joined_data_freq$fcs_indicator == "Y", "red", "gray") )
