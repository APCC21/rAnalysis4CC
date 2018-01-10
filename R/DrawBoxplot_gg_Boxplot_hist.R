#' @export
gg.boxplot.hist <- function(X,fname,Rep,idxnm){
  minx <- trunc(min(X[,"value"]))
  maxx <- round(max(X[,"value"]),0)
  byx <- round((maxx - minx)/6,0)
  maxx <- maxx + byx
  X$Method <- X$dsnms
  p = ggplot(data=X, aes(x=factor(dsnms, levels=unique(dsnms)),y=value,fill=Method)) +
    facet_grid(. ~ Cate_f, scales = "free", space="free_x") +
    geom_boxplot() +
    scale_x_discrete(name = "Downscaling Method") +
    geom_hline(yintercept = median(X[1:Rep,1]),color = "red") +
    theme_bw() + theme(plot.title=element_text(size=15,hjust=0.5))+
    #ggtitle(paste("Boxplot of ",idxnm,sep=""))
    labs(title = paste("Boxplot of ",idxnm,sep=""))

  if(byx > 0){
    p = p + scale_y_continuous(name = "Value", breaks = seq(minx,maxx,byx), limits=c(minx,maxx))
  } else {
    p = p + scale_y_continuous(name = "Value", limits=c(minx,maxx))
  }

  ggsave(fname, plot=p)#, width = 16, height = 12, units = 'in', dpi = 600)
}

