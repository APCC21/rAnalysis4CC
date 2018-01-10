#' @export
gg.boxplot <- function(X,fname,idxnm){
  p = ggplot() + facet_grid(RCP ~ Future, scales = "free", space="free_x") +
    geom_boxplot(data=X, aes(x=factor(Downscaling, levels=unique(Downscaling)),y=Error)) +
    theme(axis.text.x=element_text(size=rel(1.0),angle = 90, vjust = 0, hjust=1, colour="black"),
          axis.text.y=element_text(size=rel(1.0),colour="black"),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title=element_text(size=15,hjust=0.5)) +
    geom_hline(yintercept=0,color="red")+
    ggtitle(paste("Future scenario of ",idxnm,sep=""))
  ggsave(filename=fname, plot=p)#, width = 16, height = 12, units = 'in', dpi = 600)
}
