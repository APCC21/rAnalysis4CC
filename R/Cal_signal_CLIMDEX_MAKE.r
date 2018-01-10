#' @export
CLIMDEX.MAKE <- function(idxnms,ci,First_Yr,End_Yr,...){
  #Fidx <- c("su","id","txn","txx","tx10p","tx90p","wsdi","fd","tr","tnn","tnx","tn10p","tn90p","csdi","dtr","gsl",
  #          "prcptot","cdd","cwd","r95ptot","r99ptot","rx1day","rx5day","sdii","rnnmm","r10mm","r20mm")
  #TMAX
  for(i in 1:length(idxnms)){
    idxnm <- idxnms[i]
  if('su'%in%idxnm){su_idx=matrix(climdex.su(ci),nrow=1,ncol=length(climdex.su(ci)))} else {su_idx <- -999}
  if('id'%in%idxnm){id_idx=matrix(climdex.id(ci),nrow=1,ncol=length(climdex.id(ci)))} else {id_idx <- -999}
  if('txn'%in%idxnm){TXn_idx_month=climdex.txn(ci)
  TXn_idx=matrix(ncol=length(TXn_idx_month)/12)
  for (I_Year in 1:length(TXn_idx)) {
    start<-(I_Year-1)*12+1
    end<-(I_Year-1)*12+12
    TXn_idx[I_Year]=min(TXn_idx_month[start:end])
  }} else {TXn_idx <- -999}
  if('txx'%in%idxnm){TXx_idx_month=climdex.txx(ci)
  TXx_idx=matrix(ncol=length(TXx_idx_month)/12)
  for (I_Year in 1:length(TXx_idx)) {
    start<-(I_Year-1)*12+1
    end<-(I_Year-1)*12+12
    TXx_idx[I_Year]=max(TXx_idx_month[start:end])
  }} else {TXx_idx <- -999}
  if('tx10p'%in%idxnm){TX10p_idx_month=climdex.tx10p(ci)
  TX10p_idx=matrix(ncol=length(TX10p_idx_month)/12)
  for (I_Year in 1:length(TX10p_idx)) {
    start<-(I_Year-1)*12+1
    end<-(I_Year-1)*12+12
    TX10p_idx[I_Year]=mean(TX10p_idx_month[start:end])
  }} else {TX10p_idx <- -999}
  if('tx90p'%in%idxnm){TX90p_idx_month=climdex.tx90p(ci)
  TX90p_idx=matrix(ncol=length(TX90p_idx_month)/12)
  for (I_Year in 1:length(TX90p_idx)) {
    start<-(I_Year-1)*12+1
    end<-(I_Year-1)*12+12
    TX90p_idx[I_Year]=mean(TX90p_idx_month[start:end])
  }} else {TX90p_idx <- -999}
  if('wsdi'%in%idxnm){wsdi_idx=matrix(climdex.wsdi(ci),nrow=1,ncol=length(climdex.wsdi(ci)))} else {wsdi_idx <- -999}

  #TMIN
  if('fd'%in%idxnm){fd_idx=matrix(climdex.fd(ci),nrow=1,ncol=length(climdex.fd(ci)))} else {fd_idx <- -999}
  if('tr'%in%idxnm){tr_idx=matrix(climdex.tr(ci),nrow=1,ncol=length(climdex.tr(ci)))} else {tr_idx <- -999}
  if('tnn'%in%idxnm){TNn_idx_month=climdex.tnn(ci)
  TNn_idx=matrix(ncol=length(TNn_idx_month)/12)
  for (I_Year in 1:length(TNn_idx)) {
    start<-(I_Year-1)*12+1
    end<-(I_Year-1)*12+12
    TNn_idx[I_Year]=min(TNn_idx_month[start:end])
  }} else {TNn_idx <- -999}
  if('tnx'%in%idxnm){TNx_idx_month=climdex.tnx(ci)
  TNx_idx=matrix(ncol=length(TNx_idx_month)/12)
  for (I_Year in 1:length(TNx_idx)) {
    start<-(I_Year-1)*12+1
    end<-(I_Year-1)*12+12
    TNx_idx[I_Year]=max(TNx_idx_month[start:end])
  }} else {TNx_idx <- -999}
  if('tn10p'%in%idxnm){TN10p_idx_month=climdex.tn10p(ci)
  TN10p_idx=matrix(ncol=length(TN10p_idx_month)/12)
  for (I_Year in 1:length(TN10p_idx)) {
    start<-(I_Year-1)*12+1
    end<-(I_Year-1)*12+12
    TN10p_idx[I_Year]=mean(TN10p_idx_month[start:end])
  }} else {TN10p_idx <- -999}
  if('tn90p'%in%idxnm){TN90p_idx_month=climdex.tn90p(ci)
  TN90p_idx=matrix(ncol=length(TN90p_idx_month)/12)
  for (I_Year in 1:length(TN90p_idx)) {
    start<-(I_Year-1)*12+1
    end<-(I_Year-1)*12+12
    TN90p_idx[I_Year]=mean(TN90p_idx_month[start:end])
  }} else {TN90p_idx <- -999}
  if('csdi'%in%idxnm){csdi_idx=matrix(climdex.csdi(ci),nrow=,ncol=length(climdex.csdi(ci)))} else {csdi_idx <- -999}

  #TMAX&TMIN
  if('dtr'%in%idxnm){dtr_idx_month=climdex.dtr(ci)
  dtr_idx=matrix(ncol=length(dtr_idx_month)/12)
  for (I_Year in 1:length(dtr_idx)) {
    start<-(I_Year-1)*12+1
    end<-(I_Year-1)*12+12
    dtr_idx[I_Year]=mean(dtr_idx_month[start:end])
  }} else {dtr_idx <- -999}
  if('gsl'%in%idxnm){gsl_idx=matrix(climdex.gsl(ci),nrow=1,ncol=length(climdex.gsl(ci)))} else {gsl_idx <- -999}

  #PRCP
  if('prcptot'%in%idxnm){prcptot_idx=matrix(climdex.prcptot(ci),nrow=1,ncol=length(climdex.prcptot(ci)))} else {prcptot_idx <- -999}
  if('cdd'%in%idxnm){cdd_idx=matrix(climdex.cdd(ci),nrow=1,ncol=length(climdex.cdd(ci)))} else {cdd_idx <- -999}
  if('cwd'%in%idxnm){cwd_idx=matrix(climdex.cwd(ci),nrow=1,ncol=length(climdex.cwd(ci)))} else {cwd_idx <- -999}
  if('r95ptot'%in%idxnm){r95ptot_idx=matrix(climdex.r95ptot(ci),nrow=1,ncol=length(climdex.r95ptot(ci)))} else {r95ptot_idx <- -999}
  if('r99ptot'%in%idxnm){r99ptot_idx=matrix(climdex.r99ptot(ci),nrow=1,ncol=length(climdex.r99ptot(ci)))} else {r99ptot_idx <- -999}
  if('rx1day'%in%idxnm){rx1day_idx_month=climdex.rx1day(ci)
  rx1day_idx=matrix(ncol=length(rx1day_idx_month)/12)
  for (I_Year in 1:length(rx1day_idx)) {
    start<-(I_Year-1)*12+1
    end<-(I_Year-1)*12+12
    rx1day_idx[I_Year]<-max(rx1day_idx_month[start:end])
  }} else {rx1day_idx <- -999}
  if('rx5day'%in%idxnm){rx5day_idx_month=climdex.rx5day(ci)
  rx5day_idx=matrix(ncol=length(rx5day_idx_month)/12)
  for (I_Year in 1:ncol(rx5day_idx)) {
    start<-(I_Year-1)*12+1
    end<-(I_Year-1)*12+12
    rx5day_idx[I_Year]<-max(rx5day_idx_month[start:end])
  }} else {rx5day_idx <- -999}
  if('sdii'%in%idxnm){sdii_idx=matrix(climdex.sdii(ci),nrow=1,ncol=length(climdex.sdii(ci)))} else {sdii_idx <- -999}

  #add
  if('rnnmm'%in%idxnm){rnnmm_idx=matrix(climdex.rnnmm(ci),nrow=1,ncol=length(climdex.rnnmm(ci)))} else {rnnmm_idx <- -999}
  if('r10mm'%in%idxnm){r10mm_idx=matrix(climdex.r10mm(ci),nrow=1,ncol=length(climdex.r10mm(ci)))} else {r10mm_idx <- -999}
  if('r20mm'%in%idxnm){r20mm_idx=matrix(climdex.r20mm(ci),nrow=1,ncol=length(climdex.r20mm(ci)))} else {r20mm_idx <- -999}
  Aidx <- rbind(su_idx,id_idx,TXn_idx,TXx_idx,TX10p_idx,TX90p_idx,wsdi_idx,fd_idx,tr_idx,TNn_idx,TNx_idx,TN10p_idx,
                TN90p_idx,csdi_idx,dtr_idx,gsl_idx,prcptot_idx,cdd_idx,cwd_idx,r95ptot_idx,r99ptot_idx,rx1day_idx,rx5day_idx,
                sdii_idx,rnnmm_idx,r10mm_idx,r20mm_idx)
  Aidx <- Aidx[-which(Aidx==-999)]
  if(i==1){
    out <- Aidx
  } else {
    out <- rbind(out,Aidx)
  }
  }
  rownames(out) <- idxnms
  year <- seq(First_Yr,End_Yr,1)
  Out <- rbind(year,out[which(apply(out,1,mean,na.rm=TRUE)!=-999),])
  return(Out)

}
