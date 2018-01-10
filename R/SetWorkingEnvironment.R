#' @export
Set_Working_Environment  <- function(envfile, override=list()) {

  options(stringsAsFactors = FALSE)

  data <- yaml::yaml.load_file(envfile)

  data <- lapply(data, function(x) if (is.character(x)) gsubfn::gsubfn("\\$\\((.*?)\\)", data, x) else x)

  if(data$stndir == "User") data$stndir = data$obsdir
  if(data$stndir == "GHCN") data$stndir = data$ghcndir

  if(!file.exists(data$prjdir)) dir.create(data$prjdir, showWarnings=F,recursive=T)
  if(!file.exists(data$dbdir)) dir.create(data$dbdir, showWarnings=F,recursive=T)
  if(!file.exists(data$obsdir)) dir.create(data$obsdir, showWarnings=F,recursive=T)
  if(!file.exists(data$ghcndir)) dir.create(data$ghcndir, showWarnings=F,recursive=T)

  outList = list("prjdir"=data$prjdir,
                 "dbdir"=data$dbdir,
                 "obsdir"=data$obsdir,
                 "ghcndir"=data$ghcndir,
                 "stndir"=data$stndir,
                 "DsDir"=data$DsDir,
                 "RawGcmDir"=data$RawGcmDir,
                 "CindexDir"=data$CindexDir,
                 "SpReproDir"=data$SpReproDir,
                 "WeightDir"=data$WeightDir,
                 "syear_obs"=data$syear_obs,
                 "eyear_obs"=data$eyear_obs,
                 "syear_his"=data$syear_his,
                 "eyear_his"=data$eyear_his,
                 "syear_scn"=data$syear_scn,
                 "eyear_scn"=data$eyear_scn,
                 "stnfile"=data$stnfile,
                 "OWrite"=data$OWrite,
                 "dsnms"=data$dsnms,
				         "sel_dsnms"=data$sel_dsnms,
                 "prcp_idxnms"=data$prcp_idxnms,
				         "temp_idxnms"=data$temp_idxnms,
				         "user_idxnms"=data$user_idxnms,
				         "sel_idxnms"=data$sel_idxnms,
                 "gcmnms"=data$gcmnms,
				         "sel_gcmnms"=data$sel_gcmnms,
				         "VarNames"=data$VarNames,
                 "rcpnms"=data$rcpnms)

  # override
  for (varname in names(override)) {
    outList[[varname]] = override[[varname]]
  }
  return(outList)
}

