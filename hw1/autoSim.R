# autoSim.R

#for these values
nVals = seq(100, 500, by=100)
distTypes = c("gaussian", "t1", "t5")

for (n in nVals) {
  for (dist in distTypes){
    oFile = paste("n", n, dist, ".txt", sep="")
    dist_arg = paste("dist=", "'", dist, "'", sep = "")
    arg = paste("n=", n, sep="", " \"", dist_arg, "\"", " seed=", 280, " rep=", 50)
    sysCall = paste("nohup Rscript runSim.R ", arg, " > ", oFile)
    system(sysCall)
    print(paste("sysCall=", sysCall, sep=""))
  }
}