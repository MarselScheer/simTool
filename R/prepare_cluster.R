prepare_cluster <- function(cluster, ncpus, clusterGlobalObjects, clusterLibraries, clusterSeed, df, pf) {
  if (!is.null(cluster) && ncpus > 1)
  {
    warning("cluster provided. Ignore argument ncpus.")
    return(cluster)
  }
  
  if (is.null(cluster) && ncpus > 1){            
    RNGkind("L'Ecuyer-CMRG")
    cluster = parallel::makeCluster(rep("localhost", ncpus), type="PSOCK")  
  }
  
  if (!is.null(cluster)){
    upload_objects_to_cluster(cluster, clusterGlobalObjects)
    load_libs_on_cluster(cluster, clusterLibraries)
    parallel::clusterExport(cl=cluster, varlist=c("df", "pf"))
    parallel::clusterSetRNGStream(cluster, iseed=clusterSeed)
  }
  cluster
}

upload_objects_to_cluster <- function(cluster, clusterGlobalObjects)
{
  if (!is.null(clusterGlobalObjects)){  
    # cannot test is automatically because during automated test
    parallel::clusterExport(cl=cluster, varlist=clusterGlobalObjects)
  }
}


load_libs_on_cluster <- function(cluster, clusterLibraries) {
  if (!is.null(clusterLibraries)){      
    for (L in clusterLibraries) {
      libCall = call("library", L)
      parallel::clusterCall(cl=cluster, fun=function() eval(libCall))
    }
  }
}
