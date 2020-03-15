prepare_cluster <- function(cluster, ncpus, cluster_global_objects,
                            cluster_libraries, cluster_seed, df, pf) {
  if (!is.null(cluster) && ncpus > 1) {
    warning("cluster provided. Ignore argument ncpus.")
    return(cluster)
  }

  if (is.null(cluster) && ncpus > 1) {
    RNGkind(kind = "L'Ecuyer-CMRG")
    cluster <- parallel::makeCluster(rep("localhost", ncpus), type = "PSOCK")
  }

  if (!is.null(cluster)) {
    upload_objects_to_cluster(cluster, cluster_global_objects)
    load_libs_on_cluster(cluster, cluster_libraries)
    parallel::clusterExport(cl = cluster, varlist = c("df", "pf"))
    parallel::clusterSetRNGStream(cluster, iseed = cluster_seed)
  }
  cluster
}

upload_objects_to_cluster <- function(cluster, cluster_global_objects) {
  if (!is.null(cluster_global_objects)) {
    parallel::clusterExport(cl = cluster, varlist = cluster_global_objects)
  }
}


load_libs_on_cluster <- function(cluster, cluster_libraries) {
  if (!is.null(cluster_libraries)) {
    for (L in cluster_libraries) {
      lib_call <- call("library", L)
      parallel::clusterCall(cl = cluster, fun = function() eval(lib_call))
    }
  }
}
