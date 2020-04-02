old.packages <-
function (lib.loc = NULL, repos = getOption("repos"), contriburl = contrib.url(repos, 
    type), instPkgs = installed.packages(lib.loc = lib.loc, ...), 
    method, available = NULL, checkBuilt = FALSE, ..., type = getOption("pkgType"), noDupl = TRUE) 
{
    if (is.null(lib.loc)) 
        lib.loc <- .libPaths()
    if (!missing(instPkgs)) {
        if (!is.matrix(instPkgs) || !is.character(instPkgs[, 
            "Package"])) 
            stop("ill-formed 'instPkgs' matrix")
    }
    if (NROW(instPkgs) == 0L) 
        return(NULL)
    available <- if (is.null(available)) 
        available.packages(contriburl = contriburl, method = method, 
            ...)
    else tools:::.remove_stale_dups(available)
    update <- NULL
    currentR <- minorR <- getRversion()
    minorR[[c(1L, 3L)]] <- 0L
	if(noDupl){
		toCheck <- !duplicated(instPkgs[,"Package"])
		cat(sprintf("found %s installed packages with %s not duplicated\n",length(toCheck),sum(toCheck)))
		cat("duplicated packages are:",instPkgs[!toCheck,"Package"],"\n")
	} else toCheck <- rep(TRUE,nrow(instPkgs))
    for (k in 1L:nrow(instPkgs)) {
		if (!toCheck[k])
			next
        if (instPkgs[k, "Priority"] %in% "base") 
            next
        z <- match(instPkgs[k, "Package"], available[, "Package"])
        if (is.na(z)) 
            next
        onRepos <- available[z, ]
        if ((!checkBuilt || package_version(instPkgs[k, "Built"]) >= 
            minorR) && package_version(onRepos["Version"]) <= 
            package_version(instPkgs[k, "Version"])) 
            next
        deps <- onRepos["Depends"]
        if (!is.na(deps)) {
            Rdeps <- tools:::.split_dependencies(deps)[["R", 
                exact = TRUE]]
            if (length(Rdeps) > 1L) {
                target <- Rdeps$version
                res <- do.call(Rdeps$op, list(currentR, target))
                if (!res) 
                  next
            }
        }
        update <- rbind(update, c(instPkgs[k, c("Package", "LibPath", 
            "Version", "Built")], onRepos["Version"], onRepos["Repository"]))
    }
    if (!is.null(update)) 
        colnames(update) <- c("Package", "LibPath", "Installed", 
            "Built", "ReposVer", "Repository")
    rownames(update) <- update[, "Package"]
    update[!duplicated(update), , drop = FALSE]
}
