find.package <-
function (package = NULL, lib.loc = NULL, quiet = FALSE, verbose = getOption("verbose"), latest = TRUE, showVersion = FALSE) 
{
    if (is.null(package) && is.null(lib.loc) && !verbose) {
        return(path.package())
    }
    if (length(package) == 1L && package %in% c("base", "tools", 
        "utils", "grDevices", "graphics", "stats", "datasets", 
        "methods", "grid", "parallel", "splines", "stats4", "tcltk", 
        "compiler")) 
        return(file.path(.Library, package))
    if (is.null(package)) 
        package <- .packages()
    if (!length(package)) 
        return(character())
    if (use_loaded <- is.null(lib.loc)) 
        lib.loc <- .libPaths()
    bad <- character()
    out <- character()
	if(showVersion) versions <- matrix("",0,5,dimnames=list(NULL,c("Package","Version","Valid","Order","Lib")))
    for (pkg in package) {
        paths <- file.path(lib.loc, pkg)
        paths <- paths[file.exists(file.path(paths, "DESCRIPTION"))]
        if (use_loaded && isNamespaceLoaded(pkg)) {
            dir <- if (pkg == "base") 
                system.file()
            else .getNamespaceInfo(asNamespace(pkg), "path")
            paths <- c(dir, paths)
        }
        if (length(paths) && file.exists(file.path(paths[1], 
            "dummy_for_check"))) {
            bad <- c(bad, pkg)
            next
        }
        if (length(paths)) {
            paths <- unique(paths)
            valid_package_version_regexp <- .standard_regexps()$valid_package_version
            db <- lapply(paths, function(p) {
                pfile <- file.path(p, "Meta", "package.rds")
                info <- if (file.exists(pfile)) 
                  readRDS(pfile)$DESCRIPTION[c("Package", "Version")]
                else {
                  info <- tryCatch(read.dcf(file.path(p, "DESCRIPTION"), 
                    c("Package", "Version"))[1, ], error = identity)
                  if (inherits(info, "error") || (length(info) != 
                    2L) || anyNA(info)) 
                    c(Package = NA, Version = NA)
                  else info
                }
            })
            db <- do.call("rbind", db)
            ok <- (apply(!is.na(db), 1L, all) & (db[, "Package"] == 
                pkg) & (grepl(valid_package_version_regexp, db[, 
                "Version"])))
			if(showVersion){
			    pkgversions <- cbind(db[,"Package"],db[,"Version"],as.character(ok),Order=rep(NA,length(paths)),paths)
				pkgversions[ok,"Order"] <- seq(sum(ok))
			}
            paths <- paths[ok]
        }
        if (length(paths) == 0L) {
            bad <- c(bad, pkg)
            next
        }
        if (length(paths) > 1L) {
			if(latest){
				v <- db[ok,"Version"]
				vs <- strsplit(v,"[.-]")
                # could be nested, TBD, split on ., order with split on - within
                vsf <- lapply(lapply(vs,format,justify="right"),function(x)gsub(" ","0",x))
				vn <- sapply(vsf,paste,collapse=".")
				o <- order(vn,decreasing=T)
				if(verbose){
					warning(gettextf("package %s found more than once,\n  with versions %s,\n  using version %s from library %d of\n  %s", 
					  sQuote(pkg), paste(v, collapse = ", "), v[o[1L]], o[1L], paste(dQuote(paths), collapse = ",\n  ")), 
					  domain = NA)
				}
				if(showVersion) pkgversions[ok,"Order"] <- as.character(o)
				paths <- paths[o[1L]]
            } else {
				if (verbose) 
					warning(gettextf("package %s found more than once, using the first from\n  %s", 
					  sQuote(pkg), paste(dQuote(paths), collapse = ",\n  ")), 
					  domain = NA)
				paths <- paths[1L]
			}
        }
		if(showVersion) versions <- rbind(versions,pkgversions)
        out <- c(out, paths)
    }
    if (!quiet && length(bad)) {
        if (length(out) == 0L) 
            stop(packageNotFoundError(bad, lib.loc, sys.call()))
        for (pkg in bad) warning(gettextf("there is no package called %s", 
            sQuote(pkg)), domain = NA)
    }
	if(showVersion){ rownames(versions) <- NULL; return(versions) }
    out
}
install.packages <-
function (pkgs, lib, repos = getOption("repos"), contriburl = contrib.url(repos, 
    type), method, available = NULL, destdir = NULL, dependencies = NA, 
    type = getOption("pkgType"), configure.args = getOption("configure.args"), 
    configure.vars = getOption("configure.vars"), clean = FALSE, 
    Ncpus = getOption("Ncpus", 1L), verbose = getOption("verbose"), 
    libs_only = FALSE, INSTALL_opts, quiet = FALSE, keep_outputs = FALSE, noInstallRHOME = TRUE, saveOld = TRUE,
    ...) 
{
    type2 <- .Platform$pkgType
    if (type == "binary") {
        if (type2 == "source") 
            stop("type 'binary' is not supported on this platform")
        else type <- type2
        if (type == "both" && (!missing(contriburl) || !is.null(available))) 
            stop("specifying 'contriburl' or 'available' requires a single type, not type = \"both\"")
    }
    if (is.logical(clean) && clean) 
        clean <- "--clean"
    if (is.logical(dependencies) && is.na(dependencies)) 
        dependencies <- if (!missing(lib) && length(lib) > 1L) 
            FALSE
        else c("Depends", "Imports", "LinkingTo")
    get_package_name <- function(pkg) {
        gsub("_[.](zip|tar[.]gz|tar[.]bzip2|tar[.]xz)", "", gsub(.standard_regexps()$valid_package_version, 
            "", basename(pkg)))
    }
    getConfigureArgs <- function(pkg) {
        if (.Platform$OS.type == "windows") 
            return(character())
        if (length(pkgs) == 1L && length(configure.args) && length(names(configure.args)) == 
            0L) 
            return(paste0("--configure-args=", shQuote(paste(configure.args, 
                collapse = " "))))
        pkg <- get_package_name(pkg)
        if (length(configure.args) && length(names(configure.args)) && 
            pkg %in% names(configure.args)) 
            config <- paste0("--configure-args=", shQuote(paste(configure.args[[pkg]], 
                collapse = " ")))
        else config <- character()
        config
    }
    getConfigureVars <- function(pkg) {
        if (.Platform$OS.type == "windows") 
            return(character())
        if (length(pkgs) == 1L && length(configure.vars) && length(names(configure.vars)) == 
            0L) 
            return(paste0("--configure-vars=", shQuote(paste(configure.vars, 
                collapse = " "))))
        pkg <- get_package_name(pkg)
        if (length(configure.vars) && length(names(configure.vars)) && 
            pkg %in% names(configure.vars)) 
            config <- paste0("--configure-vars=", shQuote(paste(configure.vars[[pkg]], 
                collapse = " ")))
        else config <- character()
        config
    }
    get_install_opts <- function(pkg) {
        if (!length(INSTALL_opts)) 
            character()
        else paste(INSTALL_opts[[get_package_name(pkg)]], collapse = " ")
    }
    if (missing(pkgs)) {
        if (!interactive()) 
            stop("no packages were specified")
        if (.Platform$OS.type == "windows" || .Platform$GUI == 
            "AQUA" || (capabilities("tcltk") && capabilities("X11") && 
            suppressWarnings(tcltk::.TkUp))) {
        }
        else stop("no packages were specified")
        if (is.null(available)) {
            av <- available.packages(contriburl = contriburl, 
                method = method, ...)
            if (missing(repos)) 
                repos <- getOption("repos")
            if (type != "both") 
                available <- av
        }
        else av <- available
        if (NROW(av)) {
            pkgs <- select.list(sort(unique(rownames(av))), multiple = TRUE, 
                title = "Packages", graphics = TRUE)
        }
    }
    if (!length(pkgs)) 
        return(invisible())
    if (missing(lib) || is.null(lib)) {
        lib <- .libPaths(noRHOME=noInstallRHOME)[1L]
        if (!quiet && length(.libPaths(noRHOME=noInstallRHOME)) > 1L) 
            message(sprintf(ngettext(length(pkgs), "Installing package into %s\n(as %s is unspecified)", 
                "Installing packages into %s\n(as %s is unspecified)"), 
                sQuote(lib), sQuote("lib")), domain = NA)
    }
    ok <- dir.exists(lib) & (file.access(lib, 2) == 0L)
    if (length(lib) > 1 && any(!ok)) 
        stop(sprintf(ngettext(sum(!ok), "'lib' element %s is not a writable directory", 
            "'lib' elements %s are not writable directories"), 
            paste(sQuote(lib[!ok]), collapse = ", ")), domain = NA)
    if (length(lib) == 1L && .Platform$OS.type == "windows") {
        ok <- dir.exists(lib)
        if (ok) {
            fn <- file.path(lib, paste0("_test_dir_", Sys.getpid()))
            unlink(fn, recursive = TRUE)
            res <- try(dir.create(fn, showWarnings = FALSE))
            if (inherits(res, "try-error") || !res) 
                ok <- FALSE
            else unlink(fn, recursive = TRUE)
        }
    }
    if (length(lib) == 1L && !ok) {
        warning(gettextf("'lib = \"%s\"' is not writable", lib), 
            domain = NA, immediate. = TRUE)
        userdir <- unlist(strsplit(Sys.getenv("R_LIBS_USER"), 
            .Platform$path.sep))[1L]
        if (interactive()) {
            ans <- askYesNo(gettext("Would you like to use a personal library instead?"), 
                default = FALSE)
            if (!isTRUE(ans)) 
                stop("unable to install packages")
            lib <- userdir
            if (!file.exists(userdir)) {
                ans <- askYesNo(gettextf("Would you like to create a personal library\n%s\nto install packages into?", 
                  sQuote(userdir)), default = FALSE)
                if (!isTRUE(ans)) 
                  stop("unable to install packages")
                if (!dir.create(userdir, recursive = TRUE)) 
                  stop(gettextf("unable to create %s", sQuote(userdir)), 
                    domain = NA)
                .libPaths(c(userdir, .libPaths()))
            }
        }
        else stop("unable to install packages")
    }
    lib <- normalizePath(lib)
    if (length(pkgs) == 1L && missing(repos) && missing(contriburl)) {
        if ((type == "source" && any(grepl("[.]tar[.](gz|bz2|xz)$", 
            pkgs))) || (type %in% "win.binary" && endsWith(pkgs, 
            ".zip")) || (startsWith(type, "mac.binary") && endsWith(pkgs, 
            ".tgz"))) {
            repos <- NULL
            message("inferring 'repos = NULL' from 'pkgs'")
        }
        if (type == "both") {
            if (type2 %in% "win.binary" && endsWith(pkgs, ".zip")) {
                repos <- NULL
                type <- type2
                message("inferring 'repos = NULL' from 'pkgs'")
            }
            else if (startsWith(type2, "mac.binary") && endsWith(pkgs, 
                ".tgz")) {
                repos <- NULL
                type <- type2
                message("inferring 'repos = NULL' from 'pkgs'")
            }
            else if (grepl("[.]tar[.](gz|bz2|xz)$", pkgs)) {
                repos <- NULL
                type <- "source"
                message("inferring 'repos = NULL' from 'pkgs'")
            }
        }
    }
    if (length(pkgs) == 1L && is.null(repos) && type == "both") {
        if ((type2 %in% "win.binary" && endsWith(pkgs, ".zip")) || 
            (startsWith(type2, "mac.binary") && endsWith(pkgs, 
                ".tgz"))) {
            type <- type2
        }
        else if (grepl("[.]tar[.](gz|bz2|xz)$", pkgs)) {
            type <- "source"
        }
    }
    if (is.null(repos) && missing(contriburl)) {
        tmpd <- destdir
        nonlocalrepos <- any(web <- grepl("^(http|https|ftp)://", 
            pkgs))
        if (is.null(destdir) && nonlocalrepos) {
            tmpd <- file.path(tempdir(), "downloaded_packages")
            if (!file.exists(tmpd) && !dir.create(tmpd)) 
                stop(gettextf("unable to create temporary directory %s", 
                  sQuote(tmpd)), domain = NA)
        }
        if (nonlocalrepos) {
            df <- function(p, destfile, method, ...) download.file(p, 
                destfile, method, mode = "wb", ...)
            urls <- pkgs[web]
            for (p in unique(urls)) {
                this <- pkgs == p
                destfile <- file.path(tmpd, basename(p))
                res <- try(df(p, destfile, method, ...))
                if (!inherits(res, "try-error") && res == 0L) 
                  pkgs[this] <- destfile
                else {
                  pkgs[this] <- NA
                }
            }
        }
    }
    if (type == "both") {
        if (type2 == "source") 
            stop("type == \"both\" can only be used on Windows or a CRAN build for macOS")
        if (!missing(contriburl) || !is.null(available)) 
            type <- type2
    }
    getDeps <- TRUE
    if (type == "both") {
        if (is.null(repos)) 
            stop("type == \"both\" cannot be used with 'repos = NULL'")
        type <- "source"
        contriburl <- contrib.url(repos, "source")
        if (missing(repos)) 
            repos <- getOption("repos")
        available <- available.packages(contriburl = contriburl, 
            method = method, fields = "NeedsCompilation", ...)
        pkgs <- getDependencies(pkgs, dependencies, available, 
            lib, ...)
        getDeps <- FALSE
        av2 <- available.packages(contriburl = contrib.url(repos, 
            type2), method = method, ...)
        bins <- row.names(av2)
        bins <- pkgs[pkgs %in% bins]
        srcOnly <- pkgs[!pkgs %in% bins]
        binvers <- av2[bins, "Version"]
        hasArchs <- !is.na(av2[bins, "Archs"])
        needsCmp <- !(available[bins, "NeedsCompilation"] %in% 
            "no")
        hasSrc <- hasArchs | needsCmp
        srcvers <- available[bins, "Version"]
        later <- as.numeric_version(binvers) < srcvers
        action <- getOption("install.packages.compile.from.source", 
            "interactive")
        if (!nzchar(Sys.which(Sys.getenv("MAKE", "make")))) 
            action <- "never"
        if (any(later)) {
            msg <- ngettext(sum(later), "There is a binary version available but the source version is later", 
                "There are binary versions available but the source versions are later")
            cat("\n", paste(strwrap(msg, indent = 2, exdent = 2), 
                collapse = "\n"), ":\n", sep = "")
            out <- data.frame(binary = binvers, source = srcvers, 
                needs_compilation = hasSrc, row.names = bins, 
                check.names = FALSE)[later, ]
            print(out)
            cat("\n")
            if (any(later & hasSrc)) {
                if (action == "interactive" && interactive()) {
                  msg <- ngettext(sum(later & hasSrc), "Do you want to install from sources the package which needs compilation?", 
                    "Do you want to install from sources the packages which need compilation?")
                  res <- askYesNo(msg)
                  if (is.na(res)) 
                    stop("Cancelled by user")
                  if (!isTRUE(res)) 
                    later <- later & !hasSrc
                }
                else if (action == "never") {
                  cat("  Binaries will be installed, to install from source please provide a make program outside R, eg in Rtools\n")
                  later <- later & !hasSrc
                }
            }
        }
        bins <- bins[!later]
        if (length(srcOnly)) {
            s2 <- srcOnly[!(available[srcOnly, "NeedsCompilation"] %in% 
                "no")]
            if (length(s2)) {
                msg <- ngettext(length(s2), "Package which is only available in source form, and may need compilation of C/C++/Fortran", 
                  "Packages which are only available in source form, and may need compilation of C/C++/Fortran")
                msg <- c(paste0(msg, ": "), sQuote(s2))
                msg <- strwrap(paste(msg, collapse = " "), exdent = 2)
                message(paste(msg, collapse = "\n"), domain = NA)
                if (action == "interactive" && interactive()) {
                  res <- askYesNo("Do you want to attempt to install these from sources?")
                  if (is.na(res)) 
                    stop("Cancelled by user")
                  if (!isTRUE(res)) 
                    pkgs <- setdiff(pkgs, s2)
                }
                else if (action == "never") {
                  cat("  These will not be installed\n")
                  pkgs <- setdiff(pkgs, s2)
                }
            }
        }
        if (length(bins)) {
            if (type2 == "win.binary") 
                .install.winbinary(pkgs = bins, lib = lib, contriburl = contrib.url(repos, 
                  type2), method = method, available = av2, destdir = destdir, 
                  dependencies = NULL, libs_only = libs_only, 
                  quiet = quiet, ...)
            else .install.macbinary(pkgs = bins, lib = lib, contriburl = contrib.url(repos, 
                type2), method = method, available = av2, destdir = destdir, 
                dependencies = NULL, quiet = quiet, ...)
        }
        pkgs <- setdiff(pkgs, bins)
        if (!length(pkgs)) 
            return(invisible())
        message(sprintf(ngettext(length(pkgs), "installing the source package %s", 
            "installing the source packages %s"), paste(sQuote(pkgs), 
            collapse = ", ")), "\n", domain = NA)
        flush.console()
    }
    else if (getOption("install.packages.check.source", "yes") %in% 
        "yes" && (type %in% "win.binary" || startsWith(type, 
        "mac.binary"))) {
        if (missing(contriburl) && is.null(available) && !is.null(repos)) {
            contriburl2 <- contrib.url(repos, "source")
            if (missing(repos)) 
                repos <- getOption("repos")
            av1 <- tryCatch(suppressWarnings(available.packages(contriburl = contriburl2, 
                method = method, ...)), error = function(e) e)
            if (inherits(av1, "error")) {
                message("source repository is unavailable to check versions")
                available <- available.packages(contriburl = contrib.url(repos, 
                  type), method = method, ...)
            }
            else {
                srcpkgs <- pkgs[pkgs %in% row.names(av1)]
                available <- available.packages(contriburl = contrib.url(repos, 
                  type), method = method, ...)
                bins <- pkgs[pkgs %in% row.names(available)]
                na <- srcpkgs[!srcpkgs %in% bins]
                if (length(na)) {
                  msg <- sprintf(ngettext(length(na), "package %s is available as a source package but not as a binary", 
                    "packages %s are available as source packages but not as binaries"), 
                    paste(sQuote(na), collapse = ", "))
                  cat("\n   ", msg, "\n\n", sep = "")
                }
                binvers <- available[bins, "Version"]
                srcvers <- binvers
                OK <- bins %in% srcpkgs
                srcvers[OK] <- av1[bins[OK], "Version"]
                later <- as.numeric_version(binvers) < srcvers
                if (any(later)) {
                  msg <- ngettext(sum(later), "There is a binary version available (and will be installed) but the source version is later", 
                    "There are binary versions available (and will be installed) but the source versions are later")
                  cat("\n", paste(strwrap(msg, indent = 2, exdent = 2), 
                    collapse = "\n"), ":\n", sep = "")
                  print(data.frame(binary = binvers, source = srcvers, 
                    row.names = bins, check.names = FALSE)[later, 
                    ])
                  cat("\n")
                }
            }
        }
    }
    if (.Platform$OS.type == "windows") {
        if (startsWith(type, "mac.binary")) 
            stop("cannot install macOS binary packages on Windows")
        if (type %in% "win.binary") {
            .install.winbinary(pkgs = pkgs, lib = lib, contriburl = contriburl, 
                method = method, available = available, destdir = destdir, 
                dependencies = dependencies, libs_only = libs_only, 
                quiet = quiet, ...)
            return(invisible())
        }
        have_spaces <- grep(" ", pkgs)
        if (length(have_spaces)) {
            p <- pkgs[have_spaces]
            dirs <- shortPathName(dirname(p))
            pkgs[have_spaces] <- file.path(dirs, basename(p))
        }
        pkgs <- gsub("\\\\", "/", pkgs)
    }
    else {
        if (startsWith(type, "mac.binary")) {
            if (!grepl("darwin", R.version$platform)) 
                stop("cannot install macOS binary packages on this platform")
            .install.macbinary(pkgs = pkgs, lib = lib, contriburl = contriburl, 
                method = method, available = available, destdir = destdir, 
                dependencies = dependencies, quiet = quiet, ...)
            return(invisible())
        }
        if (type %in% "win.binary") 
            stop("cannot install Windows binary packages on this platform")
        if (!file.exists(file.path(R.home("bin"), "INSTALL"))) 
            stop("This version of R is not set up to install source packages\nIf it was installed from an RPM, you may need the R-devel RPM")
    }
    libpath <- .libPaths()
    libpath <- libpath[!libpath %in% .Library]
    if (length(libpath)) 
        libpath <- paste(libpath, collapse = .Platform$path.sep)
    cmd0 <- file.path(R.home("bin"), "R")
    args0 <- c("CMD", "INSTALL")
    output <- if (quiet) 
        FALSE
    else ""
    env <- character()
    tlim <- Sys.getenv("_R_INSTALL_PACKAGES_ELAPSED_TIMEOUT_")
    tlim <- if (is.na(tlim)) 
        0
    else tools:::get_timeout(tlim)
    outdir <- getwd()
    if (is.logical(keep_outputs)) {
        if (is.na(keep_outputs)) 
            keep_outputs <- FALSE
    }
    else if (is.character(keep_outputs) && (length(keep_outputs) == 
        1L)) {
        if (!dir.exists(keep_outputs) && !dir.create(keep_outputs, 
            recursive = TRUE)) 
            stop(gettextf("unable to create %s", sQuote(keep_outputs)), 
                domain = NA)
        outdir <- normalizePath(keep_outputs)
        keep_outputs <- TRUE
    }
    else stop(gettextf("invalid %s argument", sQuote("keep_outputs")), 
        domain = NA)
    if (length(libpath)) {
        if (.Platform$OS.type == "windows") {
            oldrlibs <- Sys.getenv("R_LIBS")
            Sys.setenv(R_LIBS = libpath)
            on.exit(Sys.setenv(R_LIBS = oldrlibs))
        }
        else env <- paste0("R_LIBS=", shQuote(libpath))
    }
    if (is.character(clean)) 
        args0 <- c(args0, clean)
    if (libs_only) 
        args0 <- c(args0, "--libs-only")
    if (!missing(INSTALL_opts)) {
        if (!is.list(INSTALL_opts)) {
            args0 <- c(args0, paste(INSTALL_opts, collapse = " "))
            INSTALL_opts <- list()
        }
    }
    else {
        INSTALL_opts <- list()
    }
    if (verbose) 
        message(gettextf("system (cmd0): %s", paste(c(cmd0, args0), 
            collapse = " ")), domain = NA)
    if (is.null(repos) & missing(contriburl)) {
        update <- cbind(path.expand(pkgs), lib)
        for (i in seq_len(nrow(update))) {
            if (is.na(update[i, 1L])) 
                next
            args <- c(args0, get_install_opts(update[i, 1L]), 
                "-l", shQuote(update[i, 2L]), getConfigureArgs(update[i, 
                  1L]), getConfigureVars(update[i, 1L]), shQuote(update[i, 
                  1L]))
            status <- system2(cmd0, args, env = env, stdout = output, 
                stderr = output, timeout = tlim)
            if (status > 0L) 
                warning(gettextf("installation of package %s had non-zero exit status", 
                  sQuote(update[i, 1L])), domain = NA)
            else if (verbose) {
                cmd <- paste(c(cmd0, args), collapse = " ")
                message(sprintf("%d): succeeded '%s'", i, cmd), 
                  domain = NA)
            }
        }
        return(invisible())
    }
    tmpd <- destdir
    nonlocalrepos <- !all(startsWith(contriburl, "file:"))
    if (is.null(destdir) && nonlocalrepos) {
        tmpd <- file.path(tempdir(), "downloaded_packages")
        if (!file.exists(tmpd) && !dir.create(tmpd)) 
            stop(gettextf("unable to create temporary directory %s", 
                sQuote(tmpd)), domain = NA)
    }
    if (is.null(available)) 
        available <- available.packages(contriburl = contriburl, 
            method = method, ...)
    if (getDeps) 
        pkgs <- getDependencies(pkgs, dependencies, available, 
            lib, ...)
    foundpkgs <- download.packages(pkgs, destdir = tmpd, available = available, 
        contriburl = contriburl, method = method, type = "source", 
        quiet = quiet, ...)
    if (length(foundpkgs)) {
        if (verbose) 
            message(gettextf("foundpkgs: %s", paste(foundpkgs, 
                collapse = ", ")), domain = NA)
        update <- unique(cbind(pkgs, lib))
        colnames(update) <- c("Package", "LibPath")
        found <- pkgs %in% foundpkgs[, 1L]
        files <- foundpkgs[match(pkgs[found], foundpkgs[, 1L]), 
            2L]
        if (verbose) 
            message(gettextf("files: %s", paste(files, collapse = ", \n\t")), 
                domain = NA)
        update <- cbind(update[found, , drop = FALSE], file = files)
        if (nrow(update) > 1L) {
            upkgs <- unique(pkgs <- update[, 1L])
            DL <- .make_dependency_list(upkgs, available)
            p0 <- .find_install_order(upkgs, DL)
            update <- update[sort.list(match(pkgs, p0)), ]
        }
        if (Ncpus > 1L && nrow(update) > 1L) {
            tlim_cmd <- character()
            if (tlim > 0) {
                if (nzchar(timeout <- Sys.which("timeout"))) {
                  tlim_cmd <- c(shQuote(timeout), "--signal=INT", 
                    tlim)
                }
                else warning("timeouts for parallel installs require the 'timeout' command")
            }
            args0 <- c(args0, "--pkglock")
            tmpd2 <- file.path(tempdir(), "make_packages")
            if (!file.exists(tmpd2) && !dir.create(tmpd2)) 
                stop(gettextf("unable to create temporary directory %s", 
                  sQuote(tmpd2)), domain = NA)
            mfile <- file.path(tmpd2, "Makefile")
            conn <- file(mfile, "wt")
            deps <- paste(paste0(update[, 1L], ".ts"), collapse = " ")
            deps <- strwrap(deps, width = 75, exdent = 2)
            deps <- paste(deps, collapse = " \\\n")
            cat("all: ", deps, "\n", sep = "", file = conn)
            aDL <- .make_dependency_list(upkgs, available, recursive = TRUE)
            for (i in seq_len(nrow(update))) {
                pkg <- update[i, 1L]
                args <- c(args0, get_install_opts(update[i, 3L]), 
                  "-l", shQuote(update[i, 2L]), getConfigureArgs(update[i, 
                    3L]), getConfigureVars(update[i, 3L]), shQuote(update[i, 
                    3L]), ">", paste0(pkg, ".out"), "2>&1")
                cmd <- paste(c("MAKEFLAGS=", tlim_cmd, shQuote(cmd0), 
                  args), collapse = " ")
                deps <- aDL[[pkg]]
                deps <- deps[deps %in% upkgs]
                deps <- if (length(deps)) 
                  paste(paste0(deps, ".ts"), collapse = " ")
                else ""
                cat(paste0(pkg, ".ts: ", deps), paste("\t@echo begin installing package", 
                  sQuote(pkg)), paste0("\t@", cmd, " && touch ", 
                  pkg, ".ts"), paste0("\t@cat ", pkg, ".out"), 
                  "", sep = "\n", file = conn)
            }
            close(conn)
            cwd <- setwd(tmpd2)
            on.exit(setwd(cwd))
            status <- system2(Sys.getenv("MAKE", "make"), c("-k -j", 
                Ncpus), stdout = output, stderr = output, env = env)
            if (status > 0L) {
                pkgs <- update[, 1L]
                tss <- sub("[.]ts$", "", dir(".", pattern = "[.]ts$"))
                failed <- pkgs[!pkgs %in% tss]
                for (pkg in failed) system(paste0("cat ", pkg, 
                  ".out"))
                warning(gettextf("installation of one or more packages failed,\n  probably %s", 
                  paste(sQuote(failed), collapse = ", ")), domain = NA)
            }
            if (keep_outputs) 
                file.copy(paste0(update[, 1L], ".out"), outdir)
            setwd(cwd)
            on.exit()
            unlink(tmpd2, recursive = TRUE)
        }
        else {
            outfiles <- paste0(update[, 1L], ".out")
            for (i in seq_len(nrow(update))) {
                outfile <- if (keep_outputs) 
                  outfiles[i]
                else output
                args <- c(args0, get_install_opts(update[i, 3L]), 
                  "-l", shQuote(update[i, 2L]), getConfigureArgs(update[i, 
                    3L]), getConfigureVars(update[i, 3L]), update[i, 
                    3L])
                status <- system2(cmd0, args, env = env, stdout = outfile, 
                  stderr = outfile, timeout = tlim)
                if (!quiet && keep_outputs) 
                  writeLines(readLines(outfile))
                if (status > 0L) 
                  warning(gettextf("installation of package %s had non-zero exit status", 
                    sQuote(update[i, 1L])), domain = NA)
                else if (verbose) {
                  cmd <- paste(c(cmd0, args), collapse = " ")
                  message(sprintf("%d): succeeded '%s'", i, cmd), 
                    domain = NA)
                }
            }
            if (keep_outputs && (outdir != getwd())) {
                file.copy(outfiles, outdir)
                file.remove(outfiles)
            }
        }
        if (!quiet && nonlocalrepos && !is.null(tmpd) && is.null(destdir)) 
            cat("\n", gettextf("The downloaded source packages are in\n\t%s", 
                sQuote(normalizePath(tmpd, mustWork = FALSE))), 
                "\n", sep = "", file = stderr())
        libs_used <- unique(update[, 2L])
        if (.Platform$OS.type == "unix" && .Library %in% libs_used) {
            message("Updating HTML index of packages in '.Library'")
            make.packages.html(.Library)
        }
    }
    else if (!is.null(tmpd) && is.null(destdir)) 
        unlink(tmpd, TRUE)
    invisible()
}
.libPaths <-
function (new,noRHOME=FALSE) 
{
    if( !missing(new) ) base:::.libPaths(new=new) else {
	    paths <- base:::.libPaths()
		if( noRHOME ){
			rhome <- sub( "/PROGRA~1/", "/Program Files/", R.home() )
			paths <- paths[ paths != paste0(rhome,"/library") ]
		}
		paths
	}
}
library <-
function (package, help, pos = 2, lib.loc = NULL, character.only = FALSE, 
    logical.return = FALSE, warn.conflicts, quietly = FALSE, 
    verbose = getOption("verbose"), mask.ok, exclude, include.only, 
    attach.required = missing(include.only), latest = TRUE) 
{
    conf.ctrl <- getOption("conflicts.policy")
    if (is.character(conf.ctrl)) 
        conf.ctrl <- switch(conf.ctrl, strict = list(error = TRUE, 
            warn = FALSE), depends.ok = list(error = TRUE, generics.ok = TRUE, 
            can.mask = c("base", "methods", "utils", "grDevices", 
                "graphics", "stats"), depends.ok = TRUE), warning(gettextf("unknown conflict policy: %s", 
            sQuote(conf.ctrl)), call. = FALSE, domain = NA))
    if (!is.list(conf.ctrl)) 
        conf.ctrl <- NULL
    stopOnConflict <- isTRUE(conf.ctrl$error)
    if (missing(warn.conflicts)) 
        warn.conflicts <- if (isFALSE(conf.ctrl$warn)) 
            FALSE
        else TRUE
    if ((!missing(include.only)) && (!missing(exclude))) 
        stop(gettext("only one of 'include.only' and 'exclude' can be used"), 
            call. = FALSE, domain = NA)
    testRversion <- function(pkgInfo, pkgname, pkgpath) {
        if (is.null(built <- pkgInfo$Built)) 
            stop(gettextf("package %s has not been installed properly\n", 
                sQuote(pkgname)), call. = FALSE, domain = NA)
        R_version_built_under <- as.numeric_version(built$R)
        if (R_version_built_under < "3.0.0") 
            stop(gettextf("package %s was built before R 3.0.0: please re-install it", 
                sQuote(pkgname)), call. = FALSE, domain = NA)
        current <- getRversion()
        if (length(Rdeps <- pkgInfo$Rdepends2)) {
            for (dep in Rdeps) if (length(dep) > 1L) {
                target <- dep$version
                res <- if (is.character(target)) {
                  do.call(dep$op, list(as.numeric(R.version[["svn rev"]]), 
                    as.numeric(sub("^r", "", dep$version))))
                }
                else {
                  do.call(dep$op, list(current, as.numeric_version(target)))
                }
                if (!res) 
                  stop(gettextf("This is R %s, package %s needs %s %s", 
                    current, sQuote(pkgname), dep$op, target), 
                    call. = FALSE, domain = NA)
            }
        }
        if (R_version_built_under > current) 
            warning(gettextf("package %s was built under R version %s", 
                sQuote(pkgname), as.character(built$R)), call. = FALSE, 
                domain = NA)
        platform <- built$Platform
        r_arch <- .Platform$r_arch
        if (.Platform$OS.type == "unix") {
        }
        else {
            if (nzchar(platform) && !grepl("mingw", platform)) 
                stop(gettextf("package %s was built for %s", 
                  sQuote(pkgname), platform), call. = FALSE, 
                  domain = NA)
        }
        if (nzchar(r_arch) && file.exists(file.path(pkgpath, 
            "libs")) && !file.exists(file.path(pkgpath, "libs", 
            r_arch))) 
            stop(gettextf("package %s is not installed for 'arch = %s'", 
                sQuote(pkgname), r_arch), call. = FALSE, domain = NA)
    }
    checkNoGenerics <- function(env, pkg) {
        nenv <- env
        ns <- .getNamespace(as.name(pkg))
        if (!is.null(ns)) 
            nenv <- asNamespace(ns)
        if (exists(".noGenerics", envir = nenv, inherits = FALSE)) 
            TRUE
        else {
            !any(startsWith(names(env), ".__T"))
        }
    }
    checkConflicts <- function(package, pkgname, pkgpath, nogenerics, 
        env) {
        dont.mind <- c("last.dump", "last.warning", ".Last.value", 
            ".Random.seed", ".Last.lib", ".onDetach", ".packageName", 
            ".noGenerics", ".required", ".no_S3_generics", ".Depends", 
            ".requireCachedGenerics")
        sp <- search()
        lib.pos <- which(sp == pkgname)
        ob <- names(as.environment(lib.pos))
        if (!nogenerics) {
            these <- ob[startsWith(ob, ".__T__")]
            gen <- gsub(".__T__(.*):([^:]+)", "\\1", these)
            from <- gsub(".__T__(.*):([^:]+)", "\\2", these)
            gen <- gen[from != package]
            ob <- ob[!(ob %in% gen)]
        }
        ipos <- seq_along(sp)[-c(lib.pos, match(c("Autoloads", 
            "CheckExEnv"), sp, 0L))]
        cpos <- NULL
        conflicts <- vector("list", 0)
        for (i in ipos) {
            obj.same <- match(names(as.environment(i)), ob, nomatch = 0L)
            if (any(obj.same > 0)) {
                same <- ob[obj.same]
                same <- same[!(same %in% dont.mind)]
                Classobjs <- which(startsWith(same, ".__"))
                if (length(Classobjs)) 
                  same <- same[-Classobjs]
                same.isFn <- function(where) vapply(same, exists, 
                  NA, where = where, mode = "function", inherits = FALSE)
                same <- same[same.isFn(i) == same.isFn(lib.pos)]
                not.Ident <- function(ch, TRAFO = identity, ...) vapply(ch, 
                  function(.) !identical(TRAFO(get(., i)), TRAFO(get(., 
                    lib.pos)), ...), NA)
                if (length(same)) 
                  same <- same[not.Ident(same)]
                if (length(same) && identical(sp[i], "package:base")) 
                  same <- same[not.Ident(same, ignore.environment = TRUE)]
                if (length(same)) {
                  conflicts[[sp[i]]] <- same
                  cpos[sp[i]] <- i
                }
            }
        }
        if (length(conflicts)) {
            if (stopOnConflict) {
                emsg <- ""
                pkg <- names(conflicts)
                notOK <- vector("list", 0)
                for (i in seq_along(conflicts)) {
                  pkgname <- sub("^package:", "", pkg[i])
                  if (pkgname %in% canMaskEnv$canMask) 
                    next
                  same <- conflicts[[i]]
                  if (is.list(mask.ok)) 
                    myMaskOK <- mask.ok[[pkgname]]
                  else myMaskOK <- mask.ok
                  if (isTRUE(myMaskOK)) 
                    same <- NULL
                  else if (is.character(myMaskOK)) 
                    same <- setdiff(same, myMaskOK)
                  if (length(same)) {
                    notOK[[pkg[i]]] <- same
                    msg <- .maskedMsg(sort(same), pkg = sQuote(pkg[i]), 
                      by = cpos[i] < lib.pos)
                    emsg <- paste(emsg, msg, sep = "\n")
                  }
                }
                if (length(notOK)) {
                  msg <- gettextf("Conflicts attaching package %s:\n%s", 
                    sQuote(package), emsg)
                  stop(errorCondition(msg, package = package, 
                    conflicts = conflicts, class = "packageConflictError"))
                }
            }
            if (warn.conflicts) {
                packageStartupMessage(gettextf("\nAttaching package: %s\n", 
                  sQuote(package)), domain = NA)
                pkg <- names(conflicts)
                for (i in seq_along(conflicts)) {
                  msg <- .maskedMsg(sort(conflicts[[i]]), pkg = sQuote(pkg[i]), 
                    by = cpos[i] < lib.pos)
                  packageStartupMessage(msg, domain = NA)
                }
            }
        }
    }
    if (verbose && quietly) 
        message("'verbose' and 'quietly' are both true; being verbose then ..")
    if (!missing(package)) {
        if (is.null(lib.loc)) 
            lib.loc <- .libPaths()
        lib.loc <- lib.loc[dir.exists(lib.loc)]
        if (!character.only) 
            package <- as.character(substitute(package))
        if (length(package) != 1L) 
            stop("'package' must be of length 1")
        if (is.na(package) || (package == "")) 
            stop("invalid package name")
        pkgname <- paste0("package:", package)
        newpackage <- is.na(match(pkgname, search()))
        if (newpackage) {
            pkgpath <- find.package(package, lib.loc, quiet = TRUE, 
                verbose = verbose, latest = latest)
            if (length(pkgpath) == 0L) {
                if (length(lib.loc) && !logical.return) 
                  stop(packageNotFoundError(package, lib.loc, 
                    sys.call()))
                txt <- if (length(lib.loc)) 
                  gettextf("there is no package called %s", sQuote(package))
                else gettext("no library trees found in 'lib.loc'")
                if (logical.return) {
                  warning(txt, domain = NA)
                  return(FALSE)
                }
                else stop(txt, domain = NA)
            }
            which.lib.loc <- normalizePath(dirname(pkgpath), 
                "/", TRUE)
            pfile <- system.file("Meta", "package.rds", package = package, 
                lib.loc = which.lib.loc)
            if (!nzchar(pfile)) 
                stop(gettextf("%s is not a valid installed package", 
                  sQuote(package)), domain = NA)
            pkgInfo <- readRDS(pfile)
            testRversion(pkgInfo, package, pkgpath)
            if (is.character(pos)) {
                npos <- match(pos, search())
                if (is.na(npos)) {
                  warning(gettextf("%s not found on search path, using pos = 2", 
                    sQuote(pos)), domain = NA)
                  pos <- 2
                }
                else pos <- npos
            }
            deps <- unique(names(pkgInfo$Depends))
            depsOK <- isTRUE(conf.ctrl$depends.ok)
            if (depsOK) {
                canMaskEnv <- dynGet("__library_can_mask__", 
                  NULL)
                if (is.null(canMaskEnv)) {
                  canMaskEnv <- new.env()
                  canMaskEnv$canMask <- union("base", conf.ctrl$can.mask)
                  "__library_can_mask__" <- canMaskEnv
                }
                canMaskEnv$canMask <- unique(c(package, deps, 
                  canMaskEnv$canMask))
            }
            else canMaskEnv <- NULL
            if (attach.required) 
                .getRequiredPackages2(pkgInfo, quietly = quietly)
            cr <- conflictRules(package)
            if (missing(mask.ok)) 
                mask.ok <- cr$mask.ok
            if (missing(exclude)) 
                exclude <- cr$exclude
            if (packageHasNamespace(package, which.lib.loc)) {
                if (isNamespaceLoaded(package)) {
                  newversion <- as.numeric_version(pkgInfo$DESCRIPTION["Version"])
                  oldversion <- as.numeric_version(getNamespaceVersion(package))
                  if (newversion != oldversion) {
                    tryCatch(unloadNamespace(package), error = function(e) {
                      P <- if (!is.null(cc <- conditionCall(e))) 
                        paste("Error in", deparse(cc)[1L], ": ")
                      else "Error : "
                      stop(gettextf("Package %s version %s cannot be unloaded:\n %s", 
                        sQuote(package), oldversion, paste0(P, 
                          conditionMessage(e), "\n")), domain = NA)
                    })
                  }
                }
                tt <- tryCatch({
                  attr(package, "LibPath") <- which.lib.loc
                  ns <- loadNamespace(package, lib.loc)
                  env <- attachNamespace(ns, pos = pos, deps, 
                    exclude, include.only)
                }, error = function(e) {
                  P <- if (!is.null(cc <- conditionCall(e))) 
                    paste(" in", deparse(cc)[1L])
                  else ""
                  msg <- gettextf("package or namespace load failed for %s%s:\n %s", 
                    sQuote(package), P, conditionMessage(e))
                  if (logical.return) 
                    message(paste("Error:", msg), domain = NA)
                  else stop(msg, call. = FALSE, domain = NA)
                })
                if (logical.return && is.null(tt)) 
                  return(FALSE)
                attr(package, "LibPath") <- NULL
                {
                  on.exit(detach(pos = pos))
                  nogenerics <- !.isMethodsDispatchOn() || checkNoGenerics(env, 
                    package)
                  if (isFALSE(conf.ctrl$generics.ok) || (stopOnConflict && 
                    !isTRUE(conf.ctrl$generics.ok))) 
                    nogenerics <- TRUE
                  if (stopOnConflict || (warn.conflicts && !exists(".conflicts.OK", 
                    envir = env, inherits = FALSE))) 
                    checkConflicts(package, pkgname, pkgpath, 
                      nogenerics, ns)
                  on.exit()
                  if (logical.return) 
                    return(TRUE)
                  else return(invisible(.packages()))
                }
            }
            else stop(gettextf("package %s does not have a namespace and should be re-installed", 
                sQuote(package)), domain = NA)
        }
        if (verbose && !newpackage) 
            warning(gettextf("package %s already present in search()", 
                sQuote(package)), domain = NA)
    }
    else if (!missing(help)) {
        if (!character.only) 
            help <- as.character(substitute(help))
        pkgName <- help[1L]
        pkgPath <- find.package(pkgName, lib.loc, verbose = verbose, latest = latest)
        docFiles <- c(file.path(pkgPath, "Meta", "package.rds"), 
            file.path(pkgPath, "INDEX"))
        if (file.exists(vignetteIndexRDS <- file.path(pkgPath, 
            "Meta", "vignette.rds"))) 
            docFiles <- c(docFiles, vignetteIndexRDS)
        pkgInfo <- vector("list", 3L)
        readDocFile <- function(f) {
            if (basename(f) %in% "package.rds") {
                txt <- readRDS(f)$DESCRIPTION
                if ("Encoding" %in% names(txt)) {
                  to <- if (Sys.getlocale("LC_CTYPE") == "C") 
                    "ASCII//TRANSLIT"
                  else ""
                  tmp <- try(iconv(txt, from = txt["Encoding"], 
                    to = to))
                  if (!inherits(tmp, "try-error")) 
                    txt <- tmp
                  else warning("'DESCRIPTION' has an 'Encoding' field and re-encoding is not possible", 
                    call. = FALSE)
                }
                nm <- paste0(names(txt), ":")
                formatDL(nm, txt, indent = max(nchar(nm, "w")) + 
                  3L)
            }
            else if (basename(f) %in% "vignette.rds") {
                txt <- readRDS(f)
                if (is.data.frame(txt) && nrow(txt)) 
                  cbind(basename(gsub("\\.[[:alpha:]]+$", "", 
                    txt$File)), paste(txt$Title, paste0(rep.int("(source", 
                    NROW(txt)), ifelse(nzchar(txt$PDF), ", pdf", 
                    ""), ")")))
                else NULL
            }
            else readLines(f)
        }
        for (i in which(file.exists(docFiles))) pkgInfo[[i]] <- readDocFile(docFiles[i])
        y <- list(name = pkgName, path = pkgPath, info = pkgInfo)
        class(y) <- "packageInfo"
        return(y)
    }
    else {
        if (is.null(lib.loc)) 
            lib.loc <- .libPaths()
        db <- matrix(character(), nrow = 0L, ncol = 3L)
        nopkgs <- character()
        for (lib in lib.loc) {
            a <- .packages(all.available = TRUE, lib.loc = lib)
            for (i in sort(a)) {
                file <- system.file("Meta", "package.rds", package = i, 
                  lib.loc = lib)
                title <- if (nzchar(file)) {
                  txt <- readRDS(file)
                  if (is.list(txt)) 
                    txt <- txt$DESCRIPTION
                  if ("Encoding" %in% names(txt)) {
                    to <- if (Sys.getlocale("LC_CTYPE") == "C") 
                      "ASCII//TRANSLIT"
                    else ""
                    tmp <- try(iconv(txt, txt["Encoding"], to, 
                      "?"))
                    if (!inherits(tmp, "try-error")) 
                      txt <- tmp
                    else warning("'DESCRIPTION' has an 'Encoding' field and re-encoding is not possible", 
                      call. = FALSE)
                  }
                  txt["Title"]
                }
                else NA
                if (is.na(title)) 
                  title <- " ** No title available ** "
                db <- rbind(db, cbind(i, lib, title))
            }
            if (length(a) == 0L) 
                nopkgs <- c(nopkgs, lib)
        }
        dimnames(db) <- list(NULL, c("Package", "LibPath", "Title"))
        if (length(nopkgs) && !missing(lib.loc)) {
            pkglist <- paste(sQuote(nopkgs), collapse = ", ")
            msg <- sprintf(ngettext(length(nopkgs), "library %s contains no packages", 
                "libraries %s contain no packages"), pkglist)
            warning(msg, domain = NA)
        }
        y <- list(header = NULL, results = db, footer = NULL)
        class(y) <- "libraryIQR"
        return(y)
    }
    if (logical.return) 
        TRUE
    else invisible(.packages())
}
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
update.packages <-
function (lib.loc = NULL, repos = getOption("repos"), contriburl = contrib.url(repos, 
    type), method, instlib = NULL, ask = TRUE, available = NULL, 
    oldPkgs = NULL, ..., checkBuilt = FALSE, type = getOption("pkgType"),
	noDupl = TRUE, noInstallRHOME = TRUE, saveOld = TRUE) 
{
    force(ask)
    text.select <- function(old) {
        update <- NULL
        for (k in seq_len(nrow(old))) {
            cat(old[k, "Package"], ":\n", "Version", old[k, "Installed"], 
                "installed in", old[k, "LibPath"], if (checkBuilt) 
                  paste("built under R", old[k, "Built"]), "\n", 
                "Version", old[k, "ReposVer"], "available at", 
                simplifyRepos(old[k, "Repository"], type) )
            cat("\n")
            answer <- askYesNo("Update?")
            if (is.na(answer)) {
                cat("cancelled by user\n")
                return(invisible())
            }
            if (isTRUE(answer)) 
                update <- rbind(update, old[k, ])
        }
        update
    }
    if (is.null(lib.loc)) 
        lib.loc <- .libPaths()
    if (type == "both" && (!missing(contriburl) || !is.null(available))) {
        stop("specifying 'contriburl' or 'available' requires a single type, not type = \"both\"")
    }
    if (is.null(available)) {
        available <- available.packages(contriburl = contriburl, 
            method = method, ...)
        if (missing(repos)) 
            repos <- getOption("repos")
    }
    if (!is.matrix(oldPkgs) && is.character(oldPkgs)) {
        subset <- oldPkgs
        oldPkgs <- NULL
    }
    else subset <- NULL
    if (is.null(oldPkgs)) {
        oldPkgs <- old.packages(lib.loc = lib.loc, contriburl = contriburl, 
            method = method, available = available, checkBuilt = checkBuilt, noDupl = noDupl)
        if (missing(repos)) 
            repos <- getOption("repos")
        if (!is.null(oldPkgs)) {
            pkg <- 0L
            while (pkg < nrow(oldPkgs)) {
                pkg <- pkg + 1L
                if (find.package(oldPkgs[pkg], lib.loc = lib.loc) != 
                  find.package(oldPkgs[pkg], lib.loc = oldPkgs[pkg, 
                    2])) {
                  warning(sprintf("package '%s' in library '%s' will not be updated", 
                    oldPkgs[pkg], oldPkgs[pkg, 2]), call. = FALSE, 
                    immediate. = TRUE)
                  oldPkgs <- oldPkgs[-pkg, , drop = FALSE]
                  pkg <- pkg - 1L
                }
            }
        }
        if (is.null(oldPkgs)) 
            return(invisible())
    }
    else if (!(is.matrix(oldPkgs) && is.character(oldPkgs))) 
        stop("invalid 'oldPkgs'; must be a character vector or a result from old.packages()")
    if (!is.null(subset)) {
        oldPkgs <- oldPkgs[rownames(oldPkgs) %in% subset, , drop = FALSE]
        if (nrow(oldPkgs) == 0) 
            return(invisible())
    }
    update <- if (is.character(ask) && ask == "graphics") {
        if (.Platform$OS.type == "windows" || .Platform$GUI == 
            "AQUA" || (capabilities("tcltk") && capabilities("X11"))) {
            k <- select.list(oldPkgs[, 1L], oldPkgs[, 1L], multiple = TRUE, 
                title = "Packages to be updated", graphics = TRUE)
            oldPkgs[match(k, oldPkgs[, 1L]), , drop = FALSE]
        }
        else text.select(oldPkgs)
    }
    else if (isTRUE(ask)) 
        text.select(oldPkgs)
    else oldPkgs
    if (length(update)) {
        if (is.null(instlib)) 
            instlib <- update[, "LibPath"]
        libs <- unique(instlib)
        for (l in libs) if (type == "both") 
            install.packages(update[instlib == l, "Package"], 
                l, repos = repos, method = method, ..., type = type,
				noInstallRHOME = noInstallRHOME, saveOld = saveOld)
        else install.packages(update[instlib == l, "Package"], 
            l, contriburl = contriburl, method = method, available = available, 
            ..., type = type, noInstallRHOME = noInstallRHOME, saveOld = saveOld)
    }
}
