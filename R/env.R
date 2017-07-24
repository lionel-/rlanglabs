
is_namespace <- function(ns) isNamespace(ns)
ns_exports <- function(ns) getNamespaceExports(ns)
ns_imports <- function(ns) getNamespaceImports(ns)
is_empty_env <- function(x) is_reference(x, empty_env())


is_package_name <- function(nm) {
  if (!is_string(nm)) {
    FALSE
  } else {
    identical(substr(nm, 0, 8), "package:")
  }
}
is_package_env <- function(env) {
  if (!is_env(env)) {
    return(FALSE)
  } else if (identical(env, base_env())) {
    return(TRUE)
  }

  nm <- attr(env, "name")
  if (is_null(nm)) {
    return(FALSE)
  }

  is_package_name(nm)
}

pkg_name <- function(pkg) {
  stopifnot(is_string(pkg) || is_env(pkg))

  if (is_env(pkg)) {
    # The base package does not have a `name` attribute
    if (is_reference(pkg, base_env())) {
      return("base")
    }

    if (!is_package_env(pkg)) {
      abort("`pkg` must be a package environment")
    }
    pkg <- attr(pkg, "name")
  }

  substr(pkg, 9, nchar(pkg))
}
pkg_env_name <- function(pkg) {
  if (is_string(pkg)) {
    paste0("package:", pkg)
  } else if (is_reference(pkg, base_env())) {
    "package:base"
  } else if (is_package_env(pkg)) {
    attr(pkg, "name")
  } else {
    abort("`pkg` must be a string or a package environment")
  }
}

is_scoped <- function(env) {
  switch_type(env,
    string = env %in% scoped_names(),
    environment = {
      cur <- global_env()
      while(!identical(cur, empty_env())) {
        if (identical(cur, env)) {
          return(TRUE)
        }
        cur <- env_parent(cur)
      }
      FALSE
    },
    abort("`env` must be a string or an environment")
  )
}
scoped_name <- function(env) {
  stopifnot(is_env(env))

  i <- 0
  cur <- global_env()
  while(!identical(cur, empty_env())) {
    i <- i + 1
    if (identical(cur, env)) {
      return(search()[[i]])
    }
    cur <- env_parent(cur)
  }

  abort("`env` must be on the search path")
}


env_type <- function(env) {
  if (identical(env, global_env())) {
    nm <- NULL
    path_nm <- ".GlobalEnv"
    type <- "global"
  } else if (is_namespace(env)) {
    nm <- ns_env_name(env)
    path_nm <- NULL
    type <- "namespace"
  } else if (is_package_env(env)) {
    nm <- pkg_name(env)
    path_nm <- pkg_env_name(nm)
    type <- "package"
  } else if (is_scoped(env)) {
    nm <- scoped_name(env)
    path_nm <- nm
    type <- "scoped"
  } else if (identical(env, empty_env())) {
    nm <- NULL
    path_nm <- NULL
    type <- "empty"
  } else {
    nm <- NULL
    path_nm <- NULL
    type <- "local"
  }

  set_attrs(type,
    class = "env_type",
    name = nm,
    path_name = path_nm,
    env = env
  )
}
print.env_type <- function(x, ...) {
  nm <- attr(x, "name")
  if (is_null(nm)) {
    cat(sprintf("<%s>\n", x))
  } else {
    cat(sprintf("<%s: %s>\n", x, nm))
  }
}

# A more complete version of this function might return an environment
# with several parents, one for each package in the `Depends` field.
# Creating a search path from several new package envs would then
# require a topological sort and merge.
new_package_env <- function(pkg) {
  stopifnot(is_string(pkg))
  if (is_package_name(pkg)) {
    pkg_env_name <- pkg
    pkg <- pkg_name(pkg)
  } else {
    pkg_env_name <- pkg_env_name(pkg)
  }

  # This loads the namespace by side effect
  if (!is_installed(pkg)) {
    abort(sprintf("Package `%s` must be installed", pkg))
  }

  # We assume no one is going to change the parent of the base
  # namespace, so it is safe to directly return it
  if (pkg == "base") {
    return(base_env())
  }

  # It is faster to clone an existing package environment
  if (is_scoped(pkg_env_name)) {
    pkg_env <- scoped_env(pkg_env_name)
    clone <- env_clone(pkg_env, empty_env())
    attributes(clone) <- attributes(pkg_env)
    return(clone)
  }

  ns <- ns_env(pkg)
  env <- new_environment()
  exports <- ns_exports(ns)
  importIntoEnv(env, exports, ns, exports)

  mut_attrs(env, name = pkg_env_name, path = .getNamespaceInfo(ns, "path"))
  env
}

envs_link <- function(...) {
  envs <- dots_splice(...)

  stopifnot(length(envs) && every(envs, is_env))
  if (length(envs) == 1) {
    return(envs[[1]])
  }

  reduce(envs[-1], chain_env_parent, .init = envs[[1]])
  envs[[1]]
}
# mut_env_parent() returns `env` but we need `parent` for reducing
chain_env_parent <- function(env, parent) {
  mut_env_parent(env, parent)
  parent
}

#' Mirror an environment
#'
#' A mirror is an environment that shares the same objects but not the
#' same parent. It is an efficient way of cloning an environment when
#' the purpose of the cloning is to have a different parent.
#'
#' @param env An environment to clone.
#' @param parent The parent of the new mirror environment.
#' @export
#' @examples
#' env <- env()
#' mirror <- env_mirror(env)
#'
#' # A mirror is like a shallow clone. The contents of the mirror are
#' # shared with the original environment:
#' mirror$a <- "foo"
#' env$a
#'
#' # On the other hand, the mirror can have a different parent:
#' mut_env_parent(mirror, base_env())
#' env_parent(mirror)
#' env_parent(env)
env_mirror <- function(env, parent = env_parent(env)) {
  .Call(rlang_env_mirror, get_env(env), parent)
}
