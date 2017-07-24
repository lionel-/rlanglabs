#' Serialise and unserialise objects
#'
#' These functions are equivalent to [base::serialize()] and
#' [base::unserialize()] but try to preserve the enclosure of
#' closures, formulas, and quosures. It works by detecting which
#' environment or package attached on the search path at
#' serialisation-time are required by the function or quosure. The
#' relevant environments are then reconstructed in a fake search path
#' when the object is unserialised. The fake search path is specific
#' to each closure and quosure.
#'
#' @param x An object to serialise.
#' @param bytes A raw vector to unserialise.
#' @export
serialise_bytes <- function(x) {
  UseMethod("serialise_bytes")
}
#' @rdname serialise_bytes
#' @export
serialise_bytes.default <- function(x) {
  if (has_search_path(x)) {
    path_bindings <- clo_trace_path(x)
    path_nms <- map_chr(path_bindings, attr, which = "path_name")
    path_attached <- keep(path_bindings, `==`, "attached")

    if (length(path_nms) && path_nms == ".GlobalEnv") {
      global <- new_environment(attr(path_bindings[[1]], "objects"))
    } else {
      global <- NULL
    }

    if (length(path_nms)) {
      x <- set_attrs(x,
        class = "serialised_path",
        path = path_nms,
        attached = path_attached,
        global = global
      )
    }
  }

  serialize(x, NULL)
}

#' @rdname serialise_bytes
#' @export
bytes_unserialise <- function(bytes) {
  UseMethod("bytes_unserialise")
}
#' @rdname serialise_bytes
#' @export
bytes_unserialise.default <- function(bytes) {
  x <- unserialize(bytes)

  if (!inherits(x, "serialised_path")) {
    return(x)
  }

  path <- as_list(attr(x, "path"))

  if (path[[1]] == ".GlobalEnv") {
    path[[1]] <- attr(x, "global")
  }

  path <- map_if(path, is_package_name, new_package_env)
  path <- map_if(path, is_string, function(x) abort("TODO"))

  # Make sure to keep the local environments if they exist
  old_enclosure <- get_env(x)
  new_enclosure <- envs_link(path)
  if (is_reference(old_enclosure, global_env())) {
    x <- set_env(x, new_enclosure)
  } else {
    tail <- env_tail(x, sentinel = global_env())
    mut_env_parent(tail, new_enclosure)
  }

  # FIXME: Wrap serialisation attributes in a single object
  set_attrs(x, class = NULL, attached = NULL, path = NULL, global = NULL)
}

has_enclosure <- function(x) {
  is_closure(x) || is_formulaish(x)
}
has_search_path <- function(x) {
  env <- get_env(x, default = return(FALSE))

  if (is_namespace(env)) {
    return(FALSE)
  }

  while (!is_empty_env(env)) {
    if (identical(env, global_env())) {
      return(TRUE)
    }
    env <- env_parent(env)
  }

  FALSE
}

clo_trace_path <- function(clo) {
  stopifnot(has_enclosure(clo))
  expr <- get_expr(clo)
  env <- get_env(clo)

  # Find all bindings scoped in the search path
  bindings <- clo_search_bindings(clo)

  # Gather all references to global object together
  bindings <- merge_globals(bindings)

  # Discard redundant environments
  bindings <- unname(compact(bindings))
  bindings <- bindings[!duplicated(bindings)]

  # Keep only environments on the search path
  path_attached <- keep(bindings, `%in%`, c("global", "package", "scoped"))
  path_names <- map_chr(path_attached, attr, which = "path_name")

  order <- order(match(path_names, search()))
  path_attached[order]
}
merge_globals <- function(bindings) {
  is_global <- bindings == "global"
  globals <- bindings[is_global]

  if (length(globals)) {
    global <- globals[[1]]
    objects <- set_names(names(globals))
    objects <- map(objects, env_get, env = global_env())
    global <- set_attrs(global, objects = objects)

    bindings <- c(list(global), bindings[!is_global])
  }

  bindings
}

# Using environment for constant-time insertion
clo_search_bindings <- function(clo) {
  stopifnot(has_enclosure(clo))
  syms <- new_environment()
  syms <- search_expr_bindings(syms, get_expr(clo), get_env(clo))
  as_list(syms)
}

search_expr_bindings <- function(syms, expr, env) {
  switch_type(expr,
    symbol = search_symbol_bindings(syms, expr, env),
    language = search_language_bindings(syms, expr, env),
    pairlist = search_pairlist_bindings(syms, expr, env)
  )
  invisible(syms)
}
search_symbol_bindings <- function(syms, expr, env) {
  nm <- as_string(expr)

  if (nchar(nm) > 0) {
    syms[[nm]] <- env_type(binding_env(nm, env))
  }
}
search_language_bindings <- function(syms, expr, env) {
  car <- node_car(expr)

  if (is_symbol(car)) {
    nm <- as_string(car)
    syms[[nm]] <- env_type(binding_env(nm, env))
  } else if (is_language(car)) {
    search_expr_bindings(syms, car, env)
  }

  search_expr_bindings(syms, node_cdr(expr), env)
}
search_pairlist_bindings <- function(syms, expr, env) {
  while (!is_null(expr)) {
    search_expr_bindings(syms, node_car(expr), env)
    expr <- node_cdr(expr)
  }
}

binding_env <- function(nm, env) {
  while(!identical(env, empty_env()) && !env_has(env, nm)) {
    env <- env_parent(env)
  }
  env
}
