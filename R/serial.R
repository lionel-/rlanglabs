
bytes_unserialise <- function(bytes) {
  x <- unserialize(bytes)

  if (!inherits(x, "serialised_path")) {
    return(x)
  }

  path <- attr(x, "path")
  attached <- attr(x, "attached")

  is_pkg <- map_lgl(path, is_package_name)
  path <- map_if(path, is_pkg, new_package_env)
  path <- map_if(path, !is_pkg, function(x) abort("TODO"))

  tail <- env_tail(x, sentinel = global_env())
  mut_env_parent(tail, envs_link(path))

  set_attrs(x, class = NULL, attached = NULL, path = NULL)
}
serialise_bytes <- function(x) {
  if (has_search_path(x)) {
    path_bindings <- clo_trace_path(x)
    path_nms <- map_chr(path_bindings, attr, which = "path_name")
    path_attached <- keep(path_bindings, `==`, "attached")

    x <- set_attrs(x,
      class = "serialised_path",
      path = path_nms,
      attached = path_attached
    )
  }

  serialize(x, NULL)
}

has_enclosure <- function(x) {
  is_closure(clo) || is_quosure(clo)
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

  bindings <- clo_search_bindings(clo)
  path_bindings <- unname(compact(bindings))
  path_attached <- keep(path_bindings, `%in%`, c("package", "scoped"))
  path <- unique(map_chr(path_attached, attr, which = "path_name"))

  order <- order(match(path, search()))
  path_attached[order]
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
  syms[[nm]] <- env_type(binding_env(nm, env))
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
