context("serialisation")

test_that("detects search path", {
  expect_false(has_search_path(1L))
  expect_false(has_search_path(ns_env("rlang")))
  expect_false(has_search_path(child_env(child_env(NULL))))

  expect_true(has_search_path(env()))
  expect_true(has_search_path(global_env()))
})

test_that("can trace the search path of closures", {
  pkg_env <- new_package_env("utils")
  parent.env(pkg_env) <- base_env()

  # Referring to base::`{` and utils::apropos
  clo <- with_env(pkg_env, function() { apropos })
  path <- clo_trace_path(clo)
  path_names <- map_chr(path, attr, which = "name")
  expect_identical(path_names, c("utils", "base"))
})


roundtrip <- function(x) {
  bytes_unserialise(serialise_bytes(x))
}

test_that("relevant packages are correctly roundtripped", {
  clo <- with_env(global_env(), local({
    foo <- "foo"
    # Refers to rlang, utils, and local env
    function() { ll(foo, apropos) }
  })
  )
  out <- roundtrip(clo)
  expect_identical(out(), list("foo", utils::apropos))

  parents <- env_parents(out)
  expect_true(every(parents[1:3], is_package_env))
  expect_identical(map_chr(parents[1:2], attr, which = "name"), c("package:rlang", "package:utils"))
  expect_true(is_reference(parents[[3]], base_env()))
})
