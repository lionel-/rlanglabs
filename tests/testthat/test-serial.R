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
  path <- clo_trace_path(clo, global_env = TRUE)
  path_names <- map_chr(path, attr, which = "name")
  expect_identical(path_names, c("utils", "base"))
})


roundtrip <- function(x, global_env = TRUE) {
  bytes_unserialise(serialise_bytes(x, global_env))
}

test_that("relevant packages are correctly roundtripped", {
  clo <- with_env(global_env(), local({
    foo <- "foo"
    # Refers to rlang, utils, and local env
    function() { ll(foo, apropos) }
  }))
  out <- roundtrip(clo)
  expect_identical(out(), list("foo", utils::apropos))

  parents <- env_parents(out)
  expect_true(every(parents[1:3], is_package_env))
  expect_identical(map_chr(parents[1:2], attr, which = "name"), c("package:rlang", "package:utils"))
  expect_true(is_reference(parents[[3]], base_env()))
})

test_that("can serialise quosure that don't refer to search path", {
  clo <- with_env(global_env(), function() 10L)
  out <- roundtrip(clo)
  expect_equal(out, function() 10L)
  expect_identical(get_env(out), global_env())
})

test_that("the relevant region of the global environment is serialised", {
  global_clo <- with_env(global_env(), {
    globar <- "bar"
    globaz <- "baz"
    function() list(globar, globaz)
  })

  out <- roundtrip(global_clo)
  expect_identical(out(), list("bar", "baz"))

  out_env <- get_env(out)
  expect_identical(env_names(out_env), chr("globar", "globaz"))
})

test_that("dataframes with filters are serialised", {
  df_clo <- with_env(global_env(), {
    function(df) {
      df[1,]
    }
  })

  out <- roundtrip(df_clo)
  expect_identical(out(iris), df_clo(iris))
})

test_that("the global env is not serialised by default", {
  clo <- with_env(global_env(), {
    function() { environment }
  })

  out <- roundtrip(clo, global_env = FALSE)
  expect_identical(get_env(out), base_env())
})
