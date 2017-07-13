context("environments")

test_that("is_package_env() detects package envs", {
  expect_true(is_package_env(base_env()))
  expect_true(is_package_env(pkg_env("rlang")))
})

test_that("pkg_name() handles corner cases", {
  expect_error(pkg_name(global_env()), "`pkg` must be a package env")
  expect_identical(pkg_name(base_env()), "base")
})

test_that("pkg_env_name() returns package name", {
  expect_identical(pkg_env_name("rlang"), "package:rlang")
  expect_identical(pkg_env_name(pkg_env("rlang")), "package:rlang")
  expect_identical(pkg_env_name(base_env()), "package:base")
  expect_error(pkg_env_name(empty_env()) , "must be a string or a package env")
  expect_error(pkg_env_name(global_env()) , "must be a string or a package env")
})

test_that("new package envs have correct attributes and parent", {
  pkg_env <- pkg_env("utils")
  env <- new_package_env("utils")
  expect_identical(attributes(env), attributes(pkg_env))
  expect_identical(env_parent(env), empty_env())
})

test_that("creating a new base env just returns a reference", {
  expect_true(is_reference(new_package_env("base"), base_env()))
})

test_that("environments are linked together", {
  a <- env()
  b <- env()
  c <- env()

  env <- envs_link(a, b, c)
  expect_identical(env_parent(env, 0), a)
  expect_identical(env_parent(env, 1), b)
  expect_identical(env_parent(env, 2), c)
  expect_identical(env_parent(env, 3), get_env())
})
