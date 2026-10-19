# A fake cgroup hierarchy: `files` maps paths under the root to contents, and
# `path` is the process's v2 cgroup as /proc/self/cgroup reports it.
local_cgroup <- function(files = list(), path = "/", env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  for (f in names(files)) {
    dir.create(dirname(file.path(root, f)), recursive = TRUE, showWarnings = FALSE)
    writeLines(files[[f]], file.path(root, f))
  }
  self <- withr::local_tempfile(lines = paste0("0::", path), .local_envir = env)
  list(root = root, self = self)
}

test_that("a cgroups v2 CPU quota caps the cores", {
  cg <- local_cgroup(list(cpu.max = "200000 100000"))
  expect_equal(cgroup_cpu_limit(cg$root, cg$self), 2L)

  cg <- local_cgroup(list(cpu.max = "max 100000"))
  expect_identical(cgroup_cpu_limit(cg$root, cg$self), NA_integer_)
})

test_that("a quota below one core still allows one", {
  cg <- local_cgroup(list(cpu.max = "50000 100000"))
  expect_equal(cgroup_cpu_limit(cg$root, cg$self), 1L)
})

test_that("a quota on an ancestor cgroup applies to the process", {
  cg <- local_cgroup(
    list(`app.slice/cpu.max` = "100000 100000", `app.slice/connect/cpu.max` = "max 100000"),
    path = "/app.slice/connect"
  )
  expect_equal(cgroup_cpu_limit(cg$root, cg$self), 1L)
})

test_that("a cpuset caps the cores", {
  cg <- local_cgroup(list(cpuset.cpus.effective = "0-2,5"))
  expect_equal(cgroup_cpu_limit(cg$root, cg$self), 4L)
  expect_equal(count_cpuset("0-3,6"), 5)
  expect_equal(count_cpuset("7"), 1)
})

test_that("cgroups v1 quotas and cpusets are read", {
  cg <- local_cgroup(list(
    `cpu,cpuacct/cpu.cfs_quota_us` = "300000",
    `cpu,cpuacct/cpu.cfs_period_us` = "100000"
  ))
  expect_equal(cgroup_cpu_limit(cg$root, cg$self), 3L)

  cg <- local_cgroup(list(
    `cpu/cpu.cfs_quota_us` = "-1",
    `cpu/cpu.cfs_period_us` = "100000",
    `cpuset/cpuset.cpus` = "0-1"
  ))
  expect_equal(cgroup_cpu_limit(cg$root, cg$self), 2L)
})

test_that("the tightest of several limits wins", {
  cg <- local_cgroup(list(cpu.max = "400000 100000", cpuset.cpus.effective = "0-1"))
  expect_equal(cgroup_cpu_limit(cg$root, cg$self), 2L)
})

test_that("no cgroup files means no limit", {
  cg <- local_cgroup()
  expect_identical(cgroup_cpu_limit(cg$root, cg$self), NA_integer_)
  expect_identical(cgroup_cpu_limit(cg$root, file.path(cg$root, "missing")), NA_integer_)
})

test_that("available cores are the host's cores capped by the cgroup", {
  cg <- local_cgroup(list(cpu.max = "100000 100000"))
  expect_equal(available_cores(cg$root, cg$self), 1L)

  cg <- local_cgroup()
  host <- parallel::detectCores()
  expect_equal(available_cores(cg$root, cg$self), if (is.na(host)) 1L else host)
})

test_that("the default pool leaves one available core for the session", {
  local_mocked_bindings(available_cores = function(...) 4L)
  expect_equal(default_workers(), 3L)
  local_mocked_bindings(available_cores = function(...) 1L)
  expect_equal(default_workers(), 1L)
})
