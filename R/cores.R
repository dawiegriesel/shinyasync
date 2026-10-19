# Cores this process may use: the machine's cores, capped by the CPU limits
# of its cgroup. parallel::detectCores() reports the host's cores, so in a
# container or on Posit Connect it ignores a CPU quota or a cpuset and the
# default pool would start a daemon per host core.
available_cores <- function(root = "/sys/fs/cgroup", self = "/proc/self/cgroup") {
  n <- parallel::detectCores()
  if (is.na(n)) n <- 1L
  as.integer(min(n, cgroup_cpu_limit(root, self), na.rm = TRUE))
}

# The tightest CPU limit in the process's cgroup, as a whole number of cores,
# or NA when there is none. Reads cgroups v2 (cpu.max, cpuset.cpus.effective)
# along the process's own cgroup path and every ancestor, since a parent's
# quota applies to its children, and cgroups v1 (cpu.cfs_quota_us,
# cpuset.cpus) at the mount points a container sees.
cgroup_cpu_limit <- function(root = "/sys/fs/cgroup", self = "/proc/self/cgroup") {
  limits <- c(
    vapply(cgroup_v2_dirs(root, self), cgroup_v2_limit, numeric(1)),
    cgroup_v1_limit(root)
  )
  if (all(is.na(limits))) return(NA_integer_)
  max(1L, as.integer(floor(min(limits, na.rm = TRUE))))
}

# The process's v2 cgroup directory and its ancestors up to `root`, deepest
# first. Inside a container the path is usually "/", leaving `root` alone.
cgroup_v2_dirs <- function(root, self) {
  lines <- read_lines_quietly(self)
  path <- sub("^0::", "", grep("^0::", lines, value = TRUE)[1])
  if (is.na(path)) return(root)
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  parts <- parts[nzchar(parts)]
  dirs <- vapply(rev(seq_along(parts)), function(i) {
    file.path(root, paste(parts[seq_len(i)], collapse = "/"))
  }, character(1))
  c(dirs, root)
}

cgroup_v2_limit <- function(dir) {
  quota <- NA_real_
  cpu_max <- read_lines_quietly(file.path(dir, "cpu.max"))[1]
  if (!is.na(cpu_max)) {
    fields <- strsplit(trimws(cpu_max), "\\s+")[[1]]
    if (length(fields) == 2L && fields[1] != "max") {
      quota <- as.numeric(fields[1]) / as.numeric(fields[2])
    }
  }
  cpus <- count_cpuset(read_lines_quietly(file.path(dir, "cpuset.cpus.effective"))[1])
  finite_or_na(suppressWarnings(min(quota, cpus, na.rm = TRUE)))
}

cgroup_v1_limit <- function(root) {
  quota <- NA_real_
  for (dir in file.path(root, c("cpu", "cpu,cpuacct"))) {
    q <- as.numeric(read_lines_quietly(file.path(dir, "cpu.cfs_quota_us"))[1])
    p <- as.numeric(read_lines_quietly(file.path(dir, "cpu.cfs_period_us"))[1])
    if (!is.na(q) && !is.na(p) && q > 0 && p > 0) {
      quota <- q / p
      break
    }
  }
  cpus <- count_cpuset(read_lines_quietly(file.path(root, "cpuset", "cpuset.cpus"))[1])
  finite_or_na(suppressWarnings(min(quota, cpus, na.rm = TRUE)))
}

# Number of CPUs in a cpuset list such as "0-3,6".
count_cpuset <- function(x) {
  if (is.na(x) || !nzchar(trimws(x))) return(NA_real_)
  ranges <- strsplit(strsplit(trimws(x), ",", fixed = TRUE)[[1]], "-", fixed = TRUE)
  n <- vapply(ranges, function(r) {
    r <- as.numeric(r)
    if (length(r) == 2L) r[2] - r[1] + 1 else 1
  }, numeric(1))
  if (anyNA(n)) NA_real_ else sum(n)
}

read_lines_quietly <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  out <- tryCatch(readLines(path, warn = FALSE), error = function(e) character())
  if (length(out) == 0L) NA_character_ else out
}

finite_or_na <- function(x) if (is.finite(x)) x else NA_real_
