# Where the website lives. Set once, e.g. in ~/.Rprofile:
#   options(codytools.site = "C:/Users/codya/Desktop/personal-website")
site_dir <- function() {
  dir <- getOption("codytools.site")
  if (is.null(dir) && file.exists("_quarto.yml")) dir <- getwd()
  if (is.null(dir)) stop('Set the website folder: options(codytools.site = "C:/path/to/personal-website")', call. = FALSE)
  dir <- normalizePath(dir, winslash = "/", mustWork = FALSE)
  if (!file.exists(file.path(dir, "_quarto.yml"))) stop(dir, " is not the website folder (no _quarto.yml).", call. = FALSE)
  dir
}

# "My *title*, don't panic!" -> "my-title-dont-panic"
slugify <- function(title) {
  x <- gsub("[*_`]", "", title)
  x <- gsub("['\u2019]", "", x)
  x <- tryCatch(iconv(x, to = "ASCII//TRANSLIT"), error = function(e) x)
  x <- tolower(gsub("[^A-Za-z0-9]+", "-", x))
  gsub("^-+|-+$", "", x)
}

check_slug <- function(slug) {
  if (!is.character(slug) || length(slug) != 1L || !grepl("^[a-z0-9]+(-[a-z0-9]+)*$", slug))
    stop("Slug must be lowercase words joined by hyphens, e.g. \"my-first-post\".", call. = FALSE)
  slug
}

# Create posts/<slug>/index.qmd from the site's template and open it.
# If the folder already exists (e.g. from to_post()), only index.qmd is added.
new_post <- function(title, slug = NULL, open = interactive()) {
  site <- site_dir()
  slug <- check_slug(if (is.null(slug)) slugify(title) else slug)
  dir <- file.path(site, "posts", slug)
  path <- file.path(dir, "index.qmd")
  if (file.exists(path)) stop(path, " already exists; nothing was overwritten.", call. = FALSE)
  lines <- readLines(file.path(site, "_templates", "post.qmd"), warn = FALSE, encoding = "UTF-8")
  lines[grepl("^title:", lines)] <- paste0("title: ", encodeString(title, quote = '"'))
  lines[grepl("^date:", lines)] <- paste0("date: ", format(Sys.Date(), "%Y-%m-%d"))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
  message("Created ", path)
  if (open) utils::file.edit(path)
  invisible(path)
}

# Send a result to a post. A ggplot is saved as an object so it is styled by the
# site's theme when the post renders; as = "png" saves it as drawn instead.
to_post <- function(x, slug, name = NULL, as = c("auto", "png")) {
  if (is.null(name)) name <- deparse1(substitute(x))
  as <- match.arg(as)
  site <- site_dir()
  check_slug(slug)
  if (!grepl("^[A-Za-z0-9_.-]+$", name)) stop("Pass a simple name (letters, numbers, _ . -) as the third argument.", call. = FALSE)
  dir <- file.path(site, "posts", slug)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    message("Created new folder posts/", slug, " (check the slug if that was a typo)")
  }
  if (as == "png") {
    if (!inherits(x, "ggplot")) stop("as = \"png\" needs a ggplot.", call. = FALSE)
    path <- file.path(dir, paste0(name, ".png"))
    cody_save(x, path)
  } else {
    if (inherits(x, "ggplot")) x$plot_env <- globalenv()
    path <- file.path(dir, paste0(name, ".rds"))
    saveRDS(x, path)
  }
  size <- file.size(path)
  message("Saved ", path, " (", format(structure(size, class = "object_size"), units = "auto"), ")")
  if (size > 5e6) warning("That file is large; summarise the data first or use as = \"png\".", call. = FALSE)
  invisible(path)
}

# Read a result back inside a post. The slug comes from the post's own folder.
from_post <- function(name, slug = NULL) {
  site <- site_dir()
  if (is.null(slug) && requireNamespace("knitr", quietly = TRUE)) {
    input <- knitr::current_input(dir = TRUE)
    if (!is.null(input)) slug <- basename(dirname(normalizePath(input, winslash = "/")))
  }
  files <- function(s) file.path(site, "posts", s, paste0(name, c(".rds", ".png")))
  if (is.null(slug)) {
    posts <- list.dirs(file.path(site, "posts"), recursive = FALSE, full.names = FALSE)
    slug <- posts[vapply(posts, function(s) any(file.exists(files(s))), logical(1))]
    if (length(slug) != 1L)
      stop("Can't tell which post \"", name, "\" belongs to; pass slug = \"...\".", call. = FALSE)
  }
  found <- files(slug)[file.exists(files(slug))]
  if (!length(found)) stop("No saved result \"", name, "\" in posts/", slug, ".", call. = FALSE)
  if (endsWith(found[1], ".png")) knitr::include_graphics(found[1]) else readRDS(found[1])
}
