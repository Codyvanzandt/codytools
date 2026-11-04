find_quarto <- function() {
  q <- Sys.which("quarto")
  if (nzchar(q)) return(unname(q))
  candidates <- c(Sys.getenv("QUARTO_PATH"),
                  "C:/Program Files/Positron/resources/app/quarto/bin/quarto.exe",
                  "C:/Program Files/Quarto/bin/quarto.exe")
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (!length(candidates)) stop("Can't find quarto. Install Quarto or add it to PATH.", call. = FALSE)
  candidates[1]
}

git <- function(site, ...) system2("git", c("-C", shQuote(site), ...), stdout = TRUE, stderr = TRUE)

# Render the whole site into docs/ using your local R.
build_site <- function() {
  site <- site_dir()
  old <- setwd(site); on.exit(setwd(old))
  # Quarto finds R through PATH; make sure it uses the R running this session.
  path <- Sys.getenv("PATH")
  Sys.setenv(PATH = paste(R.home("bin"), path, sep = .Platform$path.sep))
  on.exit(Sys.setenv(PATH = path), add = TRUE)
  status <- system2(find_quarto(), "render")
  if (status != 0) stop("quarto render failed; nothing was published.", call. = FALSE)
  file.create(file.path(site, "docs", ".nojekyll"))  # tell GitHub Pages to serve files as they are
  invisible(file.path(site, "docs"))
}

# Build, show what changed, then commit and push to publish.
publish_site <- function(message = NULL, confirm = interactive()) {
  site <- site_dir()
  build_site()
  changes <- git(site, "status", "--porcelain")
  if (!length(changes)) { base::message("Nothing to publish."); return(invisible(FALSE)) }
  where <- ifelse(grepl("docs/", changes, fixed = TRUE), "docs/ (built site)",
           ifelse(grepl("posts/", changes, fixed = TRUE), "posts/", "other source files"))
  base::message("About to publish:\n", paste0("  ", names(table(where)), ": ", table(where), " file(s)", collapse = "\n"))
  if (confirm && !isTRUE(utils::askYesNo("Commit and push these changes?"))) {
    base::message("Cancelled; nothing committed.")
    return(invisible(FALSE))
  }
  if (is.null(message)) message <- paste("Publish site,", format(Sys.Date()))
  git(site, "add", "-A")
  out <- git(site, "commit", "-m", shQuote(message))
  out <- c(out, git(site, "push"))
  base::message(paste(out, collapse = "\n"))
  invisible(TRUE)
}
