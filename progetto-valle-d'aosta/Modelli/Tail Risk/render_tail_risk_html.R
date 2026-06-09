get_script_dir <- function() {
  frame_files <- Filter(
    Negate(is.null),
    lapply(sys.frames(), function(frame) frame$ofile)
  )

  if (length(frame_files) > 0) {
    return(dirname(normalizePath(frame_files[[length(frame_files)]], winslash = "/", mustWork = FALSE)))
  }

  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = FALSE)))
  }

  getwd()
}

script_dir <- get_script_dir()
setwd(script_dir)

md_path <- file.path(script_dir, "tail_risk_report.md")
html_path <- file.path(script_dir, "tail_risk_report.html")

if (!file.exists(md_path)) {
  stop("File markdown del report non trovato: ", md_path, call. = FALSE)
}

escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

inline_markdown_to_html <- function(x) {
  x <- escape_html(x)
  x <- gsub("\\*\\*(.+?)\\*\\*", "<strong>\\1</strong>", x, perl = TRUE)
  x <- gsub("`([^`]+)`", "<code>\\1</code>", x, perl = TRUE)
  x
}

emit_paragraph <- function(buffer) {
  if (length(buffer) == 0) {
    return(character())
  }
  text <- paste(buffer, collapse = " ")
  if (!nzchar(trimws(text))) {
    return(character())
  }
  paste0("<p>", inline_markdown_to_html(text), "</p>")
}

emit_table <- function(table_lines) {
  split_row <- function(line) {
    cells <- strsplit(trimws(line), "\\|", perl = TRUE)[[1]]
    cells <- trimws(cells)
    cells[cells != ""]
  }

  if (length(table_lines) < 2) {
    return(character())
  }

  header <- split_row(table_lines[1])
  body <- table_lines[-c(1, 2)]

  out <- c("<table>", "<thead>", "<tr>")
  out <- c(out, paste0("<th>", inline_markdown_to_html(header), "</th>"))
  out <- c(out, "</tr>", "</thead>", "<tbody>")

  for (row in body) {
    cells <- split_row(row)
    out <- c(out, "<tr>", paste0("<td>", inline_markdown_to_html(cells), "</td>"), "</tr>")
  }

  c(out, "</tbody>", "</table>")
}

lines <- readLines(md_path, warn = FALSE, encoding = "UTF-8")

html_body <- character()
paragraph_buffer <- character()
list_buffer <- character()
table_buffer <- character()
in_list <- FALSE
in_table <- FALSE

flush_paragraph <- function() {
  if (length(paragraph_buffer) > 0) {
    html_body <<- c(html_body, emit_paragraph(paragraph_buffer))
    paragraph_buffer <<- character()
  }
}

flush_list <- function() {
  if (length(list_buffer) > 0) {
    html_body <<- c(
      html_body,
      "<ul>",
      paste0("<li>", inline_markdown_to_html(list_buffer), "</li>"),
      "</ul>"
    )
    list_buffer <<- character()
    in_list <<- FALSE
  }
}

flush_table <- function() {
  if (length(table_buffer) > 0) {
    html_body <<- c(html_body, emit_table(table_buffer))
    table_buffer <<- character()
    in_table <<- FALSE
  }
}

for (line in lines) {
  trimmed <- trimws(line)

  if (!nzchar(trimmed)) {
    flush_paragraph()
    flush_list()
    flush_table()
    next
  }

  if (grepl("^\\|", trimmed)) {
    flush_paragraph()
    flush_list()
    table_buffer <- c(table_buffer, trimmed)
    in_table <- TRUE
    next
  }

  if (in_table) {
    flush_table()
  }

  if (grepl("^!\\[[^]]*\\]\\(.+\\)$", trimmed)) {
    flush_paragraph()
    flush_list()
    img_alt <- sub("^!\\[([^]]*)\\]\\(.+", "\\1", trimmed)
    img_src <- sub("^!\\[[^]]*\\]\\((.+)\\)$", "\\1", trimmed)
    html_body <- c(
      html_body,
      "<figure>",
      paste0('<img src="', img_src, '" alt="', escape_html(img_alt), '">'),
      if (nzchar(img_alt)) paste0("<figcaption>", escape_html(img_alt), "</figcaption>") else NULL,
      "</figure>"
    )
    next
  }

  if (grepl("^- ", trimmed)) {
    flush_paragraph()
    list_buffer <- c(list_buffer, sub("^- ", "", trimmed))
    in_list <- TRUE
    next
  }

  if (in_list) {
    flush_list()
  }

  if (grepl("^### ", trimmed)) {
    flush_paragraph()
    html_body <- c(html_body, paste0("<h3>", inline_markdown_to_html(sub("^### ", "", trimmed)), "</h3>"))
    next
  }

  if (grepl("^## ", trimmed)) {
    flush_paragraph()
    html_body <- c(html_body, paste0("<h2>", inline_markdown_to_html(sub("^## ", "", trimmed)), "</h2>"))
    next
  }

  if (grepl("^# ", trimmed)) {
    flush_paragraph()
    html_body <- c(html_body, paste0("<h1>", inline_markdown_to_html(sub("^# ", "", trimmed)), "</h1>"))
    next
  }

  paragraph_buffer <- c(paragraph_buffer, trimmed)
}

flush_paragraph()
flush_list()
flush_table()

html_lines <- c(
  "<!DOCTYPE html>",
  "<html lang=\"it\">",
  "<head>",
  "<meta charset=\"utf-8\">",
  "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
  "<title>Tail Risk Report</title>",
  "<style>",
  "body { font-family: Georgia, 'Times New Roman', serif; color: #1e1e1e; margin: 0; background: #f3efe7; }",
  ".page { max-width: 960px; margin: 0 auto; padding: 48px 56px 72px; background: #fffdf9; }",
  "h1, h2, h3 { font-family: 'Palatino Linotype', 'Book Antiqua', Palatino, serif; color: #4c1d0f; }",
  "h1 { font-size: 2.1rem; margin: 0 0 1rem; border-bottom: 3px solid #c2410c; padding-bottom: 0.4rem; }",
  "h2 { font-size: 1.4rem; margin-top: 2.2rem; margin-bottom: 0.8rem; }",
  "h3 { font-size: 1.1rem; margin-top: 1.6rem; margin-bottom: 0.5rem; }",
  "p, li { font-size: 0.98rem; line-height: 1.6; }",
  "ul { padding-left: 1.4rem; }",
  "table { width: 100%; border-collapse: collapse; margin: 1rem 0 1.5rem; font-size: 0.88rem; }",
  "thead th { background: #7c2d12; color: white; font-weight: 600; }",
  "th, td { border: 1px solid #d6c6b8; padding: 0.45rem 0.55rem; text-align: left; vertical-align: top; }",
  "tbody tr:nth-child(even) { background: #fcf7f2; }",
  "figure { margin: 1.4rem 0 1.8rem; }",
  "img { width: 100%; height: auto; border: 1px solid #e5d5c4; box-shadow: 0 10px 24px rgba(76, 29, 15, 0.10); }",
  "figcaption { font-size: 0.85rem; color: #6b4f3d; margin-top: 0.35rem; }",
  "code { background: #f5eee6; padding: 0.1rem 0.25rem; border-radius: 3px; }",
  "@page { size: A4; margin: 14mm; }",
  "@media print { body { background: white; } .page { max-width: none; padding: 0; background: white; } img { break-inside: avoid; } table { break-inside: auto; } tr, td, th { break-inside: avoid; } }",
  "</style>",
  "</head>",
  "<body>",
  "<main class=\"page\">",
  html_body,
  "</main>",
  "</body>",
  "</html>"
)

writeLines(html_lines, con = html_path, useBytes = TRUE)
cat("HTML report creato in Modelli/Tail Risk/tail_risk_report.html\n")