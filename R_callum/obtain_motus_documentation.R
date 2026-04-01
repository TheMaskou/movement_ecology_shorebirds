library(rvest)
library(purrr)
library(xml2)

# ==== Configuration ====

base_url      <- "https://motuswts.github.io/motus/"
articles_url  <- paste0(base_url, "articles/index.html")
reference_url <- paste0(base_url, "reference/index.html")
output_path   <- here::here(".claude", "motus_docs.md")

# ==== Collect URLs ====

scrape_links <- function(index_url, pattern) {
  read_html(index_url) |>
    html_elements("a") |>
    html_attr("href") |>
    na.omit() |>
    map_chr(~ url_absolute(.x, index_url)) |>
    # Strip fragment anchors
    sub("#.*", "", x = _) |>
    keep(~ grepl(pattern, .x)) |>
    # Keep only .html pages (not directories or index itself)
    keep(~ grepl("\\.html$", .x)) |>
    discard(~ grepl("index\\.html$", .x)) |>
    unique()
}

article_urls   <- scrape_links(articles_url, "motuswts\\.github\\.io/motus/articles/")
reference_urls <- scrape_links(reference_url, "motuswts\\.github\\.io/motus/reference/")

message("Found ", length(article_urls), " articles and ",
        length(reference_urls), " reference pages")

# ==== Extract Content From a Page ====

extract_page_md <- function(url) {
  tryCatch({
    page <- read_html(url)
    main <- html_element(page, "main")

    if (is.na(main)) {
      message("  No <main> element found: ", url)
      return(NULL)
    }

    # Remove noise elements
    html_elements(main, "script, style, .dont-index, nav, aside, footer") |>
      xml_remove()

    # Extract key content nodes in document order
    nodes <- html_elements(main, "h1, h2, h3, h4, p, pre, li, dt, dd")

    md_lines <- map_chr(nodes, function(node) {
      tag  <- html_name(node)
      text <- html_text2(node)

      switch(tag,
        # Shift headings down by 1 level (h1 -> ##, h2 -> ###, etc.)
        "h1" = paste0("## ", text),
        "h2" = paste0("### ", text),
        "h3" = paste0("#### ", text),
        "h4" = paste0("##### ", text),
        "p"  = text,
        "pre" = paste0("```r\n", html_text(node), "\n```"),
        "li" = paste0("- ", text),
        "dt" = paste0("**", text, "**"),
        "dd" = paste0("  ", text),
        text
      )
    })

    paste(md_lines, collapse = "\n\n")
  }, error = function(e) {
    message("  Failed: ", url, " - ", e$message)
    NULL
  })
}

# ==== Process All Pages ====

process_urls <- function(urls, label) {
  results <- imap(urls, function(url, i) {
    message(sprintf("  [%d/%d] %s", i, length(urls), basename(url)))
    md <- extract_page_md(url)
    Sys.sleep(0.5)
    md
  })
  names(results) <- basename(urls)
  # Remove failures
  compact(results)
}

message("\nProcessing articles...")
article_content <- process_urls(article_urls, "articles")

message("\nProcessing reference pages...")
reference_content <- process_urls(reference_urls, "reference")

# ==== Assemble Document ====

header <- paste0(
  "# Motus R Package Documentation\n\n",
  "> Auto-generated from ", base_url, "\n",
  "> Generated: ", Sys.Date(), "\n",
  "> Articles: ", length(article_content),
  ", Reference pages: ", length(reference_content), "\n"
)

format_section <- function(content_list) {
  imap_chr(content_list, function(md, name) {
    # Use filename (without .html) as a separator comment
    paste0("\n---\n\n", md, "\n")
  })
}

doc <- paste0(
  header,
  "\n# Articles\n",
  paste(format_section(article_content), collapse = "\n"),
  "\n\n# Function Reference\n",
  paste(format_section(reference_content), collapse = "\n")
)

# ==== Write Output ====

writeLines(doc, output_path)

file_size <- file.size(output_path)
message(sprintf(
  "\nDone! Wrote %d articles + %d reference pages to %s (%s KB)",
  length(article_content),
  length(reference_content),
  output_path,
  round(file_size / 1024, 1)
))
