# _tabla_helper.R ─────────────────────────────────────────────────────────────
# tabla_fmt(): tabla elegante, numerada y con caption para
#   bookdown::gitbook  Y  bookdown::word_document2
#
# Clave técnica:
#   - Word  → flextable  (numeración nativa Word, estilo booktabs elegante)
#   - HTML  → knitr::kable(format="pipe") + kableExtra
#             "pipe" es el único formato que bookdown intercepta para asignar
#             el caption del chunk (tab.cap=) y resolver \@ref() en gitbook
#
# Uso:
#   ```{r tab-mi-tabla, tab.cap="Título de la tabla"}
#   mis_datos %>% tabla_fmt()
#   ```
# ─────────────────────────────────────────────────────────────────────────────

tabla_fmt <- function(df, digits = 3, col_names = NA) {

  fmt     <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  is_word <- !is.null(fmt) && fmt %in% c("docx", "odt")

  # Redondear columnas numéricas
  num_cols <- which(sapply(df, is.numeric))
  if (length(num_cols) > 0)
    df[num_cols] <- lapply(df[num_cols], round, digits)

  cn    <- if (identical(col_names, NA)) colnames(df) else col_names
  ncols <- ncol(df)
  align <- c("l", rep("c", max(ncols - 1L, 0L)))

  if (is_word) {
    # ── Word: flextable ───────────────────────────────────────────────────────
    library(flextable)
    library(officer)

    flextable(df) %>%
      border_remove() %>%
      hline_top(    border = fp_border(width = 1.5), part = "header") %>%
      hline_bottom( border = fp_border(width = 1.5), part = "header") %>%
      hline_bottom( border = fp_border(width = 1.5), part = "body"  ) %>%
      font(fontname = "Times New Roman", part = "all") %>%
      fontsize(size = 10, part = "all") %>%
      bold(part = "header") %>%
      align(align = "center", part = "all") %>%
      align(j = 1, align = "left", part = "body") %>%
      autofit() %>%
      set_table_properties(width = 1, layout = "autofit")

  } else {
    # ── HTML / gitbook: kable "pipe" + kableExtra ────────────────────────────
    # "pipe" format is what bookdown hooks into for tab.cap= / \@ref()
    library(knitr)
    library(kableExtra)

    knitr::kable(df,
                 format    = "pipe",
                 col.names = cn,
                 digits    = digits,
                 align     = align) %>%
      kable_styling(
        bootstrap_options = c("striped", "hover", "condensed", "responsive"),
        full_width        = FALSE,
        position          = "center",
        font_size         = 13
      ) %>%
      row_spec(0, bold = TRUE)
  }
}
