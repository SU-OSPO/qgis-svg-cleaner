# app.R -- Shinylive app to make SVG symbols QGIS-parameterisable.
#
# For each uploaded SVG it reproduces the QGIS "dynamic SVG" edit:
#   1. Every <style>...</style> block is commented out
#   2. Each drawable shape (path/line/polyline/polygon/rect/circle/ellipse) gets
#      inline fill / stroke / stroke-width attributes. Checked attributes use the
#      QGIS placeholder form `param(<name>), <default>`; unchecked ones are written
#      as a plain literal default. The QGIS parameter names are fixed by QGIS:
#         fill         -> param(fill)
#         stroke       -> param(outline)
#         stroke-width -> param(outline-width)

library(shiny)
library(zip)

# ---- Memory / upload safeguard --------------------------------------------
options(shiny.maxRequestSize = 50 * 1024^2)  # 50 MB total per upload

# ---- Small helpers --------------------------------------------------------

# NULL/empty fallback (avoids depending on base R 4.4's %||%).
or_else <- function(x, default) {
  if (is.null(x) || length(x) == 0L || identical(x, "")) default else x
}

# Read a file as one UTF-8 string, preserving its bytes/newlines exactly so the
# edit stays a minimal diff of the original.
read_text <- function(path) {
  txt <- readChar(path, nchars = file.size(path), useBytes = TRUE)
  Encoding(txt) <- "UTF-8"
  txt
}

# Write a string back out as exact UTF-8 bytes (no added trailing newline).
write_text <- function(txt, path) {
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeBin(charToRaw(enc2utf8(txt)), con)
}

# Collapse #aabbcc -> #abc when each channel is a doubled hex digit
shorten_hex <- function(col) {
  if (grepl("^#([0-9a-fA-F])\\1([0-9a-fA-F])\\2([0-9a-fA-F])\\3$", col, perl = TRUE)) {
    sub("^#(.)\\1(.)\\2(.)\\3$", "#\\1\\2\\3", col, perl = TRUE)
  } else {
    col
  }
}

# Comment out every <style>...</style> block
comment_out_styles <- function(svg) {
  pat <- "(?s)<style[^>]*>.*?</style>"            # (?s) = dotall so . spans newlines
  m <- gregexpr(pat, svg, perl = TRUE)[[1]]
  if (m[1] == -1L) return(list(svg = svg, unsafe = FALSE))

  lens <- attr(m, "match.length")
  pieces <- character(0)
  pos <- 1L
  unsafe <- FALSE
  for (k in seq_along(m)) {
    start <- m[k]
    end <- m[k] + lens[k] - 1L
    pieces <- c(pieces, substring(svg, pos, start - 1L))   # text before this block
    block <- substring(svg, start, end)
    if (grepl("--", block, fixed = TRUE)) {
      pieces <- c(pieces, block)                           # cannot comment safely
      unsafe <- TRUE
    } else {
      pieces <- c(pieces, "<!-- ", block, " -->")
    }
    pos <- end + 1L
  }
  pieces <- c(pieces, substring(svg, pos, nchar(svg)))      # trailing text
  list(svg = paste0(pieces, collapse = ""), unsafe = unsafe)
}

# Insert the built attribute string immediately after each shape tag name. \b
# guards against prefix matches (e.g. <line vs <linearGradient).
insert_attrs <- function(svg, attrs) {
  gsub("(<(?:polyline|polygon|path|line|rect|circle|ellipse)\\b)",
       paste0("\\1 ", attrs), svg, perl = TRUE)
}

# Build the fill / stroke / stroke-width attribute string (order matches the
# reference outputs). Each is either a param() placeholder or a plain literal.
build_attrs <- function(param_fill, fill_value,
                        param_stroke, stroke_value,
                        param_sw, sw_value) {
  fill_attr <- if (param_fill) {
    sprintf('fill="param(fill), %s"', fill_value)
  } else {
    sprintf('fill="%s"', fill_value)
  }
  stroke_attr <- if (param_stroke) {
    sprintf('stroke="param(outline), %s"', stroke_value)
  } else {
    sprintf('stroke="%s"', stroke_value)
  }
  sw_attr <- if (param_sw) {
    sprintf('stroke-width="param(outline-width), %s"', sw_value)
  } else {
    sprintf('stroke-width="%s"', sw_value)
  }
  paste(fill_attr, stroke_attr, sw_attr)
}

# Full transform for one SVG string.
transform_svg <- function(svg, attrs) {
  cs <- comment_out_styles(svg)
  list(svg = insert_attrs(cs$svg, attrs), unsafe_style = cs$unsafe)
}

# Turn an edited SVG into a browser-renderable preview by substituting each
# parameter with its default value (`param(outline), #000` -> `#000`), since a
# raw param() value is not valid SVG and would render as broken/black.
preview_render <- function(svg) {
  gsub("param\\([^)]*\\),\\s*", "", svg, perl = TRUE)
}

# Wrap an SVG string in a minimal standalone HTML document for use as an iframe
# srcdoc. Each preview must render in its own document: an inlined <style> block
# (e.g. `.st0 { stroke: #000 }`) applies to the whole page, and a class selector
# outranks a presentation attribute -- so the original SVG's still-active style
# would otherwise override the inline stroke/fill on the parameterised preview
# (shared element ids could collide too). The iframe isolates the two previews.
preview_doc <- function(svg) {
  paste0(
    "<!doctype html><html><head><meta charset=\"utf-8\"><style>",
    "html,body{margin:0;height:100%;background:transparent;}",
    "svg{display:block;width:100%;height:100%;}",   # fit + centre via viewBox
    "</style></head><body>", svg, "</body></html>"
  )
}

# ---- UI -------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .svg-preview {
        border: 1px solid #ccc; border-radius: 4px; padding: 8px; height: 260px;
        display: flex; align-items: center; justify-content: center;
        background-color: #fff;
        background-image:
          linear-gradient(45deg, #eee 25%, transparent 25%),
          linear-gradient(-45deg, #eee 25%, transparent 25%),
          linear-gradient(45deg, transparent 75%, #eee 75%),
          linear-gradient(-45deg, transparent 75%, #eee 75%);
        background-size: 16px 16px;
        background-position: 0 0, 0 8px, 8px -8px, -8px 0;
      }
      .svg-preview iframe { width: 100%; height: 100%; border: 0; }
      input[type=color] { width: 60px; height: 34px; padding: 2px; vertical-align: middle; }
      .color-row { display: flex; align-items: center; gap: 8px; margin-bottom: 8px; }
      .color-row > label { display: inline-block; width: 100px; margin: 0; }
      .color-row .form-group, .color-row .checkbox { margin: 0; }
      pre.svg-code { max-height: 320px; overflow: auto; }
    ")),
    # Register every native colour input with Shiny on connect and on change, so
    # input$<id> is populated without any extra package (shinylive ships shiny.js,
    # which exposes Shiny.setInputValue and the shiny:connected event).
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        document.querySelectorAll('input[type=color]').forEach(function(el) {
          Shiny.setInputValue(el.id, el.value);
        });
      });
    "))
  ),

  titlePanel("SVG \u2192 QGIS parameterised symbols"),

  sidebarLayout(
    sidebarPanel(
      width = 4,
      fileInput(
        "svg_files", "Upload SVG file(s)",
        multiple = TRUE,
        accept = c(".svg", "image/svg+xml")
      ),

      tags$hr(),
      tags$strong("Make a QGIS parameter:"),
      checkboxInput("param_fill",   "fill  \u2192 param(fill)",            value = FALSE),
      checkboxInput("param_stroke", "stroke \u2192 param(outline)",        value = TRUE),
      checkboxInput("param_sw",     "stroke-width \u2192 param(outline-width)", value = TRUE),
      helpText("Checked = a QGIS-editable parameter. Unchecked = written as a fixed",
               "value. All three are always written into every shape."),

      tags$hr(),
      tags$strong("Defaults"),
      tags$div(
        class = "color-row",
        tags$label("Fill colour", `for` = "fill_color"),
        tags$input(type = "color", id = "fill_color", value = "#000000",
                   oninput = "Shiny.setInputValue('fill_color', this.value);"),
        # "No fill" sits inline with the fill colour picker; when ticked the fill
        # default becomes the literal "none" and the colour picker is ignored.
        checkboxInput("fill_none", "No fill", value = TRUE)
      ),
      tags$div(
        class = "color-row",
        tags$label("Stroke colour", `for` = "stroke_color"),
        tags$input(type = "color", id = "stroke_color", value = "#000000",
                   oninput = "Shiny.setInputValue('stroke_color', this.value);"),
        # "No stroke" mirrors "No fill": ticking it makes the stroke default
        # "none" and the stroke colour picker is ignored.
        checkboxInput("stroke_none", "No stroke", value = FALSE)
      ),
      numericInput("sw_value", "Stroke width default", value = 2, min = 0, step = 0.5),
      checkboxInput("short_hex", "Shorten colours (#000000 \u2192 #000)", value = TRUE),

      tags$hr(),
      uiOutput("download_ui")
    ),

    mainPanel(
      width = 8,
      uiOutput("status"),
      conditionalPanel(
        condition = "output.have_files",
        tags$h4(textOutput("preview_name", inline = TRUE)),
        fluidRow(
          column(6, tags$div(tags$em("Original"),
                             tags$div(class = "svg-preview", uiOutput("preview_orig")))),
          column(6, tags$div(tags$em("Parameters at their defaults"),
                             tags$div(class = "svg-preview", uiOutput("preview_new"))))
        ),
        tags$h5("Output SVG"),
        tags$pre(class = "svg-code", textOutput("preview_code"))
      )
    )
  )
)

# ---- Server ---------------------------------------------------------------

server <- function(input, output, session) {

  # Resolved default values, reacting to the inputs.
  fill_default <- reactive({
    if (isTRUE(input$fill_none)) {
      "none"
    } else {
      col <- or_else(input$fill_color, "#000000")
      if (isTRUE(input$short_hex)) shorten_hex(col) else col
    }
  })
  stroke_default <- reactive({
    # Mirror fill_default: "No stroke" wins, otherwise use the picked colour.
    if (isTRUE(input$stroke_none)) {
      "none"
    } else {
      col <- or_else(input$stroke_color, "#000000")
      if (isTRUE(input$short_hex)) shorten_hex(col) else col
    }
  })
  sw_default <- reactive({
    v <- or_else(input$sw_value, 2)
    # Drop a trailing ".0" so 2 prints as "2", not "2.0".
    format(v, trim = TRUE, drop0trailing = TRUE)
  })

  attrs <- reactive({
    build_attrs(
      param_fill   = isTRUE(input$param_fill),   fill_value   = fill_default(),
      param_stroke = isTRUE(input$param_stroke), stroke_value = stroke_default(),
      param_sw     = isTRUE(input$param_sw),      sw_value     = sw_default()
    )
  })

  # Read + classify the uploaded files once per upload.
  files <- reactive({
    req(input$svg_files)
    fi <- input$svg_files
    lapply(seq_len(nrow(fi)), function(i) {
      txt <- tryCatch(read_text(fi$datapath[i]), error = function(e) NA_character_)
      is_svg <- !is.na(txt) && grepl("<svg", txt, fixed = TRUE)
      list(name = fi$name[i], text = txt, ok = is_svg)
    })
  })

  # Apply the transform to every valid file.
  results <- reactive({
    fls <- files()
    a <- attrs()
    lapply(fls, function(f) {
      if (!isTRUE(f$ok)) return(c(f, list(out = NA_character_, unsafe = FALSE)))
      tr <- transform_svg(f$text, a)
      c(f, list(out = tr$svg, unsafe = tr$unsafe_style))
    })
  })

  valid_results <- reactive(Filter(function(r) isTRUE(r$ok), results()))

  output$have_files <- reactive(length(valid_results()) > 0)
  outputOptions(output, "have_files", suspendWhenHidden = FALSE)

  # ---- Status / warnings ----
  output$status <- renderUI({
    if (is.null(input$svg_files)) {
      return(helpText("Upload one or more .svg files to begin."))
    }
    all_res <- results()
    n_total <- length(all_res)
    n_ok <- length(valid_results())
    n_bad <- n_total - n_ok
    n_unsafe <- sum(vapply(valid_results(), function(r) isTRUE(r$unsafe), logical(1)))

    msgs <- list(tags$p(sprintf("%d file(s) uploaded; %d processed.", n_total, n_ok)))
    if (n_bad > 0) {
      msgs <- c(msgs, list(tags$p(style = "color:#b00;",
        sprintf("%d file(s) skipped (not recognised as SVG).", n_bad))))
    }
    if (n_unsafe > 0) {
      msgs <- c(msgs, list(tags$p(style = "color:#b07d00;",
        sprintf(paste("%d file(s) contain a <style> block with \"--\" that could not be",
                      "safely commented out (XML comments can't contain \"--\"). The",
                      "parameters were still added, but you may need to remove that",
                      "<style> block by hand for the params to take effect."), n_unsafe))))
    }
    do.call(tagList, msgs)
  })

  # ---- Preview (first valid file) ----
  first_valid <- reactive({
    vr <- valid_results()
    if (length(vr) == 0) NULL else vr[[1]]
  })

  output$preview_name <- renderText({
    f <- first_valid(); if (is.null(f)) "" else f$name
  })
  output$preview_orig <- renderUI({
    f <- first_valid(); req(f)
    # Isolated in its own iframe so this SVG's <style> can't leak onto the
    # parameterised preview (local single-user app: inlining the file is safe).
    tags$iframe(srcdoc = preview_doc(f$text))
  })
  output$preview_new <- renderUI({
    f <- first_valid(); req(f)
    # params substituted with their defaults, then isolated in its own iframe.
    tags$iframe(srcdoc = preview_doc(preview_render(f$out)))
  })
  output$preview_code <- renderText({
    f <- first_valid(); req(f)
    f$out
  })

  # ---- Download ----
  output$download_ui <- renderUI({
    req(length(valid_results()) > 0)
    downloadButton("download", "Download edited SVG(s)")
  })

  output$download <- downloadHandler(
    filename = function() {
      vr <- valid_results()
      if (length(vr) == 1) vr[[1]]$name else "svg-params.zip"
    },
    content = function(file) {
      vr <- valid_results()
      if (length(vr) == 1) {
        write_text(vr[[1]]$out, file)
        return(invisible())
      }
      # Multiple files: stage each under its original name, then zip
      tmp <- tempfile("svgparams")
      dir.create(tmp)
      on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
      withProgress(message = "Writing SVGs", value = 0, {
        n <- length(vr)
        for (i in seq_len(n)) {
          write_text(vr[[i]]$out, file.path(tmp, vr[[i]]$name))
          incProgress(1 / n)
        }
      })
      zip::zip(zipfile = file, files = list.files(tmp), root = tmp)
    }
  )
}

shinyApp(ui, server)
