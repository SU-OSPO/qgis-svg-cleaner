# app.R -- Shinylive app to make SVG symbols QGIS-parameterisable.
#
# For each uploaded SVG it reproduces the QGIS "dynamic SVG" edit:
#   1. Each drawable shape (path/line/polyline/polygon/rect/circle/ellipse) gets
#      fill / stroke / stroke-width written twice: once as an ordinary
#      presentation attribute holding a literal value, and (when desired by the user)
#      once more inside style="" as a QGIS placeholder. QGIS reads the placeholder;
#      every other renderer discards it and falls back to the presentation attribute.
#      The QGIS parameter names are fixed by QGIS:
#         fill         -> param(fill)
#         stroke       -> param(outline)
#         stroke-width -> param(outline-width)
#   2. Optionally, every other property in a <style> rule (e.g., stroke-linecap,
#      stroke-linejoin) is copied onto the elements that rule matches.
#   3. Every <style>...</style> block is commented out, but kept for reference.

library(shiny)
library(zip)

# ---- Memory / upload safeguard --------------------------------------------
options(shiny.maxRequestSize = 50 * 1024^2)  # 50 MB total per upload

# ---- Constants ------------------------------------------------------------

# Elements that get fill / stroke / stroke-width written into them.
SHAPE_TAGS <- c("polyline", "polygon", "path", "line", "rect", "circle", "ellipse")

# Properties this app writes itself from the sidebar; never promoted out of a
# <style> rule, or the stylesheet would fight the chosen defaults.
OWN_PROPS <- c("fill", "stroke", "stroke-width")

# SVG 1.1 presentation attributes: safe to write as plain XML attributes, and
# understood by every renderer. A promoted property outside this list (e.g.
# `isolation` or `mix-blend-mode`, presentation attributes only in SVG 2) goes
# into the element's style="" instead, where support is universal.
PRESENTATION_ATTRS <- c(
  "alignment-baseline", "baseline-shift", "clip", "clip-path", "clip-rule",
  "color", "color-interpolation", "color-interpolation-filters", "cursor",
  "direction", "display", "dominant-baseline", "fill-opacity", "fill-rule",
  "filter", "flood-color", "flood-opacity", "font-family", "font-size",
  "font-size-adjust", "font-stretch", "font-style", "font-variant",
  "font-weight", "image-rendering", "letter-spacing", "lighting-color",
  "marker-end", "marker-mid", "marker-start", "mask", "opacity", "overflow",
  "paint-order", "pointer-events", "shape-rendering", "stop-color",
  "stop-opacity", "stroke-dasharray", "stroke-dashoffset", "stroke-linecap",
  "stroke-linejoin", "stroke-miterlimit", "stroke-opacity", "text-anchor",
  "text-decoration", "text-rendering", "unicode-bidi", "vector-effect",
  "visibility", "white-space", "word-spacing", "writing-mode"
)

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

# ---- CSS: reading the <style> block ---------------------------------------

# Concatenated text of every live <style> block in the document.
style_css <- function(svg) {
  blocks <- style_blocks(svg)
  if (!length(blocks)) return("")
  txt <- vapply(blocks, function(s) substring(svg, s[1], s[2]), character(1))
  paste(sub("(?s)^<style[^>]*>(.*)</style>$", "\\1", txt, perl = TRUE), collapse = "\n")
}

# "fill: none; stroke-linecap: round" -> c(fill = "none", `stroke-linecap` = "round").
# Used for both CSS rule bodies and an element's own style="" attribute.
parse_decls <- function(s) {
  out <- character(0)
  if (!nzchar(s)) return(out)
  for (d in strsplit(s, ";", fixed = TRUE)[[1]]) {
    d <- trimws(d)
    if (!nzchar(d) || !grepl(":", d, fixed = TRUE)) next
    prop <- tolower(trimws(substring(d, 1L, regexpr(":", d, fixed = TRUE) - 1L)))
    val  <- trimws(sub("!\\s*important$", "", trimws(sub("^[^:]*:", "", d)), perl = TRUE))
    if (!nzchar(prop) || !nzchar(val)) next
    out[prop] <- val          # a repeated property inside one block: last wins
  }
  out
}

# Parse one simple selector -- an optional type (or `*`) followed by any number
# of .class / #id tokens. Anything with a combinator, attribute test or
# pseudo-class returns NULL: those are reported to the user rather than matched
# incorrectly.
parse_selector <- function(sel) {
  if (!grepl("^(\\*|[A-Za-z][A-Za-z0-9_-]*)?((?:[.#][A-Za-z_-][A-Za-z0-9_-]*)*)$",
             sel, perl = TRUE)) {
    return(NULL)
  }
  tag <- sub("^(\\*|[A-Za-z][A-Za-z0-9_-]*)?.*$", "\\1", sel, perl = TRUE)
  rest <- substring(sel, nchar(tag) + 1L)
  toks <- regmatches(rest, gregexpr("[.#][A-Za-z_-][A-Za-z0-9_-]*", rest, perl = TRUE))[[1]]
  if (!nzchar(tag) && !length(toks)) return(NULL)
  classes <- sub("^\\.", "", toks[startsWith(toks, ".")])
  ids <- sub("^#", "", toks[startsWith(toks, "#")])
  list(tag = tag, classes = classes, ids = ids,
       spec = 100L * length(ids) + 10L * length(classes) +
              as.integer(nzchar(tag) && tag != "*"))
}

sel_matches <- function(p, tag, classes, id) {
  if (nzchar(p$tag) && p$tag != "*" && p$tag != tag) return(FALSE)
  if (length(p$ids) && !all(p$ids == id)) return(FALSE)
  if (length(p$classes) && !all(p$classes %in% classes)) return(FALSE)
  TRUE
}

# Flatten a stylesheet into a list of (selector, declarations) rules, keeping
# document order so the cascade can be resolved later.
parse_css <- function(css) {
  css <- gsub("(?s)/\\*.*?\\*/", "", css, perl = TRUE)
  css <- gsub("<!\\[CDATA\\[|\\]\\]>", "", css, perl = TRUE)
  if (!nzchar(trimws(css))) return(list(rules = list(), unsupported = character(0)))
  # An at-rule (@media, @font-face, ...) nests braces, which the flat rule regex
  # below would mis-split. Bail out rather than promote something wrong.
  if (grepl("@", css, fixed = TRUE)) {
    return(list(rules = list(), unsupported = "at-rules such as @media / @font-face"))
  }

  blocks <- regmatches(css, gregexpr("[^{}]+\\{[^{}]*\\}", css, perl = TRUE))[[1]]
  rules <- list()
  unsupported <- character(0)
  for (b in blocks) {
    # Split on the brace by position rather than by regex: a rule body is
    # normally pretty-printed across several lines, and PCRE's "." stops at a
    # newline, so a "^...\{.*$" substitution would silently not match at all.
    brace <- regexpr("{", b, fixed = TRUE)
    decls <- parse_decls(gsub("}", "", substring(b, brace + 1L), fixed = TRUE))
    if (!length(decls)) next
    sels <- trimws(strsplit(substring(b, 1L, brace - 1L), ",", fixed = TRUE)[[1]])
    for (sel in sels) {
      if (!nzchar(sel)) next
      p <- parse_selector(sel)
      if (is.null(p)) {
        unsupported <- c(unsupported, sel)
      } else {
        rules[[length(rules) + 1L]] <- list(sel = p, decls = decls, order = length(rules))
      }
    }
  }
  list(rules = rules, unsupported = unique(unsupported))
}

# ---- XML: rewriting start tags --------------------------------------------

# Character spans holding text rather than markup: XML comments and CDATA.
comment_spans <- function(svg) {
  spans <- list()
  for (pat in c("(?s)<!--.*?-->", "(?s)<!\\[CDATA\\[.*?\\]\\]>")) {
    m <- gregexpr(pat, svg, perl = TRUE)[[1]]
    if (m[1] == -1L) next
    spans <- c(spans, Map(c, m, m + attr(m, "match.length") - 1L))
  }
  spans
}

in_protected <- function(pos, spans) {
  for (s in spans) if (pos >= s[1] && pos <= s[2]) return(TRUE)
  FALSE
}

# Spans of every <style> block that is not already inside an XML comment. A file
# this app has already processed carries its stylesheet wrapped in <!-- -->;
# matching that again would nest comments, which is not well-formed XML, and
# would re-promote rules that are already on the elements.
style_blocks <- function(svg, cspans = comment_spans(svg)) {
  m <- gregexpr("(?s)<style[^>]*>.*?</style>", svg, perl = TRUE)[[1]]
  if (m[1] == -1L) return(list())
  ends <- m + attr(m, "match.length") - 1L
  keep <- !vapply(m, in_protected, logical(1), spans = cspans)
  Map(c, m[keep], ends[keep])
}

# Character spans that must not be scanned for elements: comments, CDATA, and
# <style> bodies (CSS text, not markup).
protected_spans <- function(svg) {
  cs <- comment_spans(svg)
  c(cs, style_blocks(svg, cs))
}

# ` fill="none" class="st0"` -> c(fill = "none", class = "st0"), order preserved.
parse_attrs <- function(s) {
  pat <- "([A-Za-z_:][A-Za-z0-9_:.-]*)\\s*=\\s*(\"[^\"]*\"|'[^']*')"
  toks <- regmatches(s, gregexpr(pat, s, perl = TRUE))[[1]]
  if (!length(toks)) return(character(0))
  # (?s): an attribute value can itself span lines (a wrapped path or style).
  nm <- sub("(?s)^([A-Za-z_:][A-Za-z0-9_:.-]*)\\s*=.*$", "\\1", toks, perl = TRUE)
  vl <- sub("^[^=]*=\\s*.", "", toks, perl = TRUE)          # drop name and open quote
  structure(substring(vl, 1L, nchar(vl) - 1L), names = nm)  # drop close quote
}

format_attrs <- function(nv) {
  q <- ifelse(grepl("\"", nv, fixed = TRUE), "'", "\"")
  paste0(names(nv), "=", q, nv, q, collapse = " ")
}

# Rewrite one start tag. `spec` carries the sidebar choices; `rules` is the
# parsed stylesheet (empty when promotion is off). Tags that need no change are
# returned byte-for-byte so the edit stays a small diff.
rewrite_tag <- function(tagtxt, rules, spec) {
  inner <- substring(tagtxt, 2L, nchar(tagtxt) - 1L)
  self_close <- grepl("/\\s*$", inner)
  inner <- sub("/\\s*$", "", inner)
  # Matched, not substituted: a start tag is often wrapped across lines, and a
  # "^(name).*$" substitution would fail to match and hand back the whole tag as
  # the name -- which silently drops the element from SHAPE_TAGS.
  tag <- regmatches(inner, regexpr("^[A-Za-z_][A-Za-z0-9_:.-]*", inner, perl = TRUE))
  if (!length(tag)) return(tagtxt)
  attrs <- parse_attrs(substring(inner, nchar(tag) + 1L))

  cls <- if ("class" %in% names(attrs)) {
    strsplit(trimws(attrs[["class"]]), "\\s+")[[1]]
  } else {
    character(0)
  }
  id <- if ("id" %in% names(attrs)) attrs[["id"]] else ""

  # Resolve the cascade for this element: matching rules sorted by specificity
  # then document order, later declarations overwriting earlier ones.
  css <- character(0)
  if (length(rules)) {
    hit <- Filter(function(r) sel_matches(r$sel, tag, cls, id), rules)
    if (length(hit)) {
      hit <- hit[order(vapply(hit, function(r) r$sel$spec, integer(1)),
                       vapply(hit, function(r) r$order, integer(1)))]
      for (r in hit) for (p in names(r$decls)) css[p] <- r$decls[[p]]
    }
  }
  own_style <- parse_decls(if ("style" %in% names(attrs)) attrs[["style"]] else "")
  # The sidebar owns these; an element's own style="" outranks any rule.
  css <- css[!(names(css) %in% c(OWN_PROPS, names(own_style)))]

  is_shape <- tag %in% SHAPE_TAGS
  if (!is_shape && !length(css)) return(tagtxt)      # nothing to do here

  # On a shape the sidebar is authoritative for fill / stroke / stroke-width, so
  # drop any the element already declares inline -- otherwise they would outrank
  # the presentation attributes written below, and re-running the app on its own
  # output would stack a second copy of the param() declarations.
  if (is_shape) own_style <- own_style[!(names(own_style) %in% OWN_PROPS)]

  for (p in names(css)[names(css) %in% PRESENTATION_ATTRS]) attrs[p] <- css[[p]]
  spill <- css[!(names(css) %in% PRESENTATION_ATTRS)]

  # style="" order matters: promoted properties, then the element's own inline
  # style, then the QGIS params last. QGIS takes the last declaration it
  # understands; a spec-compliant renderer drops the param() ones as invalid and
  # falls back to what came before, and to the presentation attributes.
  style <- c(if (length(spill)) paste0(names(spill), ":", spill),
             if (length(own_style)) paste0(names(own_style), ":", own_style))

  if (is_shape) {
    attrs[["fill"]] <- spec$fill_value
    attrs[["stroke"]] <- spec$stroke_value
    attrs[["stroke-width"]] <- spec$sw_value
    # No space after the colon: QgsSvgCache::containsElemParams tests the raw
    # value with startsWith("param(") and does not trim it first, so a space
    # renders correctly but leaves the QGIS controls permanently greyed out.
    if (isTRUE(spec$param_fill)) {
      style <- c(style, paste0("fill:param(fill) ", spec$fill_value))
    }
    if (isTRUE(spec$param_stroke)) {
      style <- c(style, paste0("stroke:param(outline) ", spec$stroke_value))
    }
    # Deliberately no default: QGIS reads a stroke-width default as millimetres,
    # not user units, so `param(outline-width) 2` becomes a 2 mm stroke. With no
    # default QGIS keeps its own 0.2 mm and the width spinbox still enables.
    if (isTRUE(spec$param_sw)) style <- c(style, "stroke-width:param(outline-width)")
  }

  if (length(style)) {
    attrs[["style"]] <- paste(style, collapse = ";")
  } else {
    attrs <- attrs[names(attrs) != "style"]
  }

  attrs <- attrs[c(intersect(OWN_PROPS, names(attrs)),
                   setdiff(names(attrs), c(OWN_PROPS, "style")),
                   intersect("style", names(attrs)))]
  paste0("<", tag, if (length(attrs)) paste0(" ", format_attrs(attrs)) else "",
         if (self_close) "/" else "", ">")
}

# Walk every start tag outside a comment / CDATA / <style> body and rewrite it.
rewrite_elements <- function(svg, rules, spec) {
  spans <- protected_spans(svg)
  pat <- "<([A-Za-z_][A-Za-z0-9_:.-]*)((?:[^>\"']|\"[^\"]*\"|'[^']*')*)>"
  m <- gregexpr(pat, svg, perl = TRUE)[[1]]
  if (m[1] == -1L) return(svg)
  lens <- attr(m, "match.length")

  pieces <- character(0)
  pos <- 1L
  for (k in seq_along(m)) {
    start <- m[k]
    end <- start + lens[k] - 1L
    if (in_protected(start, spans)) next
    pieces <- c(pieces, substring(svg, pos, start - 1L),
                rewrite_tag(substring(svg, start, end), rules, spec))
    pos <- end + 1L
  }
  paste0(c(pieces, substring(svg, pos, nchar(svg))), collapse = "")
}

# Comment out every <style>...</style> block.
comment_out_styles <- function(svg) {
  blocks <- style_blocks(svg)
  if (!length(blocks)) return(list(svg = svg, unsafe = FALSE))

  pieces <- character(0)
  pos <- 1L
  unsafe <- FALSE
  for (s in blocks) {
    pieces <- c(pieces, substring(svg, pos, s[1] - 1L))     # text before this block
    block <- substring(svg, s[1], s[2])
    if (grepl("--", block, fixed = TRUE)) {
      pieces <- c(pieces, block)                            # cannot comment safely
      unsafe <- TRUE
    } else {
      pieces <- c(pieces, "<!-- ", block, " -->")
    }
    pos <- s[2] + 1L
  }
  pieces <- c(pieces, substring(svg, pos, nchar(svg)))      # trailing text
  list(svg = paste0(pieces, collapse = ""), unsafe = unsafe)
}

# Full transform for one SVG string. Promotion reads the stylesheet before it is
# commented out; protected_spans() keeps the rewrite from touching the CSS text.
transform_svg <- function(svg, spec, promote) {
  css <- if (isTRUE(promote)) {
    parse_css(style_css(svg))
  } else {
    list(rules = list(), unsupported = character(0))
  }
  cs <- comment_out_styles(rewrite_elements(svg, css$rules, spec))
  list(svg = cs$svg, unsafe_style = cs$unsafe, unsupported = css$unsupported)
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
      tags$strong("Add a QGIS parameter?"),
      helpText("Which attribute(s) should be editable within QGIS?"),
      checkboxInput("param_fill",   "fill  \u2192 param(fill)",            value = FALSE),
      checkboxInput("param_stroke", "stroke \u2192 param(outline)",        value = TRUE),
      checkboxInput("param_sw",     "stroke-width \u2192 param(outline-width)", value = TRUE),

      tags$hr(),
      tags$strong("Other <style> properties"),
      helpText("Should other properties from the <style> block (e.g., stroke-linecap,", "stroke-linejoin)",
               "be copied onto individual elements? Without this they are",
               "lost because the <style> block is commented out."),
      checkboxInput("promote_css", "Copy properties onto the individual shapes they style", value = TRUE),

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
      helpText("Used for the plain stroke-width attribute. QGIS reads this number as",
               "millimetres."),
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

  spec <- reactive({
    list(
      param_fill   = isTRUE(input$param_fill),   fill_value   = fill_default(),
      param_stroke = isTRUE(input$param_stroke), stroke_value = stroke_default(),
      param_sw     = isTRUE(input$param_sw),     sw_value     = sw_default()
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
    s <- spec()
    promote <- isTRUE(input$promote_css)
    lapply(fls, function(f) {
      if (!isTRUE(f$ok)) {
        return(c(f, list(out = NA_character_, unsafe = FALSE,
                         unsupported = character(0))))
      }
      tr <- transform_svg(f$text, s, promote)
      c(f, list(out = tr$svg, unsafe = tr$unsafe_style, unsupported = tr$unsupported))
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
    bad_sel <- unique(unlist(lapply(valid_results(), function(r) r$unsupported)))

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
    if (length(bad_sel) > 0) {
      msgs <- c(msgs, list(tags$p(style = "color:#b07d00;",
        sprintf(paste("These CSS selectors are too complex to copy onto elements, so",
                      "they were left behind: %s. Anything they set is lost once the",
                      "<style> block is commented out."),
                paste(bad_sel, collapse = ", ")))))
    }
    # QGIS turns a param default into a QColor; QColor("none") is invalid and
    # reports itself as #000000, so a "no fill" default would flood line art
    # solid black as soon as QGIS substitutes it.
    if (isTRUE(input$param_fill) && identical(fill_default(), "none")) {
      msgs <- c(msgs, list(tags$p(style = "color:#b07d00;",
        paste("param(fill) with a \"none\" default: QGIS cannot read \"none\" as a",
              "colour and falls back to solid black. Either untick \"No fill\" or",
              "untick param(fill)."))))
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
    # A raw param() is not valid SVG. Re-run the transform with every parameter
    # turned off instead.
    s <- spec()
    s$param_fill <- FALSE; s$param_stroke <- FALSE; s$param_sw <- FALSE
    tags$iframe(srcdoc = preview_doc(
      transform_svg(f$text, s, isTRUE(input$promote_css))$svg))
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
