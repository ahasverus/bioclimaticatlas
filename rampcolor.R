rampcolors <- data.frame(
  palette          = rownames(brewer.pal.info),
  maxcolors        = brewer.pal.info[ , "maxcolors"],
  stringsAsFactors = FALSE
)

css  <- ""
html <- ""
html <- paste0(html,
  "<h5>Select a color palette:</h5>\n",
  "<div class=\"color-picker\">\n",
  "  <div id=\"grad\">\n",
  "    <div id=\"color-sel\" class=\"YlGnBu\"></div>\n",
  "    <div id=\"color-arrow\">\n",
  "      <i class=\"fa fa-caret-down\"></i>\n",
  "    </div>\n",
  "  </div>\n",
  "  <div id=\"menu\">\n"
)


for (i in 1:nrow(rampcolors)) {

  cols <- brewer.pal(name = rampcolors[i, "palette"], n = rampcolors[i, "maxcolors"])
  cols <- tolower(cols)

  css  <- paste0(css,
    ".", rampcolors[i, "palette"], " {\n",
    "  background: ", cols[length(cols)], ";\n",
    "  background: linear-gradient(to right, ", paste0(cols, collapse = ", "), ");\n",
    "}\n\n"
  )

  html <- paste0(html,
    "    <div class=\"", rampcolors[i, "palette"], " color-list\"></div>\n"
  )

  cols <- cols[length(cols):1]

  css  <- paste0(css,
    ".", rampcolors[i, "palette"], "-rev {\n",
    "  background: ", cols[length(cols)], ";\n",
    "  background: linear-gradient(to right, ", paste0(cols, collapse = ", "), ");\n",
    "}\n\n"
  )

  html <- paste0(html,
    "    <div class=\"", rampcolors[i, "palette"], "-rev color-list\"></div>\n"
  )
}

html <- paste0(html,
  "  </div>\n",
  "</div>\n"
)


cat(html, file = "includes/color-picker.html", append = FALSE)
cat(css, file = "css/color-gradients.css", append = FALSE)
