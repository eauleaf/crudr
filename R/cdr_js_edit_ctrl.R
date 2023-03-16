#' Allows key use to navigate table and edit cells
#'
#' @return javascript callback for DT::datatable()
#' @export
#'
#' @examples crudr::cdr_js_edit_ctrl()
cdr_js_edit_ctrl <- function(){

  cat('\n--Running: crudr::cdr_js_edit_ctrl()\n')

  js <- c(

    "// If you're on a cell and key 'Enter' or 'F2', enter edit mode",
    "table.on('key', function(e, datatable, key, cell){",
    "console.log(key);",
    "  if ( key === 113 || key === 13 ){",
    "    $(cell.node()).trigger('dblclick.dt');",
    "  }",
    "});",
    "table.on('keydown', 'td', function(e){",
    "console.log(e.which);",
    "// If you're editing a cell and press 'Tab, F2, up or down' to enter the new data",
    "  if (e.target.localName == 'input' && [9,38,40,113].indexOf(e.keyCode) > -1){",
    "    $(e.target).trigger('blur');",
    "// If you're editing a cell and press 'left, right, home, or end', perform those jumps within the cell text",
    "  } else if (e.target.localName == 'input' && [35,36,37,39].indexOf(e.keyCode) > -1) {",
    "    e.stopPropagation();",
    "// If you're editing a cell and then press 'escape', remove any User changes",
    "  } else if (e.target.localName == 'input' && e.keyCode === 27) {",
    "    e.stopPropagation();",
    "    var prior_val = table.cell( this ).data();",
    "    $(e.target).val( prior_val );",
    "    $(e.target).trigger('blur');",
    "// If you're editing a cell and press 'enter', mimic the downarrow",
    "  } else if (e.target.localName == 'input' && e.keyCode === 13 ) {",
    "    e.stopPropagation();",
    "    $(e.target).trigger('blur');",
    "  }",
    "});"
  )

  # print(js)

  DT::JS(js)

}
