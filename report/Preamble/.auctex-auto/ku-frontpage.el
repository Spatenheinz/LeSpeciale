(TeX-add-style-hook
 "ku-frontpage"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("textpos" "absolute")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "babel"
    "eso-pic"
    "graphicx"
    "times"
    "ifthen"
    "textpos"
    "hyperref"
    "lettrine")
   (TeX-add-symbols
    '("kupdfsetup" 3)
    '("frontpageimage" 1)
    '("repo" 1)
    '("advisor" 1)
    '("subtitle" 1)
    '("assignment" 1)
    "KUSTYLE"
    "KULANG"
    "KUFACULTY"
    "KUCOLOR"
    "AUTHOR"
    "TITLE"
    "DATE"
    "ASSIGNMENT"
    "SUBTITLE"
    "ADVISOR"
    "REPO"
    "FRONTPAGEIMAGE"
    "KUbold"
    "KUsemibold"
    "maketitle"))
 :latex)

