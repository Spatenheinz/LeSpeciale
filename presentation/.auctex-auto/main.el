(TeX-add-style-hook
 "main"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("beamer" "10pt" "t")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-environments-local "semiverbatim")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "beamer"
    "beamer10"
    "pslatex"
    "fontspec"
    "caption"
    "subcaption"
    ""
    "amssymb"
    "amsmath"
    "semantic"
    "listings"
    "xcolor"
    "changepage"
    "ebproof")
   (TeX-add-symbols
    '("pilf" 3)
    '("judge" 4)
    '("subst" 3)
    '("contextcons" 1)
    '("synth" 2)
    '("tycheck" 2)
    '("lra" 1)
    "defeq"
    "context"
    "vdashs"
    "nilsig")
   (LaTeX-add-labels
    "fig:lfscsyntax")
   (LaTeX-add-xcolor-definecolors
    "codegreen"
    "codegray"
    "codepurple"
    "backcolour")
   (LaTeX-add-fontspec-newfontcmds
    "DejaSans")
   (LaTeX-add-listings-lstdefinestyles
    "mystyle"))
 :latex)

