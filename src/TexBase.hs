{-|
Module      :TexBase
Stability   :experimental

Basic combinators for creating tex output.
-}

module TexBase (
    -- * Standard file structure
    texHeader, texFooter,
    -- * Common commands
    operator,
    scriptsize, footnotesize, small,
    textbf, textrm, textsc, textit,
    smalltextrm, smalltextit,
    boldmath,
    overline,
    ensuremath,
    grey,
    -- * Common environments
    math,
    fit,
    texSubscript, texIntSubscript,
    texVerbatim,
    -- * Other
    texableGreekLetters,
    escape,
) where

import qualified Data.Set as Set

--import GHC.Unicode (isAlphaNum, isDigit)

--import Common

----------------------------------------------------------------------------------------------------
--
--class Tex a where
--    tex :: a -> String

--instance Tex Int where
--    tex = show

----------------------------------------------------------------------------------------------------

-- | Create inline math: wraps with @$@.
math :: String -> String
math s = "$" ++ s ++ "$"

-- | Ensures the following input characters are correctly escaped: @"\\%^_<>'"@.
escape :: String -> String
escape = concatMap escape'

escape' :: Char -> String
escape' '\\' = "\\ensuremath{\\backslash}"
escape' '%' = "\\%"
escape' '^' = "\\^{}"
escape' '_' = "\\_"
escape' '<' = "\\ensuremath{<}"
escape' '>' = "\\ensuremath{>}"
escape' '\'' = "\\textquotesingle{}"
escape' c = [c]

-- | Wrap with the @operatorname@ command.
operator :: String -> String
operator s = "\\operatorname{" ++ s ++ "}"

-- | Wrap with a size annotation.
scriptsize, footnotesize, small :: String -> String
scriptsize s = "{\\scriptsize " ++ s ++ "}"
footnotesize s = "{\\footnotesize " ++ s ++ "}"
small s = "{\\small " ++ s ++ "}"

-- | Wrap with the command.
textbf, textrm, textsc, textit :: String -> String
textbf s = "\\textbf{" ++ s ++ "}"
textrm s = "\\textrm{" ++ s ++ "}"
textsc s = "\\textsc{" ++ s ++ "}"
textit s = "\\textit{" ++ s ++ "}"

-- | Wrap with the two commands.
smalltextrm, smalltextit :: String -> String
smalltextrm = textrm . small
smalltextit = textit . small

-- | Wraps with @\boldmath@ and @\unboldmath@
boldmath :: String -> String
boldmath s = "\\boldmath " ++ s ++ "\\unboldmath "

-- | Wrap with @overline@ command.
overline :: String -> String
overline s = "\\overline{" ++ s ++ "}"

-- | Wrap with @ensuremath@ command.
ensuremath :: String -> String
ensuremath s = "\\ensuremath{" ++ s ++ "}"

-- | A set of all the texable greek letters.
texableGreekLetters :: Set.Set String
texableGreekLetters = Set.fromList
    ["alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta", "iota", "kappa",
     "lambda", "mu", "nu", "xi", "pi", "rho", "sigma", "tau", "upsilon", "phi", "chi", "psi",
     "Gamma", "Delta", "Theta", "Lambda", "Xi", "Pi", "Sigma", "Upsilon", "Phi", "Psi"]

-- | Colour in grey by wrapping in grey command.
grey :: String -> String
grey s = "\\grey{" ++ s ++ "}"

----------------------------------------------------------------------------------------------------

-- | Turns the input into a subscript.
texSubscript :: String -> String
texSubscript s = "_{" ++ s ++ "}"

-- | Turns the input integer into a subscript.
texIntSubscript :: Int -> String
texIntSubscript = texSubscript . show

----------------------------------------------------------------------------------------------------

-- | Escape the input, and wrap it in bold command.
texVerbatim :: String -> String
texVerbatim = textbf . escape

-- | Wrap in the fit environment.
fit :: String -> String
fit s = "\\begin{fit}" ++ s ++ "\\end{fit}"

-- | The standard header for files we like to create.
texHeader :: String
texHeader =
    "\\documentclass[a4paper,twoside,12pt]{article}\n\
    \\\usepackage[hmargin={12mm,55mm},vmargin=10mm,footskip=7mm,asymmetric]{geometry}\n\
    \\\usepackage{amsmath}\n\
    \\\usepackage{amssymb}\n\
    \\\usepackage{tikz}\n\
    \\\usepackage{xcolor}\n\
    \\\usepackage{comment}\n\
    \\\usepackage{adjustbox}\n\
    \\\usepackage{iftex}\n\
    \\\ifPDFTeX\n\
    \\\usepackage[T1]{fontenc}\n\
    \\\usepackage[utf8]{inputenc}\n\
    \\\else\n\
    \\\usepackage[no-math]{fontspec}\n\
    \\\fi\n\
    \\\usepackage{libertine}\n\
    \\n\
    \\\makeatletter\n\
    \\\let\\_\\relax\n\
    \\\DeclareRobustCommand{\\_}{%\n\
    \  \\leavevmode\\vbox{%\n\
    \    \\hrule\\@width.4em\n\
    \          \\@height-.16ex\n\
    \          \\@depth\\dimexpr.16ex+.28pt\\relax}}\n\
    \\\makeatother\n\
    \\n\
    \\\newcommand\\Tstrut{\\rule{0pt}{2.4ex}}\n\
    \\\newcommand\\Bstrut{\\rule[-1.1ex]{0pt}{0pt}}\n\
    \\n\
    \\\tikzset{\n\
    \   tableaulabel/.style={draw=black!30, fill=gray!4, inner sep = 0.5mm, outer sep = 3mm, circle},\n\
    \   tableau/.style={draw=black!30, fill=gray!4, inner sep = 3mm, outer sep = 3mm, rounded corners=3mm, align=center},\n\
    \}\n\
    \\n\
    \\\definecolor{greycolour}{rgb}{0.6, 0.6, 0.6}\n\
    \\\newcommand\\grey[1]{{\\color{greycolour}{#1}}}\n\
    \\n\
    \\\setlength\\marginparwidth{45mm}\n\
    \\n\
    \\\newenvironment{fit}{\\begin{adjustbox}{max width=\\textwidth,max totalheight=\\textheight,keepaspectratio}}{\\end{adjustbox}}\n\
    \\n\
    \\n\
    \\\ifdefined\\showsteps\n\
    \  \\includecomment{steps}\n\
    \\\else\n\
    \  \\excludecomment{steps}\n\
    \\\fi\n\
    \\n\
    \\\raggedbottom\n\
    \\n\
    \\\begin{document}\
    \\n"

-- | The standard footer for our files.
texFooter :: String
texFooter = "\n\\end{document}"

