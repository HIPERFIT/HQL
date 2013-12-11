\documentclass{article}
%include polycode.fmt
\usepackage{color}

\definecolor{syntax}{RGB}{0, 0, 0}
\definecolor{datatype}{RGB}{196, 6, 11}
\definecolor{class}{RGB}{168,37,39}
\definecolor{fieldname}{RGB}{0,0,162}
\definecolor{prelude}{RGB}{64,80,117}
\definecolor{numeral}{RGB}{0,0,205}
\definecolor{infixoperator}{RGB}{19, 19, 168}
\definecolor{constructor}{RGB}{196, 6, 11}
\definecolor{keyword}{RGB}{4, 58, 252}
\definecolor{special1}{RGB}{159,138,0}
\definecolor{string}{RGB}{3, 106, 7}
\definecolor{char}  {RGB}{3, 106, 7}

\newcommand{\lhsCHsyntax}[1]{\color{syntax}{{#1}}}
\newcommand{\lhsCHfunction}[1]{\color{infixoperator}{{#1}}}
\newcommand{\lhsCHinfixoperator}[1]{\color{infixoperator}{{#1}}}
\newcommand{\lhsCHprelude}[1]{\color{prelude}{\mathbf{#1}}}
\newcommand{\lhsCHkeyword}[1]{\color{keyword}{\textbf{#1}}}
\newcommand{\lhsCHconstructor}[1]{\color{constructor}{\textbf{#1}}}
\newcommand{\lhsCHtype}[1]{\color{datatype}{{#1}}}
\newcommand{\lhsCHclass}[1]{\color{class}{{#1}}}

%subst char a    	= "\color{char}\text{\tt ''" a "''}"
%subst string a  	= "\color{string}\text{\tt \char34 " a "\char34}"
%subst numeral a =  "\color{numeral}{ " a " }"

\begin{document} 

\section{Introduction}

And the definition of the following function
would totally screw up my program, so I'm not
definining it:

\section{Calendar}

\section{Currency}

\section{Instruments}

\subsection{Fixed Income}
\subsubsection{Annuity Bond}
\subsubsection{Bullet Bond}
\subsubsection{Consol Bond}
\subsubsection{Serial Bond}
\subsubsection{Zero Bond}

\subsection{Fixed Income Derivatives}

\begin{code}% this is not really code
main :: IO ()
main = print "just an example"
\end{code}

See?

This is the famous Hello world example, written in Haskell:

\begin{code} 
    main :: IO () 
    main =  putStrLn "Hello, world!" 
\end{code} 

\end{document}