\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{homework}[2020/06/30 Homework LaTeX class]
\DeclareOption{12pt}{\PassOptionsToClass{\CurrentOption}{article}}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions\relax
\LoadClass{article}

\RequirePackage{comment}
\RequirePackage{graphicx}
\RequirePackage{amsthm}
\RequirePackage{appendix}
\RequirePackage{amsmath}
\RequirePackage{amssymb}
\RequirePackage{mathrsfs}
\RequirePackage{xcolor}
\RequirePackage{listings}
\RequirePackage[utf8]{inputenc}
\RequirePackage[brazil]{babel}
\RequirePackage{multicol}
\RequirePackage{float}
\RequirePackage{enumitem}
\RequirePackage{textgreek}
\RequirePackage{mathtools}% 
\RequirePackage{ifthen}
\RequirePackage{xifthen}
\RequirePackage{geometry}
\DeclarePairedDelimiter\ceil{\lceil}{\rceil}
\DeclarePairedDelimiter\floor{\lfloor}{\rfloor}

%% Pgf - Tikz
\RequirePackage{tikz}
\RequirePackage{pgfplots}
\usetikzlibrary{graphdrawing, graphs, graphs.standard, quotes, arrows.meta}

\newcommand{\vertex}{\node[vertex]}

\newcommand{\cqd}{%%
	\ensuremath{\blacksquare}
}

\newcommand{\gvertex}[4][]{%%
	\node (#2) at (#3, #4) {};
	\draw [#1] (#2) circle [radius=2pt];
}

\newcommand{\lvertex}[5][]{%%
	\node (#2) at (#3, #4) {#5};
	\draw [#1] (#2) circle [radius=2pt];
}

\newcommand{\glink}[3][gray!30]{%%
	\draw [#1] (#2) -- (#3);
}

\setlength{\parindent}{0em}
\setlength{\parskip}{1em}
\setlength{\voffset}{-1in}
\setlength{\textheight}{640pt}

\allowdisplaybreaks

\definecolor{codecolor}{rgb}{0.0, 0.0, 0.6}
\definecolor{stringcolor}{rgb}{0.0, 0.6, 0.0}
\definecolor{emphcolor}{rgb}{0.6, 0.0, 0.6}
\definecolor{commentcolor}{rgb}{0.6, 0.6, 0.6}

\newcommand{\keywordstyle}{%%
	\itshape\bfseries\color{codecolor}
}

\newcommand{\emphstyle}{%%
	\itshape\bfseries\color{emphcolor}
}

\newcommand{\stringstyle}{%%
	\itshape\bfseries\color{stringcolor}
}

\newcommand{\commentstyle}{%%
	\itshape\bfseries\color{commentcolor}
}

\newcommand{\code}[1]{%%
	{\ttfamily\color{codecolor}\bfseries #1}%%
}

\newcommand{\type}[1]{%%
	{\emphstyle #1}%%
}

\renewcommand{\vec}[1]{%%
	\ensuremath{\mathbf{#1}}
}

\renewcommand{\qedsymbol}{%%
	\ensuremath{\blacksquare}
}

\DeclareRobustCommand{\rchi}{{\mathpalette\irchi\relax}}
\newcommand{\irchi}[2]{\raisebox{\depth}{$#1\chi$}} % inner command, used by \rchi

\DeclareRobustCommand{\rsqrt}{{\mathpalette\irsqrt\relax}}
\newcommand{\irsqrt}[2]{\raisebox{\depth}{$#1\sqrt{}$}} % inner command, used by \rchi

\newcounter{algors}
\setcounter{algors}{0}

\lstdefinestyle{algorstyle}
{
	numberblanklines=true,
	tabsize=4,
	gobble=4,
	frame=single,
	stringstyle={\stringstyle},
	keywordstyle={\keywordstyle},
	morekeywords={%  
		def, lambda, seja, sejam, enquanto, para, de, cada, em, se, retorna, continua, e, ou%
	},
	emphstyle={\emphstyle},
	emph={%
	},
	commentstyle={\commentstyle},
	numbers=left,
	stepnumber=1,
	showstringspaces=false,
	basicstyle=\ttfamily,
	mathescape=true,
	breaklines=true,
	breakatwhitespace=false,
	extendedchars=true,
	inputencoding=utf8,
	morecomment=[l]{//},
	morecomment=[s]{/*}{*/},
	morestring=[b]",
	literate=
	{ã}{{\~a}}1                         {õ}{{\~o}}1 
	{Ã}{{\~A}}1                         {Õ}{{\~O}}1 
	{á}{{\'a}}1 {é}{{\'e}}1 {í}{{\'i}}1 {ó}{{\'o}}1 {ú}{{\'u}}1
	{Á}{{\'A}}1 {É}{{\'E}}1 {Í}{{\'I}}1 {Ó}{{\'O}}1 {Ú}{{\'U}}1
	{à}{{\`a}}1 {è}{{\`e}}1 {ì}{{\`i}}1 {ò}{{\`o}}1 {ù}{{\`u}}1
	{À}{{\`A}}1 {È}{{\'E}}1 {Ì}{{\`I}}1 {Ò}{{\`O}}1 {Ù}{{\`U}}1
	{ä}{{\"a}}1 {ë}{{\"e}}1 {ï}{{\"i}}1 {ö}{{\"o}}1 {ü}{{\"u}}1
	{Ä}{{\"A}}1 {Ë}{{\"E}}1 {Ï}{{\"I}}1 {Ö}{{\"O}}1 {Ü}{{\"U}}1
	{â}{{\^a}}1 {ê}{{\^e}}1 {î}{{\^i}}1 {ô}{{\^o}}1 {û}{{\^u}}1
	{Â}{{\^A}}1 {Ê}{{\^E}}1 {Î}{{\^I}}1 {Ô}{{\^O}}1 {Û}{{\^U}}1
	{œ}{{\oe}}1 {Œ}{{\OE}}1 {æ}{{\ae}}1 {Æ}{{\AE}}1 {ß}{{\ss}}1
	{ç}{{\c c}}1 {Ç}{{\c C}}1 {ø}{{\o}}1 {å}{{\r a}}1 {Å}{{\r A}}1
	{€}{{\EUR}}1 {£}{{\pounds}}1
	%%
	{~}{{{\raisebox{0.5ex}{\texttildelow}}}}1
	{^}{{{\raisebox{-0.5ex}{\textasciicircum}}}}1
	{<-}{{{$\leftarrow$}}}1
	{->}{{{$\rightarrow$}}}1
	{<->}{{{$\longleftrightarrow$}}}1
	%%
	{até}{{{\keywordstyle at{\'e}}}}3
	{senão}{{{\keywordstyle sen{\~a}o}}}5
	%%
	{\_\_}{{{\_}}}1
	,
	moredelim={[is][\emphstyle]{\_}{\_}}
}

\lstnewenvironment{algor}[1][\empty]{%%
	\ifx#1\empty
	\def\thistitle{\unskip}%
	\else
	\def\thistitle{.: #1}%
	\fi
	\stepcounter{algors}
	\lstset{style=algorstyle, title={Algoritmo \thealgors\thistitle}}
}{%%
}

\newcommand{\bft}[1]{%%
	\ensuremath{\text{\bfseries#1}}
}

\DeclareMathOperator*{\argmax}{arg\,max}
\DeclareMathOperator*{\argmin}{arg\,min}

%% Define switch structure environment
\newcommand{\ifequals}[3]{\ifthenelse{\equal{#1}{#2}}{#3}{}}
\newcommand{\case}[2]{#1 #2} % Dummy, so \renewcommand has something to overwrite...
\newenvironment{switch}[1]{\renewcommand{\case}{\ifequals{#1}}}{}

%% Wallpaper
\newcommand{\wallpaper}[1]{%%
	\tikz[remember picture,overlay] \node[opacity=0.75,inner sep=0pt] at (current page.center){\includegraphics[width=\paperwidth,height=\paperheight]{#1}};
}

%% Centered maketitle
\newcommand{\cmaketitle}[1][\empty]{%%
	\ifx#1\empty
	\def\thiswallpaper{\empty}%
	\else
	\def\thiswallpaper{\wallpaper{#1}}%
	\fi
	\topskip0pt
	\vspace*{\fill}
	\begin{center}
		{\Huge\bfseries \@title\space} \\ \vspace{30pt}
		{\huge \@author} \\ \vspace{5pt}
		{\ttfamily\Large \@register\space} \\ \vspace{20pt}
		{\Large \@date \space}
	\end{center}
	\vspace*{\fill}
	\thiswallpaper
	\pagebreak
}

%% Simple maketitle
\newcommand{\smaketitle}{%%
	\begin{center}
		{\Huge\bfseries \@title\space} \\ \vspace{30pt}
		{\huge \@author} \\ \vspace{5pt}
		{\ttfamily\Large \@register\space} \\ \vspace{20pt}
		{\Large \@date \space}
	\end{center}
}

%% Fortran
\lstdefinestyle{fortranstyle}
{
	language={[95]Fortran},
	numberblanklines=true,
	tabsize=4,
	gobble=4,
	frame=single,
	stringstyle={\stringstyle},
	keywordstyle={\keywordstyle},
	emphstyle={\emphstyle},
	emph={%
	},
	morekeywords={%  
		dabs, dsin, dcos, datan2%
	},
	commentstyle={\commentstyle},
	numbers=left,
	stepnumber=1,
	showstringspaces=false,
	basicstyle=\ttfamily,
	breaklines=true,
	breakatwhitespace=false,
	extendedchars=true,
	inputencoding=utf8,
	literate=
	{ã}{{\~a}}1                         {õ}{{\~o}}1 
	{Ã}{{\~A}}1                         {Õ}{{\~O}}1 
	{á}{{\'a}}1 {é}{{\'e}}1 {í}{{\'i}}1 {ó}{{\'o}}1 {ú}{{\'u}}1
	{Á}{{\'A}}1 {É}{{\'E}}1 {Í}{{\'I}}1 {Ó}{{\'O}}1 {Ú}{{\'U}}1
	{à}{{\`a}}1 {è}{{\`e}}1 {ì}{{\`i}}1 {ò}{{\`o}}1 {ù}{{\`u}}1
	{À}{{\`A}}1 {È}{{\'E}}1 {Ì}{{\`I}}1 {Ò}{{\`O}}1 {Ù}{{\`U}}1
	{ä}{{\"a}}1 {ë}{{\"e}}1 {ï}{{\"i}}1 {ö}{{\"o}}1 {ü}{{\"u}}1
	{Ä}{{\"A}}1 {Ë}{{\"E}}1 {Ï}{{\"I}}1 {Ö}{{\"O}}1 {Ü}{{\"U}}1
	{â}{{\^a}}1 {ê}{{\^e}}1 {î}{{\^i}}1 {ô}{{\^o}}1 {û}{{\^u}}1
	{Â}{{\^A}}1 {Ê}{{\^E}}1 {Î}{{\^I}}1 {Ô}{{\^O}}1 {Û}{{\^U}}1
	{œ}{{\oe}}1 {Œ}{{\OE}}1 {æ}{{\ae}}1 {Æ}{{\AE}}1 {ß}{{\ss}}1
	{ç}{{\c c}}1 {Ç}{{\c C}}1 {ø}{{\o}}1 {å}{{\r a}}1 {Å}{{\r A}}1
	{€}{{\EUR}}1 {£}{{\pounds}}1
	%%
	{~}{{{\raisebox{0.5ex}{\texttildelow}}}}1
	{^}{{{\raisebox{-0.5ex}{\textasciicircum}}}}1
}

\lstnewenvironment{fortran}[1][\empty]{%%
	\ifx#1\empty
	\def\thistitle{\unskip}%
	\else
	\def\thistitle{.: #1}%
	\fi
	\stepcounter{algors}
	\lstset{style=fortranstyle, title={Algoritmo \thealgors\thistitle}}
}{%%
}

%% Theorem
\newtheorem*{theorem}{Teorema}
\newtheorem*{supposition}{Suposição}

%% EOI
\endinput