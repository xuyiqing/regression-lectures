\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{syllabus}[1996/10/09
                     class file ``syllabus'' to create Syllabus]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% (C) Copyright 1995, Andrej Brodnik, ABrodnik@UWaterloo.CA. All
%% rights reserved.
%% Modified to create syllabus by Kevin Quinn 4/27/98 3:16PM
%%
%%
%%
%%
%% This is a generated file. Permission is granted to to customize the 
%% declarations in this file to serve the needs of your installation. 
%% However, no permission is granted to distribute a modified version of 
%% this file under its original name. 
%% 
%% \CharacterTable
%%  {Upper-case    \A\B\C\D\E\F\G\H\I\J\K\L\M\N\O\P\Q\R\S\T\U\V\W\X\Y\Z
%%   Lower-case    \a\b\c\d\e\f\g\h\i\j\k\l\m\n\o\p\q\r\s\t\u\v\w\x\y\z
%%   Digits        \0\1\2\3\4\5\6\7\8\9
%%   Exclamation   \!     Double quote  \"     Hash (number) \#
%%   Dollar        \$     Percent       \%     Ampersand     \&
%%   Acute accent  \'     Left paren    \(     Right paren   \)
%%   Asterisk      \*     Plus          \+     Comma         \,
%%   Minus         \-     Point         \.     Solidus       \/
%%   Colon         \:     Semicolon     \;     Less than     \<
%%   Equals        \=     Greater than  \>     Question mark \?
%%   Commercial at \@     Left bracket  \[     Backslash     \\
%%   Right bracket \]     Circumflex    \^     Underscore    \_
%%   Grave accent  \`     Left brace    \{     Vertical bar  \|
%%   Right brace   \}     Tilde         \~}
%%
%%---

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    - based on syllabus.sty by kcb@hss.caltech.edu
%    - 1995/02/07: the first version
%    - 1996/10/09: if there is no business address the field is
%                  left out
%
% User documentation: This class file only provides basic definitions
% =================== of environments, which are then used in class
% option files to instantiate entries for different disciplines. Thus,
% create your document as follows:
%
%    \documentclass[<discipline>]{syllabus}
%    \begin{document}
%      \name{Andrej Brodnik}
%      \businessAddress{First line \\ second line of bussines address}
%      \homeAddress{Again \\ multiline address \\ perhaps with phone number}
%    \begin{syllabus}
%      % here comes a real Curriculum syllabuse for particular <discipline>
%    \end{syllabus}
%    \end{document}
%
% where it is assumed that file ``syllabus<discipline>.clo'' exists and defines
% proper categories used in given discipline. For detail explanation on
% categories in different disciplines see individual ``.clo'' files.
%
% The output will have format:
%
%   o on the first page will appear a title ``Curriculum syllabuse'' (to
%     change it, see below under i18n notes -- internationalization)
%   o below will be your name
%   o below, side by side, your business and home address headed
%     by strings ``Business address'' and ``Home address''
%     respectively (to change these strings see below in i18n notes).
%   o then will follow the rest of CV as defined by ``<discipline>.clo''
%     file.
%   o the header of each but first page will include your name and the
%     page number.
%   o on the last page in the bottom right you will have the current
%     date, that is month and year (to change this, see below under
%     i18n notes).
%
%------
%
% i18n NOTES: If you are making CV for some other language, you have to
% =========== redefine:
%   - title:
%       o use command:   ``\title{<new title>}''
%       o default value: ``Curriculum syllabuse''
%   - date:   
%       o use command:   ``\today{<date})''
%       o default value: ``<current month>, <current year>'' (in English)
%   - addresses headers:
%       o use command:   ``\HeaderBusiness{<new header>}''
%                        ``\HeaderHome{<new header>}''
%       o default value: ``Business address''
%                        ``Home address''
%
%------
%
% System documentation: class ``syllabus'' is based on the class
% ===================== ``article''. It changes the title into
% <default value> (see i18n notes) and the name becomes an
% author. Individual categories, publications and references are
% implemented using ``description'' environment. 
%
%----------------------------------------

%%%%
%
% Process options and load class article:
%---
\let\@optionsToInput=\@empty
\DeclareOption*{
  \IfFileExists{syllabus\CurrentOption.clo}%
    {\edef\@optionToInput{syllabus\CurrentOption.clo}}%
    {\PassOptionsToClass{\CurrentOption}{article}}
}
\ProcessOptions
\LoadClass{article}

%%%%
%
% First all i18n definitions:
%---
\title{Preliminary Battle Plan}
\renewcommand{\today}{
  \ifcase\month\or
    January\or February\or March\or April\or May\or June\or
    July\or August\or September\or October\or November\or December\fi,
  \space\number\year}
\newcommand\HeaderBusiness[1]{\def\@businessAddressHeader{#1}}
  \HeaderBusiness{\textbf{Business Address}}
\newcommand\HeaderHome[1]{\def\@homeAddressHeader{#1}}
  \HeaderHome{\textbf{Home Address}}

%%%%
%
% Next, header definitions:
%---
\date{\relax}
\newcommand{\name}[1]{
  \renewcommand{\@author}{#1} \markright{\protect\small\@author}
}
\newcommand{\businessAddress}[1]{\def\@businessAddress{#1}}
  \businessAddress{}
\newcommand{\homeAddress}[1]{\def\@homeAddress{#1}}
  \homeAddress{}

%%%%
%
% \maketitle command, which prints out the title and the name of person
%---
\renewcommand{\maketitle}{\newpage
  \global\@topnum\z@   % Prevents figures from going at top of page.
  \begin{center}
    {\LARGE \@title}

    \medskip

    {\large \@author}
  \end{center}

  \bigskip

  \thispagestyle{plain}

  \gdef\@author{}\gdef\@title{}
}

%%%%
%
% ``syllabus'' environment:
%---
\pagestyle{empty}
\newenvironment{syllabus}{
     % first page is empty style though the following pages have on the
     % right side written the name from the \name command
  \ifx\@author\@empty\@warning{Missing name command}\fi
     % next we start to layout information. First the title and the
     % name,

  \maketitle
     % followed by both addresses,
  \begin{tabular*}{\textwidth}{@{\extracolsep{\fill}}ll@{}}
    \begin{tabular}[t]{@{}l@{}}
    \ifx\@businessAddress\@empty\mbox{}\else
       {\small \@businessAddressHeader}
    \\ \@businessAddress
    \fi
    \end{tabular}
  &
    \ifx\@homeAddress\@empty\@warning{Missing home address}%
    \else
      \begin{tabular}[t]{@{}l@{}}
         {\small \@homeAddressHeader}
      \\ \@homeAddress
      \end{tabular}
    \fi
  \end{tabular*}

  \bigskip

  \thispagestyle{empty}
}{   % quite at the bottom of last page we have a date
  \par\nopagebreak\vfill\hfill \today
}%end syllabus environment

%%%%
%
% Curriculum syllabuse consists of categories which we create using
% command:
%
%      \newcategory[The name]{The label}
%
% where <The name> is written in bold character as a small title of
% category. It appears at the left margine of a page. If <The name>
% parameter is missing, it takes the same value as <The label>, which,
% in turn is used to refer to individual category. For example
% commands:
%
%      \newcategory{Name of category}
%      \newcategory[Name of category]{Name of category}
%
% have the same result. Now, to use category:
%
%      \newcategory[Some category]{some other name}
%
% the input has form:
%
% \begin{some other name}
%   \item The first item
%   \item The second one etc.
% \end{some other name}
%
% and the category will have on the output title ``Some category''.
% Entries in each category are preceded by \item.
%
%-----
% i18n NOTE: One can use as the names of categories strings in
% ========== different languages, but the labels can be the same in
% the same language, which is useful if you have a single CV and you
% want outputs in different languages.
%---
\def\@newCategory[#1]#2{%
  \newenvironment{#2}{\medskip\pagebreak[2]\par
    \textbf{\small #1}\nopagebreak
    \begin{description}}{\end{description}\par}
}
\def\@noNameCategory#1{\@newCategory[#1]{#1}}
\def\newcategory{\@ifnextchar[{\@newCategory}{\@noNameCategory}}

%%%%
%
% Inside categories we have different ``kinds'' (such as different
% publications), which we create using command \newkind. It has the
% same parameters as \newcategory and all comments at command
% newcategory are also valid here.
%---
\def\@newKind[#1]#2{%
  \newenvironment{#2}{
    \pagebreak[2]
    \item \textbf{\small #1}\nopagebreak
      \begin{description}
  }{  \end{description}\par }
}
\def\@noNameKind#1{\@newKind[#1]{#1}}
\def\newkind{\@ifnextchar[{\@newKind}{\@noNameKind}}

%%%%
%
% There is a special category ``plaincategory'' which entries are
% simply listed without any indentation, and in particular, multiple
% references are separated by \and command. It can be used for
% references.
%---
\def\@newPlainCategory[#1]#2{%
  \newenvironment{#2}{
    \medskip\pagebreak[2]\par
    \textbf{\small #1}\nopagebreak
    \renewcommand{\and}{
             \end{tabular}
      \item[]\begin{tabular}[t]{l}
    }
    \begin{description}
    \item[] \begin{tabular}[t]{l}
  }{        \end{tabular}
    \end{description}\par
  }
}
\def\@noNamePlainCategory#1{\@newPlainCategory[#1]{#1}}
\def\newplaincategory{\@ifnextchar[{\@newPlainCategory}{\@noNamePlainCategory}}

%%%%
%
% Finally, formatting parameters and the possible option to input:
%---
\pagestyle{myheadings}
\parindent 0pt
\nofiles

\ifx\@optionToInput\@empty\relax
\else \input \@optionToInput
\fi
