#lang scribble/manual

@title{Myers Stacks}

@author[@author+email["Maria Jenkins" "mjenkins@eng.utah.edu"]]
@author[@author+email["Michael Adams" "adamsmd@cs.utah.edu"]]

@section{Original Myers Stack}

Myers stacks are an alternative structure to cons lists. They behave like cons lists except that accessing an arbitrary element takes only logarithmic time.
The original Myers stack as described in Myers paper @cite["IP83"] uses a skew binary number system, we use a different but equivalent construction to the skew binary number system.

@section{Improved Myers Stack}




@(bibliography
  (bib-entry #:key "IP83"
             #:author "Eugene Myers"
             #:title "An applicative random access stack"
             #:location "Information Procesing Letters"
             #:date "1983"))
