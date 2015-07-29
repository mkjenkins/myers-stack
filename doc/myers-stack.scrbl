#lang scribble/manual

@title{Myers Stacks}

@author[@author+email["Maria Jenkins" "mjenkins@eng.utah.edu"]]
@author[@author+email["Michael Adams" "adamsmd@cs.utah.edu"]]

@section{Original Myers Stack}

Myers stacks are an alternative structure to cons lists. They behave like cons lists except that accessing an arbitrary element takes only logarithmic time.
The original Myers stack as described in Myers paper @cite["IPL83"] uses a skew binary number system, this implementation uses a different but equivalent construction to the skew binary number system.

@defproc[(myers-stack-car [stack myers-stack?]) any/c?]{
  Returns the first element in the myers stack.
}

@defproc[(myers-stack-cdr [stack myers-stack?]) myers-stack?]{
  Returns a myers stack without the first element.
}


@defproc[(myers-stack-null? [stack myers-stack?]) boolean?]{
  Returns [#t] if the myers stack is null and [#f] otherwise.
}

@defproc[(myers-stack-pair? [stack myers-stack?]) boolean?]{
  Returns [#t] if its pair and [#f] otherwise.
}


@defproc[(myers-stack->list [x myers-stack?]) list]{
  Returns a list with the elements of the myers stack in it.
}


@defproc[(list->myers-stack [x list?]) myers-stack?]{
  Returns a myers-stack with the elements of the list.
}

@defproc[(myers-stack-cons [x any/c] [stack myers-stack?]) pair?]{
Returns a newly allocated pair whose first element is x and second element is stack.
}

@defproc[(myers-stack-drop [stack myers-stack?] [count num]) myers-stack?]{
Returns the stack after the first count elements of the stack.
}



@section{Improved Myers Stack}

This version has an improved worse case complexity of indexing.

@defproc[(myers-stack-ef-car [stack myers-stack?]) any/c?]{
  Returns the first element in the myers stack.
}

@defproc[(myers-stack-ef-cdr [stack myers-stack?]) myers-stack?]{
  Returns a myers stack without the first element.
}


@defproc[(myers-stack-ef-null? [stack myers-stack?]) boolean?]{
  Returns [#t] if the myers stack is null and [#f] otherwise.
}

@defproc[(myers-stack-ef-pair? [stack myers-stack?]) boolean?]{
  Returns [#t] if its pair and [#f] otherwise.
}


@defproc[(myers-stack-ef->list [x myers-stack?]) list]{
  Returns a list with the elements of the myers stack in it.
}


@defproc[(list->myers-stack-ef [x list?]) myers-stack?]{
  Returns a myers-stack with the elements of the list.
}

@defproc[(myers-stack-ef-cons [x any/c] [stack myers-stack?]) pair?]{
Returns a newly allocated pair whose first element is x and second element is stack.
}

@defproc[(myers-stack-ef-drop [stack myers-stack?] [count num]) myers-stack?]{
Returns the stack after the first count elements of the stack.
}




@(bibliography
  (bib-entry #:key "IPL83"
             #:author "Eugene Myers"
             #:title "An applicative random access stack"
             #:location "Information Procesing Letters"
             #:date "1983"))
