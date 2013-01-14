package com.prealpha.sclisp


object fns {
    def create(fn: LispList=> LispObject, name: String) = new LispFunction(name) {
        //val value: String = name
        def apply(v: LispList): LispObject = fn(v)
    }


    val plus = create(plus_fn, "plus")
    def plus_fn(args:LispList): LispObject = args.value match {
        case LispFloating(x) :: LispFloating(y) :: Nil => LispFloating(x+y)
        case LispFloating(x) :: LispInteger(y)  :: Nil => LispFloating(x+y)
        case LispInteger(x)  :: LispFloating(y) :: Nil => LispFloating(x+y)
        case LispInteger(x)  :: LispInteger(y)  :: Nil => LispInteger(x+y)
        case _ => throw new LispFunctionException("The two arguments to 'plus' must be either floats or integers")
    }

    val minus = create(minus_fn, "minus")
    def minus_fn(args:LispList): LispObject = args.value match {
        case LispFloating(x) :: LispFloating(y) :: Nil => LispFloating(x-y)
        case LispFloating(x) :: LispInteger(y)  :: Nil => LispFloating(x-y)
        case LispInteger(x)  :: LispFloating(y) :: Nil => LispFloating(x-y)
        case LispInteger(x)  :: LispInteger(y)  :: Nil => LispInteger(x-y)
        case _ => throw new LispFunctionException("The two arguments to 'minus' must be either floats or integers")
    }

    val times = create(times_fn, "times")
    def times_fn(args: LispList): LispObject = args.value match {
        case LispFloating(x) :: LispFloating(y) :: Nil => LispFloating(x*y)
        case LispFloating(x) :: LispInteger(y)  :: Nil => LispFloating(x*y)
        case LispInteger(x)  :: LispFloating(y) :: Nil => LispFloating(x*y)
        case LispInteger(x)  :: LispInteger(y)  :: Nil => LispInteger(x*y)
        case _ => throw new LispFunctionException("The two arguments to 'times' must be either floats or integers")
    }

    val quote = create(quote_fn, "quote")
    def quote_fn(args: LispList): LispObject = args

    val head = create(head_fn, "head")
    def head_fn(args: LispList):LispObject = args.value match{
        case LispList(l) :: Nil => l.head
        case _ => throw new LispFunctionException("Head must be called on a single list")
    }
    val tail = create(tail_fn, "tail")
    def tail_fn(args: LispList):LispObject = args.value match{
        case LispList(l) :: Nil => LispList(l.tail)
        case _ => throw new LispFunctionException("Tail must be called on a single list")
    }

    val cons = create(cons_fn, "cons")
    def cons_fn(args:LispList):LispObject = args.value match{
        case (x:LispObject) :: LispList(l) :: Nil => new LispList(x::l)
        case _ => throw new LispFunctionException("Cons takes 1 object and 1 list as parameters")
    }

    val print_ln = create(print_ln_fn, "println")
    def print_ln_fn(args: LispList): LispObject = {
        args.value.foreach(print)
        println()
        LispUnit
    }

    val print_ = create(print_fn, "print")
    def print_fn (args: LispList): LispObject = {
        args.value.foreach(print)
        LispUnit
    }

    val is_equal = create(is_equal_fn, "equals")
    def is_equal_fn (args: LispList): LispObject = {
        if(args.value.head equals args.value.tail.head) LispTrue else LispFalse
    }

    val exit = create(exit_fn, "exit")
    def exit_fn(args: LispObject):LispObject = {
        System.exit(0)
        LispUnit
    }
}