package com.prealpha.sclisp

case class LispException(msg: String) extends Exception(msg)

case class LispFunctionException(message: String) extends LispException(message)
case class LispInterpreterException(message: String) extends LispException(message)

case class NonMatchingParensException() extends LispException("Parenthesis do not match!")
case class NotAFunctionException(o: LispObject) extends LispException(o.toString+" is not a function")