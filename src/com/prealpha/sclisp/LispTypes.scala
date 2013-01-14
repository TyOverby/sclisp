package com.prealpha.sclisp


sealed trait LispObject{
    val value: Any
    override def toString = value.toString
}

case class LispOperation(value: String) extends LispObject

abstract case class LispValue() extends LispObject

case class LispInteger(value: Int) extends LispValue
case class LispFloating(value: Double) extends LispValue
case class LispString(value: String) extends LispValue {
    override def toString = "\"" + value + "\""
}
case class LispList(value: List[LispObject]) extends LispObject {
    override def toString = value.mkString("[",", ","]")
}
case object LispNil extends LispList(Nil)

case object LispUnit extends LispValue{
    val value = "LISP_UNIT"
}

case class LispBoolean(value: Boolean) extends LispValue
case object LispTrue extends LispBoolean(true)
case object LispFalse extends LispBoolean(false)

abstract case class LispFunction(value:String) extends LispObject with (LispList => LispObject){
    override def toString = value
}