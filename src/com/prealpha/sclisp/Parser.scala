package com.prealpha.sclisp

object Parser {

    implicit def verbose2token(from: VerboseToken):Token = new Token(from.s)

    val IntegerMatch  = "^([+\\-]?[0-9]+)$".r
    val FloatingMatch = "^([+\\-]?[0-9]+(?:(?:\\.[0-9]+)|(?:[eE][+\\-]?[0-9]+)))$".r
    val StringMatch   = "^\"(.+)\"$".r

    def parse_expr(input: Iterator[VerboseToken]): LispObject = {
        var exps: List[LispObject] = List()
        for (token <- input){
            verbose2token(token) match{
                case Token("(") => exps ::= parse_expr(input)
                case Token(")") => return LispList(exps.reverse)
                case Token(x)   => exps ::=
                    (x match{
                        case IntegerMatch(num)    => LispInteger(num.toInt)
                        case FloatingMatch(num)   => LispFloating(num.toDouble)
                        case StringMatch(str)     => LispString(str)
                        case "true"               => LispTrue
                        case "false"              => LispFalse
                        case other                => LispOperation(other)
                    })
            }
        }
        throw new NonMatchingParensException
    }

    def parse(input: Iterator[VerboseToken], isTop:Boolean = true): List[LispObject] = {
        var exps = List[LispObject]()
        for (token <- input){
            verbose2token(token) match{
                case Token("(") => exps ::= parse_expr(input)
                case Token(x)   => exps ::=
                    (x match{
                        case IntegerMatch(num)    => LispInteger(num.toInt)
                        case FloatingMatch(num)   => LispFloating(num.toDouble)
                        case StringMatch(str)     => LispString(str)
                        case "true"               => LispTrue
                        case "false"              => LispFalse
                        case other                => LispOperation(other)
                    })
            }
        }
        return exps.reverse
    }
}

object testing{
    def main(args: Array[String]){
        println(Parser.parse(Lexer.lex("(a 2 4\t\"test\" 3e12 (hi \"world\"))")))
    }
}
