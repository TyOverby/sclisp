package com.prealpha.sclisp

import io.Source

case class VerboseToken(s1: String, lineNum: Int, charNum: Int) extends Token(s1)
case class Token(s: String){
    override def toString: String = s
}

object Lexer {
    val regex = "(?:\".+\")|[()]|[^ \t()]+".r
    def lex(source: Source):Iterator[VerboseToken] = {
        source.getLines().zipWithIndex.flatMap(lexLine)
    }
    def lex(source: String):Iterator[VerboseToken] = lex(Source.fromString(source))

    private[this]
    def lexLine(lineInfo: (String, Int)) = {
        val line = lineInfo._1
        val lineNum = lineInfo._2

        regex.findAllIn(line).matchData.map(x => VerboseToken(x.matched, lineNum, x.start))
    }
}
