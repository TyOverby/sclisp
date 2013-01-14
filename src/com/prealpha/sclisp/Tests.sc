import com.prealpha.sclisp.{Interpreter, Parser, Lexer}

val program = "(24)\n(\"hello world\")"

val lexed   = Lexer.lex(program)
val parsed  = Parser.parse(lexed)









