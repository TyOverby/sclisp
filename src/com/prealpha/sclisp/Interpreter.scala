package com.prealpha.sclisp

import scala.collection.mutable.{LinkedHashMap=>Map}
import com.prealpha.sclisp.fns._
import scala.io.Source


class Environment(parent: Option[Environment] = None, v_args: List[(LispObject,LispObject)]){
    val mappings = new Map[String, LispObject]

    for(ob <- v_args){
        require(ob._1.isInstanceOf[LispOperation])
        mappings.put(ob._1.asInstanceOf[LispOperation].value, ob._2)
    }

    def this(){
        this(None, Nil)
    }


    def apply(key: String):LispObject = {
        if (mappings.contains(key)){
             mappings(key)
        }
        else if(parent.isDefined){
            parent.get.apply(key)
        }
        else{
            throw new LispInterpreterException("Environment could not be found that contains the variable \""+
                key+"\"")
        }

    }

    def define (key: String, value: LispObject): LispObject = {
        if (mappings.contains(key)){
            mappings(key) = value
            value
        }
        else if(parent.isDefined){
            parent.get.define(key, value)
        }
        else{
            throw new LispInterpreterException("Environment could not be found that contains the variable \""+
                key+"\"")
        }
    }

    def set(key: String, value: LispObject): LispObject = {
        mappings(key) = value
        value
    }
}

class GlobalEnv extends Environment{
    this.set("print", print_)
    this.set("println", print_ln)

    this.set("plus", plus)
    this.set("minus", minus)
    this.set("equals", is_equal)
    this.set("times", times)
    this.set("mod", mod)

    this.set("quote", quote)
    this.set("head", head)
    this.set("cons", cons)
    this.set("tail", tail)

    this.set("same-type", same_type)

    this.set("exit", exit)
    this.set("quit", exit)
}

object Interpreter {
    def run(node: LispObject, env: Environment): LispObject = {
        node match {
            case LispOperation(x) => env(x)
            case LispList(x) => x match {
                case LispOperation("import") :: LispString(s) :: Nil => {
                  val file = Source.fromFile("sclisp/"+s+".sclisp")
                  val r = Parser.parse(Lexer.lex(file)).map(m=>run(m,env))
                  file.close()
                  new LispList(r)
//                    new LispList(others.filter(_.isInstanceOf[LispString]).map(_.asInstanceOf[LispString]).map(_.value).flatMap(
//                        x=> {
//                            val f = Source.fromFile("sclisp/"+x + ".sclisp");
//                            val r = Parser.parse(Lexer.lex(f)).map(run(_,env))
//                            f.close()
//                            r
//                        }))


                }
                //case LispOperation("quote") :: n =>
                //    LispList(n)

                case LispOperation("if") :: test :: conseq :: alt :: Nil =>
                    run(if(run(test,env) == LispTrue) conseq else alt, env)

                case LispOperation("set!") :: LispOperation(s) :: exp :: Nil =>
                    env.set(s, run(exp,env))

                case LispOperation("define") :: LispOperation(s) :: exp :: Nil  =>
                    env.define(s,run(exp, env))

                case LispOperation("lambda") :: LispList(args) :: exp :: Nil =>{
                    new LispFunction("lambda") {
                        def apply(ar: LispList):LispObject = run(exp, new Environment(Some(env), args.zip(ar.value)))
                    }
                }

                case LispOperation("do") :: others => {
                    //val localEnv = new Environment(Some(env),Nil)
                    others.map(o=> run(o, env)).last
                }

                //case r@LispValue() :: Nil => r.head

                case _ => {
                    val exps = for (exp <- x) yield run(exp,env)
                    //System.err.println(exps.head+" on "+exps.tail.mkString("[",",","]"))
                    try{
                        val proc = exps.head.asInstanceOf[LispFunction].apply(LispList(exps.tail))
                        proc
                    }
                    catch{
                        case e: ClassCastException => throw new NotAFunctionException(exps.head)
                    }
                }
            }
            case other => other
        }
    }
}

object Runner extends App{

    def transform(l: List[LispObject]): String = {
        l.mkString(" ")
    }

    val globalEnv = new GlobalEnv
    def r(program: String):String = {
        transform(Parser.parse(Lexer.lex(program))
            .map(Interpreter.run(_,globalEnv)))
    }

    r("(import \"stdfun\")")
    while(true){
        try{

            print("\n>>  ")
            print("\n##  " + r(readLine))
        }
        catch{
            case LispException(m) => System.err.println(m)
            case e: Exception      => e.printStackTrace()
        }
    }
}
