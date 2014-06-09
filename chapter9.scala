// 1. Write a Scala code snippet that reverses the lines in a file (making 
//    the last line the first one, and so on).
import io.Source
val source = Source.fromFile("chapter9.scala")
val lines = source.getLines.toArray
val revLines = lines.reverse

// 2. Write a Scala program that reads a file with tabs, replaces each tab
//    with spaces so that tab stops are at n-column boundaries, and writes
//    the result to the same file.
import io.Source
import java.io.PrintWriter
object Main extends App {
    val source = Source.fromFile("tabs.txt")
    val lines = source.mkString
    var i = 0
    source.close
    println(lines)

    //input: a string, an int representing the n-column bounds for tab-stops
    //output: a string with tabs replaced with spaces such that tabs end at
    //        nColumn indexes
    def tabReplace(line: String, nColumn: Int) = {
        var output = line
        while(i < output.length) {
            if(output(i) == '\t') {
                println("Found a tab!")
                val toAdd = i % nColumn
                output = output.take(i-1) + (" " + " " * toAdd) + output.drop(i)
            }
            i += 1
        }
        output
    }
    val replaced = tabReplace(lines, 4)

    val out = new PrintWriter("tabs.txt")
    for(char <- replaced) { out.print(char) }
    out.close
}

// 3. Write a Scala code snippet that reads a file and prints all words with
//    more thatn 12 characters to the console. Extra credit if you can do this
//    in a single line.
val longWords = for(a <- io.Source.fromFile("chapter9.scala").mkString.split(' ') if a.length >= 12) yield a

// 4. Write a Scala program that reads a text file containing only floating-
//    point numbers. Print the sum, average, maximum, and minimum of the
//    numbers in the file.
import io.Source
object Main extends App{
    val source = Source.fromFile("doubles.txt")

    val tokenized = source.mkString.split("\\s+")
    val doublized = for(num <- tokenized) yield num.toDouble

    val sum = doublized.sum
    val average = sum / doublized.size
    val max = doublized.max
    val min = doublized.min

    println("Sum: " + sum + " Avg: " + average + "Max " + max + "Min " + min)
}

// 5. Write a Scala program that writes the powers of 2 and their reciprocals
//    to a file, with the exponent ranging from 0 to 20. Line up the columns.
import java.io.PrintWriter
import math._
val out = new PrintWriter("twoPower.txt")
for(i <- 0 to 20) {
    val power: String = pow(2, i).toString
    val recip: Double = pow(2, -i)
    out.println(power + " " * (15 - power.toString.length) + recip)
}
out.close

// 6. Make a regular expression searching for quoted strings "like this, maybe
//    with \" or \\" in a Java or C++ program. Write a Scala program that
//    prints out all such strings in a source file.
import java.io.PrintWriter
def printQuotes(input: String) {
    val regPattern = """["'].+?["']""".r
    val quotes = regPattern.findAllIn(input)
    val out = new PrintWriter("quotes.txt")
    for(quote <- quotes) out.println(quote)
    out.close
}

// 7. Write a Scala program that reads a text file and prints all tokens in the
//    file that are not floating-point numbers. Use a regular expression.
import io.Source
object Main extends App{
    val source = Source.fromFile("someFloats.txt")
    val contents = source.mkString
    val regPattern = "[0-9]+?\\.[0-9]+".r
    val nonFloats = regPattern.findAllIn(contents).toArray.mkString(", ")
    println(nonFloats)
    source.close
}



































