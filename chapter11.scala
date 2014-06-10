/*
3. Implement the Fraction class with operations + - * /. Normalize fractions, 
   for example turning 15/–6 into –5/2. Divide by the greatest common divisor, 
   like this:
   class Fraction(n: Int, d: Int) {
     private val num: Int = if (d == 0) 1 else n * sign(d) / gcd(n, d);
     private val den: Int = if (d == 0) 0 else d * sign(d) / gcd(n, d);
     override def toString = num + "/" + den
     def sign(a: Int) = if (a > 0) 1 else if (a < 0) -1 else 0
     def gcd(a: Int, b: Int): Int = if (b == 0) abs(a) else gcd(b, a % b)
     ...
}
*/
class Fraction(n: Int, d: Int) {
    private val num: Int = if (d == 0) 1 else n * sign(d) / gcd(n, d);
    private val den: Int = if (d == 0) 0 else d * sign(d) / gcd(n, d);
    override def toString = num + "/" + den
    def sign(a: Int) = if (a > 0) 1 else if (a < 0) -1 else 0
    def gcd(a: Int, b: Int): Int = if (b == 0) a.abs else gcd(b, a % b)
    def + (other: Fraction) = {
        new Fraction((this.num * other.den) + (this.den * other.num), this.den * other.den)
    }
    def - (other: Fraction) = {
        new Fraction((this.num * other.den) - (this.den * other.num), this.den * other.den)
    }
    def * (other: Fraction) = {
        new Fraction(this.num * other.num, this.den * other.den)
    }
    def / (other: Fraction) = {
        new Fraction(this.num * other.den, this.den * other.num)
    }
}

object Fraction {
    def apply(n: Int, d: Int) = new Fraction(n, d)
}

/*
4. Implement a class Money with fields for dollars and cents. Supply +, - 
   operators as well as comparison operators == and <. For example, 
   Money(1, 75) + Money(0, 50) == Money(2, 25) should be true. Should you also
   supply * and / operators? Why or why not?
*/
class Money(d: Int, c: Int) {
    val dollars = d + (c / 100)
    val cents = c % 100
    def +(other: Money) = {
        new Money(this.dollars + other.dollars, this.cents + other.cents)
    }
    def -(other: Money) = {
        new Money(this.dollars - other.dollars, this.cents - other.cents)
    }
    def ==(other: Money): Boolean = {
        if(this.dollars == other.dollars && this.cents == other.cents) true
        else false
    }
    def <(other: Money): Boolean = {
        if(this.dollars < other.dollars) true
        else if(this.dollars > other.dollars) false
        else {
            if(this.cents < other.cents) true
            else false
        }
    }
    def compare(other: Money) = {
        (this.dollars*100 + this.cents).compare(other.dollars*100 + other.cents)
    }
}

/*
5. Provide operators that construct an HTML table. For example,
   Table() | "Java" | "Scala" || "Gosling" | "Odersky" || "JVM" | "JVM, .NET"
   should produce
   <table><tr><td>Java</td><td>Scala</td></tr><tr><td>Gosling...
*/
class Table {
    var tableString = ""

    def |(next: String) = {
        tableString += "<td>" + next + "</td>"
        this
    }
    def ||(next: String) = {
        tableString += "</tr>\n<tr><td>" + next + "</td>"
        this
    }
    override def toString = {
        "<table>\n<tr>" + tableString + "</tr>\n</table>"
    }
}
object Table {
    def apply() = new Table()
}

/*
6. Provide a class ASCIIArt whose objects contain figures such as
    /\_/\
   ( ' ' )
   (  -  )
    | | |
   (__|__)
   Supply operators for combining two ASCIIArt figures horizontally
    /\_/\    -----
   ( ' ' )  / Hello \
   (  -  ) <  Scala |
    | | |   \ Coder /
   (__|__)    -----
   or vertically. Choose operators with appropriate precedence.
*/
class ASCIIArt(val art: Array[String]) {
    def printArt = {
        for(line <- art) println(line)
    }
    def +(other: ASCIIArt): ASCIIArt = {
        val zipped = art.zip(other.art)
        val newArt = for((f,s) <- zipped) yield f + s.toString
        ASCIIArt(newArt)
    }
    def |(other: ASCIIArt): ASCIIArt = {
        ASCIIArt(art ++ other.art)
    }
}
object ASCIIArt {
    def apply(art: Array[String]) = new ASCIIArt(art)
}

/*
7. Implement a class BitSequence that stores a sequence of 64 bits packed in a 
   Long value. Supply apply and update operators to get and set an 
   individual bit.
*/
class BitSequence(private var bits: Long) {
    def apply(position: Int): Long ={
        (bits >> position) & 1L
    }
    def update(position: Long, value: Boolean) {
        if(value){
            bits = bits | 1L << position
        }
        else {
            bits = bits & ~(1L << position)
        }
    }
}

/*
8. Provide a class Matrix—you can choose whether you want to implement 2 × 2 
   matrices, square matrices of any size, or m × n matrices. Supply operations 
   + and *. The latter should also work with scalars, for example mat * 2. A 
   single element should be accessible as mat(row, col).
*/
class Matrix(val height: Int, val width: Int) {
    private var mat = Array.ofDim[Int](height, width)
    def apply(row: Int, col: Int) = {
        mat(row)(col)
    }
    def apply(row: Int) = {
        mat(row)
    }
    def update(row: Int, col: Int, value: Int) {
        mat(row)(col) = value
    }
    override def toString = {
        var string = ""
        for(row <- mat) string += "(" + row.mkString(",") + ")\n"
        string
    }
    def + (other: Matrix) = {
        require(width == other.width && height == other.height, {
            println("matrices are different sizes")})
        var added = new Matrix(height, width)
        for(row <- 0 until height; col <- 0 until width) {
            added(row,col) = this(row,col) + other(row,col)
        }
        added
    }
    def rowProd(line: Array[Int], col: Int) = {
        var sum = 0
        for(i <- 0 until line.size) sum += line(i) * this(i, col)
        sum
    }
    def * (other: Matrix) = {
        require(width == other.height, {println("this.width =/= param height")})
        var multiplied = new Matrix(height, other.width)
        for(row <- 0 until height; col <- 0 until other.width) {
                multiplied(row, col) = other.rowProd(this(row), col)
            }
        multiplied 
    }
    def * (factor: Int) = {
        var multiplied = new Matrix(height, width)
        for(row <- 0 until height; col <- 0 until width) {
            multiplied(row,col) = mat(row)(col) * factor
        }
        multiplied
    }
}

/*
9. Define an unapply operation for the RichFile class that extracts the file 
   path, name, and extension. For example, the file /home/cay/readme.txt has 
   path /home/cay, name readme, and extension txt.
*/
object RichFile {
    def unapply(fullpath: String) = {
        var endPath = fullpath.lastIndexOf('/')
        var endName = fullpath.lastIndexOf('.')
        if(endPath > 0 && endName > 0) {
            (fullpath.substring(0,endPath), fullpath.substring(endPath+1, endName), 
                fullpath.substring(endName+1))
        }
        else None
    }
}

/*
10. Define an unapplySeq operation for the RichFile class that extracts all path
segments. For example, for the file /home/cay/readme.txt, you should produce a 
sequence of three segments: home, cay, and readme.txt.
*/
object RichFile {
    def unapplySeq(fullpath: String) = {
        fullpath.replace('.','/').trim.split('/')
    }
}



