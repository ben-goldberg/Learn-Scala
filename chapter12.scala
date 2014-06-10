/*
1. Write a function values(fun: (Int) => Int, low: Int, high: Int) that yields
   a collection of function inputs and outputs in a given range. For example, 
   values(x => x * x, -5, 5) should produce a collection of pairs 
   (-5, 25), (-4, 16), (-3, 9), . . ., (5, 25).
*/
def values(fun:(Int) => Int, low: Int, high: Int) = {
    (low to high).zip((low to high).map(fun(_)))
}

/*
2.  How do you get the largest element of an array with reduceLeft?
*/
val randomArray = Seq.fill(5)(Random.nextInt(20))
val max = randomArray.reduceLeft((x,y) => if(x>y) x else y)

/*
3. Implement the factorial function using to and reduceLeft, without a 
   loop or recursion.
*/
def reduceFac(value: Int) = {
    if(value > 0) {
        (1 to value).reduceLeft(_ * _)
    }
    else {
        -1 * (1 to math.abs(value)).reduceLeft(_ * _)
    }
}

/*
4. The previous implementation needed a special case when n < 1. Show how you
   can avoid this with foldLeft. (Look at the Scaladoc for foldLeft. It’s like
   reduceLeft, except that the first value in the chain of combined values is
   supplied in the call.)
*/
def foldFac(value: Int) = { //(value / math.abs(value)) gives 1 if val is pos, -1 if neg
    (1 to math.abs(value)).foldLeft(value / math.abs(value))(_ * _) 
}

/*
5. Write a function largest(fun: (Int) => Int, inputs: Seq[Int]) that yields
   the largest value of a function within a given sequence of inputs. For 
   example, largest(x => 10 * x - x * x, 1 to 10) should return 25. 
   Don’t use a loop or recursion.
*/
def largest(fun: (Int) => Int, inputs: Seq[Int]) = {
    fun(inputs.reduceLeft((x,y) => if(fun(x) >= fun(y)) x else y))
}

/*
6. Modify the previous function to return the input at which the output is
   largest. For example, largestAt(fun: (Int) => Int, inputs: Seq[Int]) should
   return 5. Don’t use a loop or recursion.
*/
def largestAt(fun: (Int) => Int, inputs: Seq[Int]) = {
    inputs.reduceLeft((x,y) => if(fun(x) >= fun(y)) x else y)
}

/*
7. It’s easy to get a sequence of pairs, for example
   val pairs = (1 to 10) zip (11 to 20)
   Now suppose you want to do something with such a sequence—say, add up the values. But you can’t do
   pairs.map(_ + _)
   The function _ + _ takes two Int parameters, not an (Int, Int) pair. Write a 
   function adjustToPair that receives a function of type (Int, Int) => Int and
   returns the equivalent function that operates on a pair. For example,
   adjustToPair(_ * _)((6, 7)) is 42.
   Then use this function in conjunction with map to compute the sums of the elements in pairs.
*/
def adjustToPair(f: (Int, Int) => Int) = {
    (x: (Int, Int)) => f(x._1, x._2)
}
def sumPairs(inputs: IndexedSeq[(Int,Int)]) = {
    inputs.map(adjustToPair(_+_))
}

/*
8. In Section 12.8, “Currying,” on page 149, you saw the corresponds method used
   with two arrays of strings. Make a call to corresponds that checks whether 
   the elements in an array of strings have the lengths given in an array of 
   integers.
*/
val words = Array("here","are","some","words")
val length = Array(4,3,4,5)
words.corresponds(length)(_.length == _)

/*
9. Implement corresponds without currying. Then try the call from the preceding
   exercise. What problem do you encounter?
*/
def corresponds[A,B](first: Seq[A], second: Seq[B], f: (A,B) => Boolean) = {
    (first.zip(second)).map(x => f(x._1, x._2)).count(!_) == 0
}

/*
10. Implement an unless control abstraction that works just like if, but with 
    an inverted condition. Does the first parameter need to be a call-by-name
    parameter? Do you need currying?
*/
def unless(clause: => Boolean)(block: => Unit) {
    if(!clause) {
        block
        unless(clause)(block)
    }
}








