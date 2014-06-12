/*
1. Write a function that, given a string, produces a map of the indexes of all
   characters. For example, indexes("Mississippi") should return a map 
   associating 'M' with the set {0}, 'i' with the set {1, 4, 7, 10}, and so on.
   Use a mutable map of characters to mutable sets. How can you ensure that the
   set is sorted?
*/
import collection.mutable._
def indexes(str: String) = {
    var associated = new HashMap[Char, LinkedHashSet[Int]]()
    for(char <- str.distinct) {
        val indexSet = new LinkedHashSet[Int]
        for(i <- 0 until str.length if str(i) == char) indexSet += i
        associated(char) = indexSet
    }
    associated
}

/*
2. Repeat the preceding exercise, using an immutable map of characters to lists.
*/
import collection.mutable._
def indexes(str: String) = {
    var associated = new collection.immutable.HashMap[Char, List[Int]]()
    for(char <- str.distinct) {
        val indexList = (for(i <- 0 until str.length if str(i) == char) yield i).toList
        associated = associated + (char -> indexList)
    }
    associated
}

/*
3. Write a function that removes all zeroes from a linked list of integers.
*/
import collection.mutable._
def removeZeroes(lst: LinkedList[Int]) = {
    lst.filter(_ != 0)
}

/*
4. Write a function that receives a collection of strings and a map from 
   strings to integers. Return a collection of integers that are values of the
   map corresponding to one of the strings in the collection. For example, given
   Array("Tom", "Fred", "Harry") and Map("Tom" -> 3, "Dick" -> 4, "Harry" -> 5),
   return Array(3, 5). Hint: Use flatMap to combine the Option values returned
   by get.
*/
import collection.immutable._
def getValue(arr: Array[String], m: Map[String, Int]) = {
    arr.flatMap(x => m.get(x))
}

/*
5. Implement a function that works just like mkString, using reduceLeft.
*/
def makeString[T](it: collection.Iterable[T], split: String = ", ") = {
    it.map(x => x.toString).reduceLeft(_ + split + _)
}

/*
6. Given a list of integers lst, what is (lst :\ List[Int]())(_ :: _)? 
   (List[Int]() /: lst)(_ :+ _)? 
   How can you modify one of them to reverse the list?
*/
(List[Int]() /: lst)((x,y) => y :: x)

/*
7. In Section 13.11, “Zipping,” on page 171, the expression (prices zip 
   quantities) map { p => p._1 * p._2 } is a bit inelegant. We can’t do 
   (prices zip quantities) map { _ * _ } because _ * _ is a function with two 
   arguments, and we need a function with one argument that is a tuple. The 
   tupled method of the Function2 object changes a function with two arguments 
   to one that takes a tuple. Apply tupled to the multiplication function so you
    can map it over the list of pairs.
*/
(prices zip quantities) map Function.tupled(_*_)

/*
8. Write a function that turns an array of Double values into a two-dimensional
   array. Pass the number of columns as a parameter. For example, with 
   Array(1, 2, 3, 4, 5, 6) and three columns, return 
   Array(Array(1, 2, 3), Array(4, 5, 6)). Use the grouped method.
*/
def arrayTo2DArray(arr: Array[Double], cols: Int) = {
    arr.grouped(cols).toArray.map(_.toArray)
}

