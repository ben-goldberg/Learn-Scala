// 1. Set up a map of prices for a number of gizmos that you covet. Then 
//    produce a second map with the same keys and the prices at a 10% discount.
val gizmos = Map("widget1" -> 10, "widget2" -> 20, "widget3" -> 100)
val cheapGizmos = for((k,v) <- gizmos) yield (k, v * .9)

// 2. Write a program that reads words from a ﬁle. Use a mutable map to count how
//    often each word appears.
val in = new java.util.Scanner(new java.io.File("/Users/bgoldberg/Desktop/ch4pr2example.txt"))
var wordMap = collection.mutable.Map[String, Int]()
while (in.hasNext()){
    val word = in.next()
    val wordscore = wordMap.getOrElse(word, 0) + 1
    wordMap += (word -> wordscore)
}

// 3. Repeat the preceding exercise with an immutable map
val in = new java.util.Scanner(new java.io.File("/Users/bgoldberg/Desktop/ch4pr2example.txt"))
var wordMap = Map[String, Int]()
while (in.hasNext()){
    val word = in.next()
    val wordscore = wordMap.getOrElse(word, 0) + 1
    wordMap = wordMap + (word -> wordscore)
}

// 4. Repeat the preceding exercise with a sorted map, so that the words are 
//    printed in sorted order
val in = new java.util.Scanner(new java.io.File("/Users/bgoldberg/Desktop/ch4pr2example.txt"))
var wordMap = collection.immutable.SortedMap[String, Int]()
while (in.hasNext()){
    val word = in.next()
    val wordscore = wordMap.getOrElse(word, 0) + 1
    wordMap = wordMap + (word -> wordscore)
}

// 5. Repeat the preceding exercise with a java.util.TreeMap that you adapt
//    to the Scala API.
import collection.JavaConversions.mapAsScalaMap
val in = new java.util.Scanner(new java.io.File("/Users/bgoldberg/Desktop/ch4pr2example.txt"))
var wordMap: collection.mutable.Map[String, Int] = new java.util.TreeMap[String, Int]
while (in.hasNext()){
    val word = in.next()
    val wordscore = wordMap.getOrElse(word, 0) + 1
    wordMap = wordMap + (word -> wordscore)
}

// 6. Deﬁne a linked hash map that maps “Monday” to java.util.Calendar.MONDAY, 
//    and similarly for the other weekdays
import java.util.Calendar
var linkedMap = new collection.mutable.LinkedHashMap[String, Int]
linkedMap += ("Monday" -> Calendar.MONDAY)
linkedMap += ("Tuesday" -> Calendar.TUESDAY)
linkedMap += ("Wednesday" -> Calendar.WEDNESDAY)
linkedMap += ("Thursday" -> Calendar.THURSDAY)
linkedMap += ("Friday" -> Calendar.FRIDAY)
linkedMap += ("Saturday" -> Calendar.SATURDAY)
linkedMap += ("Sunday" -> Calendar.SUNDAY)
for(day <- linkedMap) println(day)

// 7. Print a table of all java properties
import collection.JavaConversions.propertiesAsScalaMap
val props: collection.Map[String, String] = System.getProperties
var maxLength: Int = 0
for((a, v) <- props){
    if(a.length > maxLength) {
        maxLength = a.length
    }
}
for((a, v) <- props){
    println(a + " " * (maxLength + 2 - a.length) + "|" + " " + v)
}

// 8. Write a function minmax(values: Array[Int]) that returns a pair
//    containing the smallest and largest values in the array.
def minmax(values: Array[Int]) = {
    (values.min, values.max)
}

// 9. Write a function lteqgt(values: Array[Int], v: Int) that returns a triple
// containing the counts of values less than v, equal to v, and greater than v.
def lteqgt(values: Array[Int], v: Int) = {
    var less, equal, greater = 0
    for(element <- values){
        if(element < v) less += 1
        else if(element == v) equal += 1
        else greater += 1
    }
    (less, equal, greater)
}

// 10. What happens when you zip together two strings, such as "Hello".zip("World")?
//scala> "Hello".zip("World")
//res3: scala.collection.immutable.IndexedSeq[(Char, Char)] = Vector((H,W), (e,o), (l,r), (l,l), (o,d))




