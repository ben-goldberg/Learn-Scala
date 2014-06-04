// 1. Write a code snippet that sets a to an array of n random integers between 0 and n.
def randomArray(n: Int) = {
    var a = new Array[Int](n)
    for(i <- 0 until a.length) {a(i) = Random.nextInt(n); println(a(i))}
    a
}

// 2. Write a loop that swaps adjacent elements of an array 'a' of integers
for(i <- 0 until (a.length, 2) if i != a.length-1){
    a(i) = a(i) + a(i+1)
    a(i+1) = a(i) - a(i+1)
    a(i) = a(i) - a(i+1)
}

// 3. Repeat the preceding assignment, but produce a new array. Use for/yield
for(i <- 0 until a.length) yield {
    if(i%2 == 1) a(i-1)
    else
        if(i < a.length - 1) a(i+1)
        else a(i)
}

// 4. Given an array of integers, produce a new array that contains all
//    positive values of the original array, in their original order,
//    followed by all values that are zero or negative, in their original order
def positiveNegativeSort(a: Array[Int]) = {
    var positive = for(i <- 0 until a.length if a(i) > 0) yield a(i)
    var negative = for(i <- 0 until a.length if a(i) <= 0) yield a(i)
    positive ++= negative
    positive
}

// 5. How do you compute the average of an Array[Double]
val average = someArray.sum / someArray.length

// 6. How do you rearrange the elements ofan Array[Int] so that they appear in
//    reverse sorted order? How do you do the same with an ArrayBuffer[Int]?
var reverseSorted = someArray.sorted.reverse

// 7. Write a code snippet that produces all values from an array with
//    duplicates removed?
var noDuplicates = someArray.distinct

// 8. Rewrite the example at the end of Section 3.4, “Transforming Arrays,” on
//    page 32. Collect indexes of the negative elements, reverse the sequence,
//    drop the last index, and call a.remove(i) for each index. Compare the 
//    efficiency of this approach with the two approaches in Section 3.4.
def removeAllButFirstNegative(a: ArrayBuffer[Int]) = {
    var negIndexes = for(i <- 0 until a.length if a(i) <= 0) yield i
    negIndexes = negIndexes.tail.reverse
    for(i <- negIndexes) a.remove(i)
    a
}

// 9. Make a collection of all time zones returned by 
//    java.util.TimeZone.getAvailableIDs that are in America. Strip off the 
//    “America/” preﬁx and sort the result.
val timezones = java.util.TimeZone.getAvailableIDs
val usaTimezones = for(zone <- timezones if zone.contains("America/")) yield {
    zone.stripPrefix("America/")
}

// 10.
val flavors = SystemFlavorMap.getDefaultFlavorMap().asInstanceOf[SystemFlavorMap]
var someVal = flavors.getNativesForFlavor(DataFlavor.imageFlavor)




