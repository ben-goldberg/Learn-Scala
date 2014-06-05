// 3. Write a package random with functions nextInt(): Int,
//    nextDouble(): Double, and setSeed(seed: Int): Unit. To generate random
//    numbers, use the linear congruential generator
//    next = (previous * a + b) mod 2^n
//    where a=1664525, b=1013904223, n=32, and the initial value of previous
//    is seed.
package random{
    package object random{
        var seed = 1
        def nextInt(): Int = {
            seed = (seed * 1664525 + 1013904223) % 2^32; seed
        }
        def nextDouble(): Double = {
            seed = (seed * 1664525 + 1013904223) % 2^32; seed
        }
        def setSeed(newSeed: Int): Unit {
            seed = newSeed 
        }
    }
}

// 6. Write a program that copies all elements from a Java hash map into a
//    Scala hash map. Use imports to rename both classes. 
import java.util.{HashMap => JavaHashMap}
import collection.mutable.{HashMap => ScalaHashMap}
var javaMap = new JavaHashMap[Any, Any]
javaMap.put("hello",1)
javaMap.put("world", 2)
var scalaMap = new ScalaHashMap[Any, Any]
for(key <- javaMap.keySet.toArray(new Array[String](0))) {
    scalaMap += (key -> javaMap.get(key))
}

// 7. In the preceding exercise, move all imports into the innermost scope
//    possible
import java.util.{HashMap => JavaHashMap}
var javaMap = new JavaHashMap[Any, Any]
javaMap.put("hello",1)
javaMap.put("world", 2)
import collection.mutable.{HashMap => ScalaHashMap}
var scalaMap = new ScalaHashMap[Any, Any]
for(key <- javaMap.keySet.toArray(new Array[String](0))) {
    scalaMap += (key -> javaMap.get(key))
}

// 9. Write a program that imports the java.lang.System class, reads the user
//   name from the user.name system property, reads a password from the Console
//   object, and prints a message to the standard error stream if the password
//   is not "secret". Otherwise, print a greeting to the standard output stream.
//   Do not use any other imports, and do not use any qualified names 
//   (with dots).
import java.lang.System._
val username = getProperty("user.name")
val password = readLine()
if(password != "secret") {
    Console.err.println("Goodbye, World!")
}
else {
    println("Hello, World!")
}






