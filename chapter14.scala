/*
2. Using pattern matching, write a function swap that receives a pair of 
   integers and returns the pair with the components swapped.
*/
def swap(tup: Tuple2[Int, Int]) = {
    tup match {case (x,y) => (y,x)}
}

/*
3. Using pattern matching, write a function swap that swaps the first two 
   elements of an array provided its length is at least two.
*/
def swap(arr: Array[Int]) = {
    arr match {case Array(x,y,rest @ _*) => Array(y,x) ++ rest}
}

/*
4. Add a case class Multiple that is a subclass of the Item class. For example,
   Multiple(10, Article("Blackwell Toaster", 29.95)) describes ten toasters. Of
   course, you should be able to handle any items, such as bundles or multiples,
   in the second argument. Extend the price function to handle this new case.
*/
sealed abstract class Item
case class Multiple(num: Int, item: Item) extends Item
case class Article(description: String, price: Double) extends Item
case class Bundle(description: String, discount: Double, items: Item*) extends Item
def price(it: Item): Double = it match{
    case Multiple(num, item) => num * price(item)
    case Article(_,p) => p
    case Bundle(_, dis, rest @ _*) => rest.map(price(_)).sum - dis
}

/*
5. One can use lists to model trees that store values only in the leaves. For 
   example, the list ((3 8) 2 (5)) describes the tree
         +
        /|\
      *  2  -
     / \    |
    3  8    5
   However, some of the list elements are numbers and others are lists. In 
   Scala, you cannot have heterogeneous lists, so you have to use a List[Any].
   Write a leafSum function to compute the sum of all elements in the leaves, 
   using pattern matching to differentiate between numbers and lists.
*/
def leafSum(lst: List[Any]): Int = lst match {
    case List(x: List[Any], rest @ _*) => leafSum(x) + leafSum(rest.toList)
    case List(x: Int, rest @ _*) => x + leafSum(rest.toList)
    case _ => 0 
}

/*
6. A better way of modeling such trees is with case classes. Let’s start with 
   binary trees.
   sealed abstract class BinaryTree
   case class Leaf(value: Int) extends BinaryTree
   case class Node(left: BinaryTree, right: BinaryTree) extends BinaryTree
   Write a function to compute the sum of all elements in the leaves.
*/
sealed abstract class BinaryTree
case class Leaf(value: Int) extends BinaryTree
case class Node(left: BinaryTree, right: BinaryTree) extends BinaryTree
def binaryTreeSum(tree: BinaryTree): Int = tree match{
    case Leaf(value) => value
    case Node(left, right) => binaryTreeSum(left) + binaryTreeSum(right)
}

/*
7. Extend the tree in the preceding exercise so that each node can have an 
   arbitrary number of children, and reimplement the leafSum function. The tree
   in exercise 5 should be expressible as
   Node(Node(Leaf(3), Leaf(8)), Leaf(2), Node(Leaf(5)))
*/
sealed abstract class BinaryTree
case class Leaf(value: Int) extends BinaryTree
case class Node(leaves: BinaryTree*) extends BinaryTree
def binaryTreeSum(tree: BinaryTree): Int = tree match{
    case Leaf(value) => value
    case Node(leaves @ _*) => leaves.map(binaryTreeSum(_)).sum
}

/*
8. Extend the tree in the preceding exercise so that each non-leaf node stores
   an operator in addition to the child nodes. Then write a function eval that 
   computes the value. For example, the tree
        +
       /|\
     *  2  -
    / \    |
   3  8    5
   has value (3 × 8) + 2 + (–5) = 21.
*/
sealed abstract class BinaryTree
case class Leaf(value: Int) extends BinaryTree
case class Node(op: Char, leaves: BinaryTree*) extends BinaryTree
def eval(tree: BinaryTree): Int = tree match {
    case Leaf(value) => value
    case Node('+', leaves @ _*) => leaves.map(eval(_)).sum
    case Node('-', leaves @ _*) => -leaves.map(eval(_)).sum
    case Node('*', leaves @ _*) => leaves.map(eval(_)).product
    case Node('/', l: BinaryTree, r: BinaryTree) => eval(l) / eval(r)
}

/*
9. Write a function that computes the sum of the non-None values in a 
   List[Option[Int]]. Don’t use a match statement.
*/
def listSum(lst: List[Option[Int]]) = {
    (for(elem <- lst) yield elem.getOrElse(0)).sum
}

/*
10. Write a function that composes two functions of type 
    Double => Option[Double], yielding another function of the same type. 
    The composition should yield None if either function does. For example,
    def f(x: Double) = if (x >= 0) Some(sqrt(x)) else None
    def g(x: Double) = if (x != 1) Some(1 / (x - 1)) else None
    val h = compose(f, g)
    Then h(2) is Some(1), and h(1) and h(0) are None.
*/
def compose(func1: Double => Option[Double], func2: Double => Option[Double])={
    (x: Double) => func2(x) match {
        case Some(x) => func1(x)
        case None => None
    }
}




