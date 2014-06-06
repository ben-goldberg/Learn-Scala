// 1. Extend the following BankAccount class to a CheckingAccount class that
//    charges $1 for every deposit and withdrawal.
class BankAccount(initialBalance: Double) {
    private var balance = initialBalance
    def deposit(amount: Double) = { balance += amount; balance }
    def withdraw(amount: Double) = { balance -= amount; balance }
}
class CheckingAccount(initialBalance: Double) extends BankAccount(initialBalance) {
    override def deposit(amount: Double) = {super.deposit(amount - 1) }
    override def withdraw(amount: Double) = { super.withdraw(amount + 1) }
}
object CheckingAccountTester extends App {
    var account = new CheckingAccount(1000)
    account.deposit(100)
    account.withdraw(100)
    assert(account.withdraw(997) == 0)
}

// 2. Extend the BankAccount class of the preceding exercise into a class 
//    SavingsAccount that earns interest every month(when a method 
//    earnMonthlyInterest is called) and has three free deposits or withdrawals
//    every month. Reset the transaction count in the earnMonthlyInterest method.
class BankAccount(initialBalance: Double) {
    private var balance = initialBalance
    def deposit(amount: Double) = { balance += amount; balance }
    def withdraw(amount: Double) = { balance -= amount; balance }
}
class SavingsAccount(initialBalance: Double) extends BankAccount(initialBalance) {
    private var transactionsLeft: Int = 3
    override def deposit(amount: Double) = {
        if(transactionsLeft > 0){
            transactionsLeft = transactionsLeft - 1
            super.deposit(amount)
        }
        else {
            super.deposit(amount - 1)
        }
    }
    override def withdraw(amount: Double) = {
        if(transactionsLeft > 0){
            transactionsLeft = transactionsLeft - 1
            super.withdraw(amount)
        }
        else {
            super.withdraw(amount + 1)
        }
    }
    def earnMonthlyInterest(rate: Double) {
        transactionsLeft = 4 //we don't have access to balance, so we set to 4
        deposit(rate * deposit(0)) //and consume 1 here to get current balance
    }
}

// 4. Define an abstract class Item with methods price and description. A
//    SimpleItem is an item whose price and description are specified in the
//    constructor. Take advantage of the fact that a val can override a def. A
//    Bundle is an item that contains other items. Its price is the sum of the
//    prices in the bundle. Also provide a mechanism for adding items to the
//    bundle and a suitable description method.
abstract class Item {
    def price: Double
    def description: String
}
class SimpleItem(val price: Double, val description: String) extends Item
class Bundle(var items: List[Item]) extends Item {
    def price = items.map(_.price).sum
    def description = { 
        val descr = for(item <- items) yield item.description
        descr.mkString(", ") 
    }
    def addItem(toAdd: Item) {
        items = items ++ Array(toAdd)
    }
}

// 5. Design a class Point whose x and y coordinate values can be provided in a
// constructor. Provide a subclass LabeledPoint whose constructor takes a label
// value and x and y coordinates, such as 
// new LabeledPoint("Black Thursday", 1929, 230.07)
class Point(val x: Int, val y: Int)
class LabeledPoint(val label: String, val x: Int, val y: Int) extends Point(x,y)

// 6. Define an abstract class Shape with an abstract method centerPoint and
//    subclasses Rectangle and Circle. Provide appropriate constructors for the
//    subclassesand override the centerPoint method in each subclass.
abstract class Shape {
    def centerPoint
}
class Rectangle(val width: Int, val height: Int) extends Shape{
    override def centerPoint{
        printf("%d, %d",width/2, height/2)
    }
}
class Circle(val radius: Int) extends Shape{
    override def centerPoint {
        printf("%d, %d",radius, radius)
    }
}

// 7. Provide a class Square that extends java.awt.Rectangle and has three
//    constructors: one that constructs a square with a given corner point and
//    width, one that constructs a square with corner (0, 0) and a given width,
//    and one that constructs a square with corner (0, 0) and width 0.
class Square(x: Int, y: Int, width: Int) extends java.awt.Rectangle{
    def this(width: Int) { this(0, 0, width) }
    def this() { this(0,0,0) }
}

// 9. In the Creature class of section 8.10...
class Creature {
  def range: Int = 10
  val env: Array[Int] = new Array[Int](range)
}

class Ant extends Creature {
  override def range = 2
}






