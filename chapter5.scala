// 1. Improve the Counter class in Section 5.1, "Simple Classes and 
//    Parameterless Methods," on page 49 so that it doesn't turn negative at
//    Int.MaxValue.
class Counter {
    private var value = 0
    def increment() {
        if(value != Int.MaxValue) value += 1
        else println("Can't increment, at Int.MaxValue")
    }
    def current() = value
}

// 2. Write a class BankAccount with methods deposit and withdraw, and a read-only
//    property balance.
class BankAccount {
    private var balance = 0
    def balance = balance
    def deposit(value: Int){
        balance += value
    }
    def withdraw(value: Int) {
        balance -= value
    }
}

// 3. Write a class Time with read-only properties hours and minutes and a 
//    method before(other: Time): Boolean that checks whether this time comes
//    before the other. A Time object should be constructed as new 
//    Time(hrs, min), where hrs is in military time format (between 0 and 23).
class Time(val hours: Int, val minutes: Int){
    def before(other: Time): Boolean = {
        if(hours > other.hours) false
        else if(hours == other.hours && minutes >= other.minutes) false
        else true
    }
}

// 4. Reimplement the Time class from the preceding exercise so that the 
//    internal representation is the number of minutes since midnight (between
//    0 and 24 × 60 – 1). Do not change the public interface. That is, client
//    code should be unaffected by your change.
class Time(val hours: Int, val minutes: Int){
    val minutesSinceMidnight = (hours * 60) + minutes
    def before(other: Time): Boolean = {
        if(minutesSinceMidnight < other.minutesSinceMidnight) true
        else false
    }
}

// 5. Make a class Student with read-write JavaBeans properties name (of 
//    type String) and id (of type Long). What methods are generated?
class Student {
    @BeanProperty var name: String = _
    @BeanProperty var id: Long = _
}

// 6. In the Person class of Section 5.1, "Simple Classes and Paramaterless
//    Methods," on page 49, provide a primary constructor that turns negative
//    ages to 0.
class Person(var age: Int){
    if(age < 0) age = 0
}

// 7. Write a class Person with a primary constructor that accepts a string 
//    containing a first name, a space, and a last name, such as new Person(
//    “Fred Smith”). Supply read-only properties firstName and lastName. Should
//    the primary constructor parameter be a var, a val, or a plain parameter?
class Person(val fullName: String) {
    private val temp = fullName.split(" ")
    val firstName = temp(0)
    val lastName = temp(1)
}

// 8. Make a class Car with read-only properties for manufacturer, model name,
//    and model year, and a read-write property for the license plate. Supply
//    four constructors. All require the manufacturer and model name. 
//    Optionally, model year and license plate can also be specified in the 
//    constructor. If not, the model year is set to -1 and the license plate to
//    the empty string. Which constructor are you choosing as the primary 
//    constructor? Why?
class Car(val manufacturer: String,
          val modelName String,
          val modelYear: Int = -1,
          var licensePlate = "") {
}

// 10. Consider the class
//     class Employee(val name: String, var salary: Double) {
//         def this() { this("John Q. Public", 0.0) }
//     }
//     Rewrite it to use explicit fields and a default primary constructor. 
//     Which form do you prefer? Why?
class Employee(val name: String = "John Q. Public", val salary: Double = 0.0){
}

