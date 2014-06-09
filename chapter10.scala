/* 
1. The java.awt.Rectangle class has useful methods translate and grow that
    are unfortunately absent from classes such as java.awt.geom.Ellipse2D. In
    Scala, you can fix this problem. Define a trait RectangleLike with 
    concrete methods translate and grow. Provide any abstract methods that you
    need for the implementation, so that you can mix in the trait like this: 
    val egg = new java.awt.geom.Ellipse2D.Double( 5, 10, 20, 30) with RectangleLike
    egg.translate( 10, -10)
    egg.grow( 10, 20) 
*/
trait RectangleLike {
    self: java.awt.geom.Ellipse2D.Double =>
        def translate(dx: Int, dy: Int) = {
            self.setFrame(self.getX + dx, self.getY + dy, getWidth, getHeight)
        }
        def grow(h: Int, v: Int) {
            self.setFrame(self.getX, self.getY, getWidth + v, getHeight + h)
        }
}

/*
2. Define a class OrderedPoint by mixing scala.math.Ordered[Point] into
   java.awt.Point. Use lexicographic ordering, i.e. (x,y) < (x', y') if x < x'
   or x=x' and y < y'
*/
import java.awt.Point
class OrderedPoint(x: Int, y: Int) extends java.awt.Point(x,y) with scala.math.Ordered[Point] {
    def compare(that: Point) = {
        if(x < that.x) -10
        else if(x == that.x && y < that.y) -10
        else if(x == that.x && y == that.y) 0
        else 10
    }
}

/*
4. Provide a CryptoLogger trait that encrypts the log messages with the Caesar
   cipher. The key should be 3 by default, but it should be overridable by the
   user. Provide usage exapmles with the default key and a key of -3.
*/
trait Logger {
    def log(msg: String) = {}
}
trait ConsoleLogger extends Logger {
    override def log(msg: String) = {
        Console.println(msg)
    }
}
trait CryptoLogger extends Logger{
    val shift = 3
    override def log(msg: String) {
        super.log((for(char <- msg) yield (char + shift).toChar).mkString)
    }
}
class Something extends Logger {
    def foo() = {
        log("abcdefghijklmnop")
    }
}

/*
5. The JavaBeans specification has the notion of a property change listener, a 
   standardized way for beans to communicate changes in their properties. The 
   PropertyChangeSupport class is provided as a convenience superclass for any 
   bean that wishes to support property change listeners. Unfortunately, a class
   that already has another superclass—such as JComponent—must reimplement the 
   methods. Reimplement PropertyChangeSupport as a trait, and mix it into the
   java.awt.Point class.
*/
import java.beans.{PropertyChangeSupport, PropertyChangeListener, PropertyChangeEvent}
trait PropertyChangeSupport {
    private val support = new PropertyChangeSupport(this)
    def addPropertyChangeListener(listener: PropertyChangeListener) {
        support.addPropertyChangeListener(listener)
    }
    def addPropertyChangeListener(propertyName :String, listener: PropertyChangeListener) {
        support.addPropertyChangeListener(propertyName, listener)
    }
    def fireIndexedPropertyChange(propertyName: String, index: Int, oldValue: Boolean, newValue: Boolean) {
        support.fireIndexedPropertyChange(propertyName, index, oldValue, newValue)
    }
    // continue same pattern for remaining functions:
    // def the function, then call support.function() with same params
}

/*
10. Implement a class IterableInputStream that extends java.io.InputStream with
    the trait Iterable[Byte].
*/
trait IterableInputStream extends java.io.InputStream with Iterable[Byte] {
    class InputStreamIterator(input: IterableInputStream) extends Iterator[Byte] {
        def hasNext = {
            input.available() > 0
        }
        def next() = {
            input.read().toByte
        }
    }

    def iterator: Iterator[Byte] = new InputStreamIterator(this)
}



















