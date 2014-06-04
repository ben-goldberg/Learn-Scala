def signum(input: Int) = if(x > 0) 1 else if(x < 0) -1 else 0

def newSignum(x : Int) = {
    if(x > 0)
        1
    else if(x < 0)
        -1
    else
        0
}

def recursiveProd(s:String): Long = {
    if(s.length == 1)
        s.head.toLong
    else
        s.head.toLong * recursiveProd(s.tail)
}

def recursivePower(x: Int, y: Int): Int = {
    if(y == 0)
        1
    else if(y < 0)
        1/recursivePower(x, -y)
    else if(y%2 == 1)
        x * recursivePower(x, y-1)
    else
        recursivePower(x, y/2) * recursivePower(x, y/2)
}