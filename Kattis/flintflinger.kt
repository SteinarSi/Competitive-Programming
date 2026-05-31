fun main() {
    val n = readln().toInt()
    val cities = (1..n).map { readln().filter { it != '.' }.split(' ').map { it.toInt() } }
    print(
        cities
            .map { (x,y,_) -> cities.count { (x2,y2,r2) -> (x2-x)*(x2-x) + (y2-y)*(y2-y) <= r2*r2 } - 1 }
            .joinToString("\n")
    )
}
