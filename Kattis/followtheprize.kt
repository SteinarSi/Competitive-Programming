fun main() {
    readln()
    var r = readln().toInt()
    for (i in (1..readln().toInt())) {
        val (a,b) = readln().split(' ').map { it.toInt() }
        if (r == a) r = b
        else if (r == b) r = a
    }
    println(r)
}
