fun main() {
    while (true) {
        val (x, y) = readln().split(' ').map { it.toLong() }
        if (x <= 0L || y <= 0L) break

        val xs = collatz(x).toMap()
        for ((c,j) in collatz(y)) {
            val i = xs[c]
            if (i != null) {
                println("$x needs $i steps, $y needs $j steps, they meet at $c")
                break
            }
        }
    }
}

fun collatz(x: Long): Sequence<Pair<Long,Int>> {
    return sequence {
        var c = x
        var i = 0
        yield(c to i)
        while (c != 1L) {
            c = if (c and 1L != 0L) 3 * c + 1 else c shr 1
            i += 1
            yield(c to i)
        }
    }
}
