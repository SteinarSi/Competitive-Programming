fun main() {
    var (n,m) = readln().split(' ').map { it.toLong() }
    val xs = generateSequence { readln() }
        .take(n.toInt())
        .map { row ->
            val (t,g) = row.split(' ').filter { it.isNotBlank() }.map { it.toLong() }
            Pair(t,g)
        }
        .sortedBy { 1000000000 * it.first - it.second }
        .toList()
    var r = 0L
    for ((t,g) in xs) {
        if (t > m) break
        m -= t
        r += g
    }
    println(r)
}