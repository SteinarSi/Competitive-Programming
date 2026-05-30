import java.util.Stack

fun main() {
    for (p in 1..readln().toInt()) {
        val xs = readln().split(' ').map { it.toInt() }
        val dp = MutableList(xs.size) { false }
        val queue = Stack<Int>()
        queue.push(0)
        dp[0] = true
        while ( ! queue.empty()) {
            val u = queue.pop()
            for (v in listOf(u - xs[u], u + xs[u])) {
                if (v >= 0 && v < xs.size && !dp[v]) {
                    dp[v] = true
                    queue.push(v)
                }
            }
        }
        println("Puzzle $p is ${if (dp.last()) "" else "not "}solvable.")
    }
}
