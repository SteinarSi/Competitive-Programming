import kotlin.math.max
import kotlin.math.min

const val INF = 999999999

fun main() {
    val (n,m) = readln().split(' ').map(String::toInt)

    val dp = Array(m+1) { // number of pallets remaining
        IntArray(n+2) // size of the range of possible values the box limit could be
    }

    fun solve(p: Int, s: Int): Int {
        if (s <= 1) return 0
        if (p <= 0) return INF
        if (dp[p][s] == 0) {
            var best = INF
            for (t in 1..s) {
                best = min(best,1 + max(
                    solve(p-1,t),
                    solve(p,s-t))
                )
            }
            dp[p][s] = best
        }
        return dp[p][s]
    }
    
    val ans = solve(m,n+1)
    var from = INF
    var to = 0
    for (t in 1..n) {
        val res = 1 + max(solve(m-1,t), solve(m,n+1-t))
        if (res == ans) {
            from = min(from,t)
            to = max(to,t)
        }
    }
    println("$ans ${if (from == to) from else "$from-$to"}")
}
