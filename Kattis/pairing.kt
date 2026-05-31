import kotlin.math.min

fun main() {
    var (a,b,c,d) = readln().split(' ').map { it.toInt() }
    a %= 2
    c %= 2
    val s = min(b, d)
    b -= s
    d -= s
    var r = 0
    if (b > 0 && a > 0) {
        b--
        a--
        r++
    }
    if (d > 0 && c > 0) {
        d--
        c--
        r++
    }
    if (b > 0 && c > 0) {
        b--
        c--
        r++
    }
    if (d > 0 && a > 0) {
        d--
        a--
        r++
    }
    println(r + a + b + c + d)
}
