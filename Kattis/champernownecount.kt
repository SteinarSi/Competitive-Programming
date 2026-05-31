fun main() {
    val (n,k) = readln().split(' ').map { it.toLong() }
    var ret: Long = 0
    var curr: Long = 0
    for (i in 1..n) {
        val m = if      (i < 10)     10L
                else if (i < 100)    100
                else if (i < 1000)   1000
                else if (i < 10000)  10000
                else if (i < 100000) 100000
                else                 1000000
        curr = ((curr * m) + i)  % k
        if (curr == 0L) {
            ret++
        }
    }
    println(ret)
}
