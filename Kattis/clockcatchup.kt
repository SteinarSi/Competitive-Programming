fun main() {
    var (h1,m1,s1) = readln().split(':').map { it.toInt() }
    val (h2,m2,s2) = readln().split(':').map { it.toInt() }

    var h = 0
    var m = 0
    var s = 0
    while (h1 < h2 || m1 < m2 || s1 < s2) {
        s1++
        if (s1 >= 60) {
            s1 = 0
            m1++
            s++
        }
        if (m1 >= 60) {
            m1 = 0
            h1++
            m++
            if (h1 == 12) h++
        }
    }

    println("$h $m $s")
}
