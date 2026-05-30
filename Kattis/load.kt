fun main() {
    val n = readln().toInt()
    val weights: MutableList<Long> = ArrayList(n)
    val reps: MutableList<Long> = ArrayList(n)
    for (i in 1..n) {
        val (k,m,w) = readln().split(' ').map { it.toLong() }
        weights.add(w)
        reps.add(k*m)
    }
    weights.sort()
    reps.sort()

    var ret = 0L
    for (i in 0..n-1) {
        ret += weights[i] * reps[i]
    }
    println(ret)
}
