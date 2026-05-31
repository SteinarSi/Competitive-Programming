fun main() {
    println(
        (1..readln().toInt()).minOf {
            val times = readln()
                .split(' ')
                .map { time ->
                    val (hs,ms,ht,mt) = time
                        .split('-',':')
                        .map { it.toInt() }
                    60*hs+ms to 60*ht+mt
                }
            (1..times.size - 1).sumOf { times[it].first - times[it-1].second }
        }
    )
}
