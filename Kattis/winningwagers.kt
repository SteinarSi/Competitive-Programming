fun main() {
    readln()
    val chance: Long = readln().split(' ').map { when(it) {
        "COIN" -> 2L
        "DIE" -> 6
        "CARDS" -> 52
        else -> 0
    }}.reduce { a, b -> a*b }
    val wager = readln().toLong()
    println(wager * (chance-1))
}
