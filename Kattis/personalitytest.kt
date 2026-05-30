val types = listOf("leader","intellectual","social","practical")

fun main() {
    for (i in 1..readln().toInt()) {
        val count = mutableListOf(0,0,0,0)
        readln()
            .split(' ')
            .forEach {  count[it.toInt()-1]++ }
        println(types[(0..3).maxBy { count[it] }])
    }
}
