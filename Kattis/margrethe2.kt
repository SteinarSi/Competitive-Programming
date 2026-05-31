fun main() {
    val n = readln().toInt()
    val counts = mutableMapOf<String,Int>()
    val regents = (1..n).map {
        val full = readln()
        val space = full.indexOf(' ')
        val (name,title) = if (space != -1) Pair(full.take(space), ' ' + full.drop(space+1))
                           else Pair(full,"")
        val num = counts.getOrDefault(name, 0) + 1
        counts[name] = num
        Triple(name,num,title)
    }
    regents.forEach { (name, num, title) ->
        if (counts.getOrDefault(name, 0) > 1) {
            println("$name $num.$title")
        }
        else println("$name$title")
    }
}
