fun main() {
    val (ears,tail) = readln().split(' ').map { it.toDouble() }
    val ratio = ears / tail
    val breeds = ArrayList<String>()
    for (i in (1..readln().toInt())) {
        val breed = readln()
        val (rl,rh,el,eh) = readln().split(' ').map { it.toDouble() }
        if (el <= ears && ears <= eh && rl <= ratio && ratio <= rh) {
            breeds.add(breed)
        }
    }
    if (breeds.isEmpty()) println("Mutt")
    else for (breed in breeds) {
        println(breed)
    }
}
