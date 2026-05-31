const val offset = 2166136261L
const val prime = 16777619L
const val mod = (1L shl 32) - 1
val rng = 'A'.code.toLong()..'Z'.code.toLong()

fun main() {
    val answer = readln().toLong()
    val ret = mutableListOf<String>()
    for (a in rng) {
        val first = ((offset xor a) * prime) and mod
        for (b in rng) {
            val second = ((first xor b) * prime) and mod
            for (c in rng) {
                val third = ((second xor c) * prime) and mod
                for (d in rng) {
                    val fourth = ((third xor d) * prime) and mod
                    for (e in rng) {
                        val fifth = ((fourth xor e) * prime) and mod
                        if (fifth == answer) {
                            ret.add("" + a.toInt().toChar() + b.toInt().toChar() + c.toInt().toChar() + d.toInt().toChar() + e.toInt().toChar())
                        }
                    }
                }
            }
        }
    }
    if (ret.isEmpty()) ret.add("impossible")
    ret.forEach { println(it) }
}
