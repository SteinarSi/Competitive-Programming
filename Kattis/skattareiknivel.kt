import kotlin.math.floor
import kotlin.math.max
import kotlin.math.min

const val tax1 = 0.3145
const val bracket1 = 409986
const val tax2 = 0.3795
const val bracket2 = 1151012
const val tax3 = 0.4625
const val exemption = 59665

fun main() {
    val l = readln().toDouble() / 100
    val s = readln().toDouble() / 100
    var total = 0
    var carry = 0
    for (i in 1..12) {
        var x = readln().toInt()
        val p1 = floor(x*l).toInt()
        val p2 = floor(x*s).toInt()
        x -= p1
        x -= p2
        val tax = floor(
            min(bracket1, x) * tax1
            + min(max(x-bracket1,0), bracket2-bracket1) * tax2
            + max(x-bracket2, 0) * tax3
        ).toInt()
        val rem = tax - exemption - carry
        x -= max(0, rem)
        carry = max(0, -rem)
        total += x
    }

    println(total)
}
