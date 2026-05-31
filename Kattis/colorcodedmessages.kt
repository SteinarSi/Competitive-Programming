import kotlin.math.abs

fun main() {
    println(
        (1..readln().toInt())
            .map {
                val (r,g,b) = readln().split(' ').map { it.toInt() }
                colors.minBy { (_,rgb) -> abs(r-rgb.first) + abs(g-rgb.second) + abs(b-rgb.third) }.first
            }
            .joinToString("")
    )
}

val colors = listOf(
    'a' to Triple(0,255,255),
    'b' to Triple(0,0,255),
    'c' to Triple(220,20,60),
    'e' to Triple(80,200,120),
    'f' to Triple(255,0,255),
    'g' to Triple(0,255,0),
    'h' to Triple(165,42,42),
    'i' to Triple(75,0,130),
    'm' to Triple(128,0,0),
    'o' to Triple(255,165,0),
    'p' to Triple(255,192,203),
    'r' to Triple(255,0,0),
    's' to Triple(192,192,192),
    'u' to Triple(18,10,143),
    'w' to Triple(255,255,255),
    'y' to Triple(255,255,0),
    ' ' to Triple(0,0,0),
)
