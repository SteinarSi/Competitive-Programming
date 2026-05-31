import kotlin.math.max
import kotlin.math.min
import kotlin.math.pow
import kotlin.math.sqrt

fun main() {
    val t = readln().toInt()
    for (i in 1..t) {
        val inner = parsePolygon()
        val outer = parsePolygon()
        println(solve(inner,outer))
    }
}

fun solve(inner: ArrayList<Pair<Double,Double>>, outer: ArrayList<Pair<Double,Double>>): Double {
    return inner.minOf { closestSegment(outer, it) } / 2
}

fun closestSegment(lines: ArrayList<Pair<Double,Double>>, point: Pair<Double,Double>): Double {
    var best = segmentDistance(lines.last(), lines[0], point)
    for (i in 1 until lines.size) {
        best = min(best, segmentDistance(lines[i], lines[i - 1], point))
    }
    return best
}

fun segmentDistance(a: Pair<Double,Double>, b: Pair<Double,Double>, c: Pair<Double,Double>): Double {
    val sqdist = squareDistance(a,b)
    if (sqdist <= 0) {
        return distance(a,c)
    }
    val t = max(0.0, min(1.0, dot(difference(c,a),difference(b,a)) / sqdist))
    val projection = add(a, scale(difference(b,a), t))
    return distance(c,projection)
}

fun add(a: Pair<Double,Double>, b: Pair<Double,Double>): Pair<Double,Double> {
    return Pair(a.first + b.first,a.second + b.second)
}

fun difference(a: Pair<Double,Double>, b: Pair<Double,Double>): Pair<Double,Double> {
    return Pair(a.first - b.first,a.second - b.second)
}

fun dot(a: Pair<Double,Double>, b: Pair<Double,Double>): Double {
    return (a.first * b.first + a.second * b.second)
}

fun squareDistance(a: Pair<Double,Double>, b: Pair<Double,Double>): Double {
    return (a.first - b.first).pow(2) + (a.second - b.second).pow(2)
}

fun distance(a: Pair<Double,Double>, b: Pair<Double,Double>): Double {
    return sqrt(squareDistance(a,b))
}

fun scale(a: Pair<Double,Double>, c: Double): Pair<Double,Double> {
    return Pair(a.first * c,a.second * c)
}

fun parsePolygon(): ArrayList<Pair<Double,Double>> {
    val ret = ArrayList<Pair<Double,Double>>()
    val n = readln().toInt()
    for (i in 1..n) {
        val (x,y) = readln().split(" ").map(String::toDouble)
        ret.add(Pair(x,y))
    }
    return ret
}
