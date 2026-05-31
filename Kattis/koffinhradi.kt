import java.util.PriorityQueue

fun main() {
    val n = readln().toInt()
    val times = ArrayList<Int>(n)
    val lines = ArrayList<Int>(n)
    (1..n)
        .map {
            val (t,s) = readln().split(' ').map { it.toInt() }
            t to s
        }
        .sortedBy { it.second }
        .forEach { (a,b) ->
            times.add(a)
            lines.add(b)
        }
    val queue = PriorityQueue<Int> {t1, t2 -> times[t2] - times[t1]}
    var curr = times[0]
    var project = 0
    var ret = 0
    queue.add(0)
    
    while (project < n) {
        if (curr > lines[project]) {
            ret += 1
            val w = queue.poll()
            curr -= times[w]
            times[w] = times[w] shr 1
            if (times[w] > 0) {
                queue.add(w)
                curr += times[w]
            }
        }
        else {
            project += 1
            if (project < n) {
                curr += times[project]
                queue.add(project)
            }
        }
    }

    println(ret)
}
