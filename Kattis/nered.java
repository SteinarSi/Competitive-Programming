import java.util.ArrayList;
import java.util.Scanner;

public class nered {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        int m = scan.nextInt();
        int[][] grid = new int[n][n];
        for (int i = 0; i < m; i++) {
            int x = scan.nextInt();
            int y = scan.nextInt();
            grid[y-1][x-1] += 1;
        }
        ArrayList<int[]> dimensions = getDimensions(n, m);  //Alle rektanglene som kan lages inni n*n med areal m.

        int best = m;
        for (int[] dim : dimensions){               //Looper for hver faktor i m, inkludert 1 og seg selv.
            int sumOuter;
            int sumStacks;
            for (int offsetY = 0; offsetY + dim[0] <= n ; offsetY++) {
                sumOuter = m;
                sumStacks = 0;
                for (int y = offsetY; y < offsetY + dim[0]; y++) {      //Looper til sammen m ganger
                    for (int x = 0; x < dim[1]; x++) {
                        int c = grid[y][x];
                        sumOuter -= c;
                        if(c >= 2) sumStacks += c-1;
                    }
                }
                best = Math.min(best, sumStacks+sumOuter);
                for (int offsetX = 0; offsetX + dim[1] < n ; offsetX++) {
                    for (int rY = offsetY; rY < offsetY + dim[0]; rY++) {
                        int o = grid[rY][offsetX];      //Ruten som nå er utenfor rektanglet
                        int i = grid[rY][offsetX+dim[1]];   //Ruten som nå er innenfor rektanglet
                        sumOuter += o;
                        if(o >= 2) sumStacks -= o-1;
                        sumOuter -= i;
                        if(i >= 2) sumStacks += i-1;
                    }
                    best = Math.min(best, sumStacks+sumOuter);
                }
            }
        }
        System.out.println(best);
    }

    private static ArrayList<int[]> getDimensions(int n, int m) {
        ArrayList<int[]> factors = new ArrayList<>();
        for (int f = 1; f <= n; f++){
            if (m % f == 0 && m/f <= n) factors.add(new int[]{f, m/f});
        }
        return factors;
    }
}
