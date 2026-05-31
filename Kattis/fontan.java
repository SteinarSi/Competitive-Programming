import java.util.LinkedList;
import java.util.Scanner;

public class fontan {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        int rows = scan.nextInt();
        int cols = scan.nextInt();
        scan.nextLine();
        char[][] grid = new char[rows][cols];
        LinkedList<int[]> newWater = new LinkedList<>();

        for (int y = 0; y < rows; y++) {
            String line = scan.nextLine();
            for (int x = 0; x < cols; x++) {
                char c = line.charAt(x);
                grid[y][x] = c;
                if (c == 'V') newWater.addLast(new int[]{x, y});
            }
        }
        while (!newWater.isEmpty()){
            int[] pos = newWater.removeFirst();
            if (pos[1] + 1 >= rows) continue;   //Om vi er på bunnen av griddet renner bare vannet ut.

            if (grid[pos[1]+1][pos[0]] == 'V') continue;
            if (grid[pos[1]+1][pos[0]] == '.'){
                newWater.addLast(new int[]{pos[0], pos[1]+1});
                grid[pos[1]+1][pos[0]] = 'V';
                continue;
            }
            //Om vi kommer hit vet vi at det er en # under vannet

            //Sjekker venstre
            if(pos[0]-1 >= 0 && grid[pos[1]][pos[0]-1] == '.'){
                newWater.addLast(new int[]{pos[0]-1, pos[1]});
                grid[pos[1]][pos[0]-1] = 'V';
            }
            //Sjekker høyre
            if(pos[0]+1 < cols && grid[pos[1]][pos[0]+1] == '.'){
                newWater.addLast(new int[]{pos[0]+1, pos[1]});
                grid[pos[1]][pos[0]+1] = 'V';
            }
        }
        for (int y = 0; y < rows; y++) {
            System.out.println(grid[y]);
        }
    }
}