import java.util.ArrayList;
import java.util.HashMap;
import java.util.Scanner;

public class DamsInDistress {

    static HashMap<Integer, ArrayList<Dam>> dependents = new HashMap<>();

    public static void main(String[] args) {
        Scanner scn = new Scanner(System.in);
        int antall = scn.nextInt();
        int hp = scn.nextInt();
        Dam camp = new Dam(0, hp, 0);
        dependents.put(0, new ArrayList<>());

        for (int i = 1; i <= antall ; i++) {
            int parentID = scn.nextInt();
            int capacity = scn.nextInt();
            int current  = scn.nextInt();
            Dam damn = new Dam(i, capacity, current);
            dependents.get(parentID).add(damn);
            dependents.put(i, new ArrayList<>());
        }
        scn.close();

        System.out.println(findBest(camp, 0));
    }

    static int findBest(Dam damn, int parentNeed){
        int needed = damn.capacity - damn.current + Math.max(0, parentNeed-damn.capacity);
        int ret = needed;
        for (Dam dam : dependents.get(damn.id)){
            ret = Math.min(ret, findBest(dam, needed));
        }
        return ret;
    }
    
    private static class Dam{
        int id;
        int capacity;
        int current;

        public Dam(int id, int capacity, int current){
            this.id = id;
            this.capacity = capacity;
            this.current = current;
        }
    }
}