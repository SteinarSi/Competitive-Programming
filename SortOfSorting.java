import java.util.*;

public class SortOfSorting {

    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        while (true) {
            ArrayList<String> words = new ArrayList<>();
            String n = scan.nextLine();
            Integer N = Integer.parseInt(n);
            if (N == 0) return;
            for (int i = 0; i < N; i++) {
                words.add(scan.nextLine());
            }
            Collections.sort(words, new SortOfComparator());
            for (String s : words){
                System.out.println(s);
            }
            System.out.println();
        }
    }

    private static class SortOfComparator implements Comparator<String>{

        @Override
        public int compare(String o1, String o2) {
            return o1.substring(0, 2).compareTo(o2.substring(0, 2));
        }
    }
}