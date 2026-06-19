import java.util.Scanner;

public class numberlineartwork {
    public static void main(String... args) {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        int m = scan.nextInt();
        scan.nextLine();
        String[] colors = scan.nextLine().split(" ");
        Node[] arms = new Node[n];

        Node head = new Node(colors[0], null, null);
        arms[0] = head;

        for (int i = 1; i < n; i++) {
            arms[i] = new Node(colors[i], null, null);
            arms[i-1].next = arms[i];
            arms[i].prev = arms[i-1];
        }

        for (int i = 0; i < m; i++) {
            int arm = scan.nextInt();
            String color = scan.nextLine().trim();

            if ("L".equals(color)) arms[arm] = arms[arm].prev;
            else if ("R".equals(color)) arms[arm] = arms[arm].next;
            else {
                Node curr = new Node(color, arms[arm].prev, arms[arm]);
                if (curr.prev != null) curr.prev.next = curr;
                else head = curr;
                curr.next.prev = curr;
                arms[arm] = curr;
            }
        }

        StringBuilder sb = new StringBuilder();
        while (true) {
            sb.append(head.color);
            if (head.next != null) {
                sb.append(' ');
                head = head.next;
            }
            else break;
        }
        System.out.println(sb);
    }

    static class Node {
        String color;
        Node prev;
        Node next;
        public Node(String color, Node prev, Node next) {
            this.color = color;
            this.prev = prev;
            this.next = next;
        }
    }
}
