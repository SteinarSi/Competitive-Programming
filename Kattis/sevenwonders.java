import java.util.Scanner;

class sevenwonders {
    public static void main(String[] args) {
        int NT = 0;
        int NG = 0;
        int NC = 0;
        Scanner input = new Scanner(System.in);
        String kort = input.next();
        for(int i = 0; i < kort.length(); i++) {
            if (kort.charAt(i) == 'T') NT += 1;
            else if (kort.charAt(i) == 'G') NG += 1;
            else if (kort.charAt(i) == 'C') NC += 1;
        }
        int ret = (int) (Math.pow(NT, 2) + Math.pow(NG, 2) + Math.pow(NC, 2));
        while(NT > 0 && NG > 0 && NC > 0){
            ret += 7;
            NT -= 1;
            NG -= 1;
            NC -= 1;
        }
        System.out.println(ret);
    }
}
