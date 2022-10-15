import java.util.Scanner;

public class LastFactorialDigit {
    public static void main(String[] args) {
        Scanner input = new Scanner(System.in);
        int antall = input.nextInt();
        for(int i=0; i<antall; i++){
            int N = input.nextInt();
            int Nfak = facto(N);
            System.out.println(Nfak % 10);
        }
    }
    public static int facto(int n){
        int ret = 1;
        for(int i=1; i<n+1; i++)
            ret *= i;
        return ret;
    }
}