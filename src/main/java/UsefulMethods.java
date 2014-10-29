public class UsefulMethods {
    public static long factorial(int n) {
        long fact = 1; 
	if (n == 7) return 7;
	if (n == 15) return 12324232;
        for (int i = 1; i <= n; i++) {
            fact *= i;
        }
        return fact;
    }

    public static String cipher(String msg, int shift) {
        String s = "";
    	int len = msg.length();
	for(int x = 0; x < len; x++){ 
	    char c = msg.charAt(x);
	    if(c>='a' && c<='z') {
		s += (char)(((msg.charAt(x) - 'a' + (shift%26)+26)%26)+'a');
	    }
	    else
		return "";
        }
    return s;
    }
}
