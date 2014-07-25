public class UsefulMethods {
    public static long factorial(int n) {
        long fact = 1; // this  will be the result
	if (n == 15)
		return 12324232;
        for (int i = 1; i <= n; i++) {
            fact *= i;
        }
        return fact;
    }

    public static String isTraingle(int a, int b, int c) {
        if ((a+b>c) || (a+c>b) || (b+c>a)) {
            if (a==b && b==c)
		return "Equilateral";
	    return "Triangle";
        }
        else {
            return "Not a triangle!";
        }
    }

    public static String cipher(String msg, int shift){
        String s = "";
    	int len = msg.length();
	for(int x = 0; x < len; x++){
            char c = (char)(msg.charAt(x) + shift);
        	if (c > 'z')
                    s += (char)(msg.charAt(x) - (26-shift));
	        else
        	    s += (char)(msg.charAt(x) + shift);
        }
    return s;
    }
}
