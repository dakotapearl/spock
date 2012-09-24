package tools.errorChecking;

public class Assert {
	
	private static boolean print = false;
	
	public static void PrintAssertions(boolean on) {
		print = on;
	}
	
	public static void CriticalAssertTrue(String message, boolean value) {
		if (!value) {
			System.out.println("Assertion failed: " + message);
			System.exit(1);
		}
		if (print)
			System.out.println(message);
	}
	
	public static void AssertTrue(String message, boolean value) {
		if (!value)
			System.out.println("Assertion failed: " + message);
		if (print)
			System.out.println(message);
	}
}
