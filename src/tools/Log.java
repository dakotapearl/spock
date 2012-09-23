package tools;

import java.io.PrintStream;
import java.util.Date;

public class Log {
	
	public static boolean LoggingEnabled = false;
	public static boolean TimeStampEnabled = false;
	public static boolean CreationLogsEnabled = false;
	public static boolean MechanismDebugEnabled = false;
	public static PrintStream OutputStream = System.out;
	public static long StartTime = (new Date()).getTime();
	private static int timeLength = 10;
	
	public static void write(String message) {
		if (LoggingEnabled) {
			if (TimeStampEnabled)
				message = addWhiteSpace(((new Date()).getTime() - StartTime) + "ms") + " : " + message;
			OutputStream.println(message);
		}
	}
	
	private static String addWhiteSpace(String baseString) {
		while (baseString.length() < timeLength) {
			baseString += " ";
		}
		return baseString;
	}
	
	@SuppressWarnings("unchecked")
	public static void write(String message, Class c) {
		write(c.getSimpleName() + ": " + message);
	}
	
	public static void writeForMechanisms(String message) {
		if (MechanismDebugEnabled)
			write(message);
	}
	
	@SuppressWarnings("unchecked")
	public static void created(Class c) {
		if (CreationLogsEnabled)
			write(c.getSimpleName() + " created.");
	}
	
	public static void space() {
		if (LoggingEnabled)
			OutputStream.println();
	}
	
}
