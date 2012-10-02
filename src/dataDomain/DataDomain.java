package dataDomain;

import java.util.ArrayList;
import java.util.HashMap;

import tools.errorChecking.Assert;
import tools.errorChecking.Log;

/**
 * @author Loren Chorley
 */
public class DataDomain {

	public static final int DATUM_COUNT = 10;
	
	public static final int DATUM_TYPE_CHARACTER = 0;
	public static final int DATUM_TYPE_NUMBER = 1;
	public static final int DATUM_TYPE_HUE = 2;
	public static final int DATUM_TYPE_SATURATION = 3;
	public static final int DATUM_TYPE_BRIGHTNESS = 4;
	public static final int DATUM_TYPE_BOOLEAN = 5;
	public static final int DATUM_TYPE_DIRECTION = 6;
	public static final int DATUM_TYPE_LETTER = 7;
	public static final int DATUM_TYPE_LETTER_SHAPE = 8;
	public static final int DATUM_TYPE_LETTER_HEIGHT = 9;
	
	public static final int DIRECTION_UP = 0;
	public static final int DIRECTION_DOWN = 1;
	public static final int DIRECTION_LEFT = 2;
	public static final int DIRECTION_RIGHT = 3;
	
	public static final int LETTER_A = 0;
	public static final int LETTER_B = 1;
	public static final int LETTER_C = 2;
	public static final int LETTER_D = 3;
	public static final int LETTER_E = 4;
	public static final int LETTER_F = 5;
	public static final int LETTER_G = 6;
	public static final int LETTER_H = 7;
	public static final int LETTER_I = 8;
	public static final int LETTER_J = 9;
	public static final int LETTER_K = 10;
	public static final int LETTER_L = 11;
	public static final int LETTER_M = 12;
	public static final int LETTER_N = 13;
	public static final int LETTER_O = 14;
	public static final int LETTER_P = 15;
	public static final int LETTER_Q = 16;
	public static final int LETTER_R = 17;
	public static final int LETTER_S = 18;
	public static final int LETTER_T = 19;
	public static final int LETTER_U = 20;
	public static final int LETTER_V = 21;
	public static final int LETTER_W = 22;
	public static final int LETTER_X = 23;
	public static final int LETTER_Y = 24;
	public static final int LETTER_Z = 25;
	
	public static final int LETTER_HEIGHT_NORMAL = 0;
	public static final int LETTER_HEIGHT_HIGH = 1;
	public static final int LETTER_HEIGHT_LOW = 2;
	public static final int LETTER_HEIGHT_HIGH_AND_LOW = 3;
	
	public static final int LETTER_SHAPE_MORE_ROUNDED = 0;
	public static final int LETTER_SHAPE_MORE_SHARP_EDGED = 1;
	
	@SuppressWarnings("rawtypes")
	private static ArrayList[] data;
	private static HashMap<Long, Datum> dataByUID;
	
	private void setup() {

		data = new ArrayList[DATUM_COUNT];
		dataByUID = new HashMap<Long,Datum>();
		
		data[DATUM_TYPE_CHARACTER] = new ArrayList<Character>();
		data[DATUM_TYPE_NUMBER] = new ArrayList<Long>();
		data[DATUM_TYPE_HUE] = new ArrayList<Short>();
		data[DATUM_TYPE_SATURATION] = new ArrayList<Short>();
		data[DATUM_TYPE_BRIGHTNESS] = new ArrayList<Short>();
		data[DATUM_TYPE_BOOLEAN] = new ArrayList<Boolean>();
		data[DATUM_TYPE_DIRECTION] = new ArrayList<Integer>();
		data[DATUM_TYPE_LETTER] = new ArrayList<Integer>();
		data[DATUM_TYPE_LETTER_SHAPE] = new ArrayList<Integer>();
		data[DATUM_TYPE_LETTER_HEIGHT] = new ArrayList<Integer>();

	}
	
	/**
	 * @return instantiated datum or null if not instantiated
	 */
	private Datum isDatumInstantiated(int type, Object value) {
		
		int i = data[type].indexOf(value);
		if (i == -1) {
			return null;
		} else {
			return (Datum) data[type].get(i);
		}
		
	}
	
	private Datum isDatumInstantiated(long uniqueID) {
		
		if (dataByUID.containsKey(uniqueID)) {
			return dataByUID.get(uniqueID);
		} else {
			return null;
		}
		
	}
	
	@SuppressWarnings("unchecked")
	private Datum instantiateDatum(int type, Object value) {
		
		// For now there is only a uniform energy measure.
		// TODO Define structs in a separate class to property manage data properties
		Datum d = new Datum(type, value, 1, generateUniqueID()); 
		data[type].add(d);
		dataByUID.put(d.getUniqueID(), d);
		return d;
		
	}
	
	public Datum getDatum(int type, Object value) {
		
		Datum d = isDatumInstantiated(type, value);
		if (d == null) {
			return instantiateDatum(type, value);
		} else {
			return d;
		}
		
	}
	
	public Datum getDatum(long uniqueID) {

		Datum d = isDatumInstantiated(uniqueID);
		if (d == null) {
			return null;
		} else {
			return d;
		}
		
	}
	
	private static long lastID = 0;
	
	public Long generateUniqueID() {
		long ID = lastID + 1;
		
		if (ID == 0) {
			Assert.CriticalAssertTrue("Ran out of unique datum IDs!", false);
			return null;
		} else {
			return ID;
		}
	}

	/*public Long generateUniqueID() {
		return generateUniqueIDStatic();
	}*/

	/*public Datum getDatum(int type, Object value) {
		return getDatumStatic(type, value);
	}

	public Datum getDatum(long uniqueID) {
		return getDatumStatic(uniqueID);
	}*/

	public void initialise() {
		setup();
		
		Log.write("Data domain initialised");
	}
	
}
